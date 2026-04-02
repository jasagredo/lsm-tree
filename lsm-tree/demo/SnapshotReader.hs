-- | Standalone demo for the SnapshotReader multi-process access pattern.
--
-- This exercises the three core behaviours introduced by the proposed changes:
--
--   1. A SnapshotReader session can open a snapshot saved by a ReadWrite
--      session without acquiring the session-level lock.
--
--   2. While a SnapshotReader has a table open from a snapshot, the ReadWrite
--      session cannot delete that snapshot (ErrSnapshotInUse).
--
--   3. Once the reader closes the table the snapshot can be deleted normally.
--
-- Run with:
--   cabal run snapshot-reader-demo
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Exception (SomeException, evaluate, try)
import           Control.Tracer (nullTracer)
import qualified Data.Vector as V
import           System.IO.Temp (withSystemTempDirectory)

import           System.FS.API (MountPoint (..), mkFsPath)
import           System.FS.BlockIO.IO (defaultIOCtxParams, withIOHasBlockIO)

import           Database.LSMTree.Internal.Config (defaultTableConfig)
import           Database.LSMTree.Internal.Config.Override (TableConfigOverride (..))
import           Database.LSMTree.Internal.Entry (Entry (..))
import           Database.LSMTree.Internal.Paths (SnapshotName)
import           Database.LSMTree.Internal.Serialise (SerialisedKey,
                     SerialisedValue, serialiseKey, serialiseValue)
import           Database.LSMTree.Internal.Snapshot (SnapshotLabel (..))
import           Database.LSMTree.Internal.Types (Salt)
import qualified Database.LSMTree.Internal.Unsafe as U

-- ---------------------------------------------------------------------------
-- Key / value types
-- We reuse the raw serialised types for simplicity.
-- ---------------------------------------------------------------------------

type K = SerialisedKey
type V = SerialisedValue

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

snapName :: SnapshotName
snapName = "demo-snap"

snapLabel :: SnapshotLabel
snapLabel = SnapshotLabel "demo"

-- Assert that an action throws a specific exception (by show comparison).
assertThrows :: String -> IO a -> IO ()
assertThrows expected action = do
    r <- try @SomeException (evaluate =<< action)
    case r of
      Left e ->
        let msg = show e
        in if expected `elem` words msg || expected `isInfixOf` msg
             then putStrLn $ "  [OK] got expected exception: " <> msg
             else putStrLn $ "  [FAIL] wrong exception.\n    expected: " <> expected
                          <> "\n    got: " <> msg
      Right _ ->
        putStrLn $ "  [FAIL] expected exception '" <> expected <> "' but none was thrown"
  where
    isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)
    isPrefixOf []     _      = True
    isPrefixOf _      []     = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails []     = [[]]
    tails xxs@(_:xs) = xxs : tails xs

assertEq :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEq label expected actual
    | expected == actual = putStrLn $ "  [OK] " <> label
    | otherwise = putStrLn $ "  [FAIL] " <> label
                          <> "\n    expected: " <> show expected
                          <> "\n    actual:   " <> show actual

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = withSystemTempDirectory "snapshot-reader-demo" $ \dir ->
    withIOHasBlockIO (MountPoint dir) defaultIOCtxParams $ \hfs hbio -> do

    let sessionDir = mkFsPath []   -- root of the MountPoint

    -- -----------------------------------------------------------------------
    putStrLn "=== Setup: writer session, insert data, save snapshot ==="

    writerSess <- U.openSession nullTracer hfs hbio salt sessionDir

    writerTable <- U.new writerSess defaultConf

    let entries = V.fromList
          [ (serialiseKey (1 :: Int), Insert (serialiseValue (10 :: Int)))
          , (serialiseKey (2 :: Int), Insert (serialiseValue (20 :: Int)))
          , (serialiseKey (3 :: Int), Insert (serialiseValue (30 :: Int)))
          ]
    U.updates resolve entries writerTable

    U.saveSnapshot snapName snapLabel writerTable
    U.close writerTable
    putStrLn "  Writer saved snapshot."

    -- -----------------------------------------------------------------------
    putStrLn "\n=== Test 1: SnapshotReader can open snapshot and look up data ==="

    -- openReaderSession acquires NO session-level lock, so it coexists with
    -- the open writerSess above.
    readerSess <- U.openReaderSession nullTracer hfs hbio sessionDir

    readerTable <- U.openTableFromSnapshot defaultConfOverride
                     readerSess snapName snapLabel resolve

    results <- U.lookups resolve
                 (V.fromList [serialiseKey (1::Int), serialiseKey (2::Int), serialiseKey (3::Int)])
                 readerTable
    let values = fmap extractValue results
    assertEq "lookup results"
      (V.fromList [ Just (serialiseValue (10::Int))
                  , Just (serialiseValue (20::Int))
                  , Just (serialiseValue (30::Int)) ])
      values

    -- -----------------------------------------------------------------------
    putStrLn "\n=== Test 2: deleteSnapshot fails while reader holds the table ==="

    -- The shared lock on snapshots/demo-snap/lock is held by readerTable.
    -- deleteSnapshot tries to acquire ExclusiveLock and gets Nothing -> ErrSnapshotInUse.
    assertThrows "ErrSnapshotInUse" $
      U.deleteSnapshot writerSess snapName

    -- -----------------------------------------------------------------------
    putStrLn "\n=== Test 3: after closing reader table, deletion succeeds ==="

    U.close readerTable
    -- Shared lock released; writer can now get ExclusiveLock.
    U.deleteSnapshot writerSess snapName
    gone <- not <$> U.doesSnapshotExist writerSess snapName
    assertEq "snapshot deleted" True gone

    -- -----------------------------------------------------------------------
    putStrLn "\n=== Test 4: new snapshots created while reader is open ==="

    writerTable2 <- U.new writerSess defaultConf
    U.updates resolve
      (V.fromList [(serialiseKey (99::Int), Insert (serialiseValue (99::Int)))])
      writerTable2
    U.saveSnapshot "snap2" snapLabel writerTable2
    U.close writerTable2

    readerSess2 <- U.openReaderSession nullTracer hfs hbio sessionDir
    readerTable2 <- U.openTableFromSnapshot defaultConfOverride
                      readerSess2 "snap2" snapLabel resolve

    -- Writer saves another snapshot while reader2 is open — no conflict.
    writerTable3 <- U.new writerSess defaultConf
    U.saveSnapshot "snap3" snapLabel writerTable3
    U.close writerTable3
    putStrLn "  Writer created snap3 while reader holds snap2 open: [OK]"

    -- snap2 cannot be deleted while reader2 holds it.
    assertThrows "ErrSnapshotInUse" $
      U.deleteSnapshot writerSess "snap2"

    U.close readerTable2
    U.closeSession readerSess2

    -- Now both can be deleted.
    U.deleteSnapshot writerSess "snap2"
    U.deleteSnapshot writerSess "snap3"
    putStrLn "  Both snapshots deleted after reader closed: [OK]"

    -- -----------------------------------------------------------------------
    U.closeSession readerSess
    U.closeSession writerSess
    putStrLn "\n=== Done ==="
  where
    salt = 42 :: Salt

    defaultConf = defaultTableConfig

    defaultConfOverride = TableConfigOverride Nothing Nothing

    resolve _ new = new   -- simple last-write-wins resolver

    extractValue :: Maybe (Entry SerialisedValue b) -> Maybe SerialisedValue
    extractValue (Just (Insert v))           = Just v
    extractValue (Just (InsertWithBlob v _)) = Just v
    extractValue _                           = Nothing
