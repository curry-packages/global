------------------------------------------------------------------------------
--- Testing to store handles im temporary globals.
---
--- @author Michael Hanus
------------------------------------------------------------------------------

import Data.Global
import System.IO
import System.Process ( system )
import Test.Prop

------------------------------------------------------------------------------
-- A temporary global entity holding a file handle:
myHandle :: GlobalT Handle
myHandle = globalTemporary (error "no handle")

setOutHandle :: IO ()
setOutHandle = do
  h <- openFile "XXXOUT" WriteMode
  writeGlobalT myHandle h

writeMyFile :: IO ()
writeMyFile = do
  h <- readGlobalT myHandle
  hPutStrLn h "ABC"

closeMyFile :: IO ()
closeMyFile = do
  h <- readGlobalT myHandle
  hClose h

setInHandle :: IO ()
setInHandle = do
  h <- openFile "XXXOUT" ReadMode
  writeGlobalT myHandle h

readMyFile :: IO String
readMyFile = do
  h <- readGlobalT myHandle
  hGetContents h

testGlobalHandles :: PropIO
testGlobalHandles =
 (setOutHandle >> writeMyFile >> closeMyFile >> setInHandle >> readMyFile)
   `returns` "ABC\n"

-- finalize: clean
testCleanUp :: PropIO
testCleanUp = (system "rm -f XXXOUT") `returns` 0

------------------------------------------------------------------------------
