module ArgParse (genPath, checkFilePath) where

import System.Exit ( exitFailure, exitSuccess )
import System.Environment ( getArgs, getExecutablePath )
import System.FilePath ((</>), takeDirectory)
import System.Directory (doesFileExist, makeAbsolute)
import Data.Functor ( (<&>) )
import Control.Monad ( unless )


genPath :: IO FilePath 
genPath = getArgs >>= parse
  where
    parse :: [String] -> IO FilePath
    parse ["-h"] = usage >> exit
    parse ["-v"] = version >> exit
    parse [] = usage >> exit
    parse [x] = makeAbsolute x
    parse _ = usage >> exit
    version = putStrLn "chordMapper version 0.0"
    usage = putStrLn "Usage: chordMapper [-v|-h|CONFIG_PATH]"
    exit = exitSuccess

checkFilePath :: String -> FilePath -> IO ()
checkFilePath typ path = do
  putStrLn $ "Specified " ++ typ ++ " file path: " ++ path
  exist <- doesFileExist path
  unless exist $ putStrLn "  is not found..." >> exitFailure
