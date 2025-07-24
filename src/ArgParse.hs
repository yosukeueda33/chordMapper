module ArgParse (genPath, checkFilePath) where

import System.Exit ( exitFailure, exitSuccess )
import System.Environment ( getArgs, getExecutablePath )
import System.FilePath ((</>), takeDirectory)
import System.Directory (doesFileExist, makeAbsolute)
import Data.Functor ( (<&>) )
import Control.Monad ( unless )


genPath :: IO (FilePath, FilePath) 
genPath = (,) <$> (getArgs >>= parse) <*> getFontPath
  where
    parse :: [String] -> IO FilePath
    parse ["-h"] = usage >> exit
    parse ["-v"] = version >> exit
    parse [] =  projDir <&> (</> "config" </> "default.dhall")
    parse [x] = makeAbsolute x
    parse _ = usage >> exit
    version = putStrLn "chordMapper version 0.0"
    usage = putStrLn "Usage: chordMapper [-v|-h|CONFIG_PATH]"
    exit = exitSuccess
    projDir = (</> "..") . takeDirectory <$> getExecutablePath 

    getFontPath :: IO FilePath
    getFontPath =  projDir <&> (</> "assets" </> "fonts" </> "Roboto-Regular.ttf")

checkFilePath :: String -> FilePath -> IO ()
checkFilePath typ path = do
  putStrLn $ "Specified " ++ typ ++ " file path: " ++ path
  exist <- doesFileExist path
  unless exist $ putStrLn "  is not found..." >> exitFailure
