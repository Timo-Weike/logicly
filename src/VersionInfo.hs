module VersionInfo 
    (
        version
        , printVersionInfo
    )
where

import System.Exit

version :: String
version = "0.1.0.0-alpha"

printVersionInfo :: IO a
printVersionInfo = do
    putStrLn $ "logicly " ++ version
    exitSuccess