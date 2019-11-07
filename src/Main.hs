-- {-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Either
import Control.Monad
import Data.Char
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf
import Data.Functor
import Control.Lens
import Data.Maybe
import Text.PrettyPrint.Boxes

import BaseExt
import LogicExp
import LogicExp.Valuation
import LogicExp.Eval
import Text.Table

data Flags =
    Table
    | Eval String
    | Help
    | OnlySat
    | OnlyUnsat
    deriving (Show, Read, Eq, Ord)

data Configuration = C {
    showTable :: Bool,
    showHelp :: Bool,
    valuation :: Maybe Valuation,
    onlySat :: Bool,
    onlyUnsat :: Bool
} deriving (Show, Eq)

emptyConfiguration = C False False Nothing False False

options :: [OptDescr (Configuration -> IO Configuration)]
options = 
    [
        Option "h" ["help"] (NoArg setHelpFlag)
            "shows this help"
    ,   Option "t" ["table"] (NoArg setTableFlag)
            "prints a truth-table for the expression"
    ,   Option "e" ["eval"] (ReqArg setEvalFlag "VAL")
            "evaluate the expession with the given valuation"
    ,   Option []  ["only-sat"] (NoArg setOnlySatFlag)
            "the table will only contain the valuation which satisfies the expression"
    ,   Option []  ["only-unsat"] (NoArg setOnlyUnsatFlag)
            "the table will only contain the valuations which does not satisfies the expression"
    ]

setTableFlag :: Configuration -> IO Configuration
setTableFlag s = 
    if showTable s then 
        reportError "table flag was set twice"
    else
        return $ s { showTable = True }

setHelpFlag :: Configuration -> IO Configuration
setHelpFlag s =
    if showHelp s then
        reportError "help flag was set twice"
    else
        return $ s { showHelp = True }

setEvalFlag :: String ->  Configuration -> IO Configuration
setEvalFlag arg s = 
    if isJust $ valuation s then
        reportError "cannot handle multiple valuations"
    else do
        val <- case parseValuation arg of
                Left s -> reportError s
                Right v -> pure v
        return $ s { valuation = Just val }


setOnlySatFlag :: Configuration -> IO Configuration
setOnlySatFlag s = 
    if onlySat s then
        reportError "only-sat flag was set multiple times"
    else
        return $ s { onlySat = True }

setOnlyUnsatFlag :: Configuration -> IO Configuration
setOnlyUnsatFlag s =
    if onlyUnsat s then
        reportError "only-unsat flag was set multiple times"
    else
        return $ s { onlyUnsat = True }

areArgsValid args = True

parseArgs :: IO (Configuration, String)
parseArgs = do
    args <- getArgs

    -- Parse options, getting a list of option actions
    case getOpt RequireOrder options args of
        (args,exps,[]) -> do
            -- Here we thread startOptions through all supplied option actions
            opts <- foldl (>>=) (return emptyConfiguration) args 
            return (opts, concat exps)
        
        (_,_,err) -> do
            reportError $ concat err
            return $ error ""
                
reportError :: String -> IO a
reportError s = do
    hPutStrLn stderr (s ++ "\n" ++ usageInfo header options)
    exitWith (ExitFailure 1)
            
header = "Usage: logic-tool [-the] EXP\n"

reportParseError :: String -> String -> IO ()
reportParseError expStr errStr = do
    printf "Syntax error in expression \"%s\"\n" expStr
    printf "Error: %s\n" errStr
    exitWith (ExitFailure 1)

main :: IO ()
main = do
    (conf,expStr) <- parseArgs

    let mayExp = parse expStr

    case mayExp of
        Left s -> reportParseError expStr s
        Right exp -> handleCorrectExp conf exp

    exitSuccess

handleCorrectExp :: Configuration -> LogicExp -> IO ()
handleCorrectExp conf exp = 
    do
        let showHelp' = showHelp conf
        let doTable = showTable conf
        let doEval = isJust $ valuation conf

        when showHelp' $ do
            hPutStrLn stderr (usageInfo header options)
            exitSuccess

        when doTable $ handleTable conf exp
        when doEval $ handleEval conf exp
        unless (doTable || doEval) $ putStrLn $ toString exp


handleTable :: Configuration -> LogicExp -> IO ()
handleTable conf exp = do
    let literals = getLiteralList exp
    let allVals = allValuations literals

    -- preparing header row
    let valuations_header = (:[]) <$> literals
    let exp_header = [toString exp]
    let table_header = valuations_header ++ ["|"] ++ exp_header :: [String]


    -- preparing table body
    let valuations_body = allVals <&> toValueList literals <&> (<&> boolToString) :: [[String]]

    let allOutcomes = eval exp <$> allVals :: [Bool]

    let table_body_unformated = zip valuations_body allOutcomes :: [([String], Bool)]

    let table_body_unformated_filtered 
            | onlySat conf        = filter snd table_body_unformated
            | onlyUnsat conf      = filter (\(_,b) -> not b) table_body_unformated
            | otherwise           = table_body_unformated

    let table_body = (\(a,b) -> a ++ ["|"] ++ [boolToString b]) <$> table_body_unformated_filtered :: [[String]]

    let table = headerAndContentToTable table_header table_body
    putStr table

handleEval :: Configuration -> LogicExp -> IO ()
handleEval conf exp = do 
    let maybeVal = valuation conf
    case maybeVal of
        Nothing  -> reportError "error: handleEval: should evaluate but got nothing"
        Just val -> do
            let evaluation = eval exp val
            print evaluation
            exitSuccess

-- converts Bool to a string representation
boolToString :: Bool -> String
boolToString True  = "1"
boolToString False = "0"
