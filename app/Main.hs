module Main where

import           Control.Applicative   ((<|>))
import           Data.Maybe            (fromMaybe)
import           Lib                   (Format (Classic, OneLine, Raw),
                                        Mode (DryRun, FormatterTest, Standard),
                                        run)
import           System.Console.GetOpt (ArgDescr (NoArg, ReqArg),
                                        ArgOrder (Permute), OptDescr (Option),
                                        getOpt, usageInfo)
import           System.Environment    (getArgs, getProgName, lookupEnv)
import           System.Exit           (ExitCode (ExitFailure, ExitSuccess),
                                        exitWith)
import           System.IO             (hPutStrLn, stderr)


validatorUrlEnv :: String
validatorUrlEnv = "HTML_VALIDATOR_URL"

defaultValidatorUrl :: String
defaultValidatorUrl = "https://validator.w3.org/nu/"

main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute optionDescriptions args of
        (optionArgs, paths, []) | Help `elem` optionArgs -> help
                                | paths == [] -> missingOperand
                                | otherwise -> main' optionArgs paths
        (_, _, errorMessages) -> invalidArguments errorMessages
  where
    help = do
        prog <- getProgName
        hPutStrLn stderr $ usageInfo (usageHeader prog) optionDescriptions
        exitWith $ ExitSuccess

    missingOperand = do
        prog <- getProgName
        hPutStrLn stderr $ prog ++ ": missing operand"
        hPutStrLn stderr ""
        help

    invalidArguments errorMessages = do
        mapM_ (hPutStrLn stderr) errorMessages
        exitWith $ ExitFailure 1

    main' optionArgs paths = do
        urlFromEnv <- lookupEnv validatorUrlEnv
        let urlFromArg = getValidatorUrl optionArgs
        let validatorUrl =
                fromMaybe defaultValidatorUrl $ urlFromArg <|> urlFromEnv

        result <- run (getMode optionArgs)
                      (getFormat optionArgs)
                      (Verbose `elem` optionArgs)
                      validatorUrl
                      (getExcludedDirs optionArgs)
                      paths

        exitWith $ if result == 0 then ExitSuccess else ExitFailure result

usageHeader :: String -> String
usageHeader prog = "Usage: " ++ prog ++ " [-u URL] [-x DIR] FILE | DIR ..."

data Argument = ValidatorUrl String
              | ExcludedDir String
              | OneLineFormat
              | RawFormat
              | DryRunMode
              | FormatterTestMode
              | Verbose
              | Help
              deriving Eq

optionDescriptions :: [OptDescr Argument]
optionDescriptions =
    [ Option
        ['s']
        ["validator-url"]
        (ReqArg ValidatorUrl "URL")
        ("validation service url (default: " ++ defaultValidatorUrl ++ ")")
    , Option ['x'] ["exclude"] (ReqArg ExcludedDir "DIR") "exclude files in DIR"
    , Option ['1']
             ["oneline"]
             (NoArg OneLineFormat)
             "print each message on one line"
    , Option ['r'] ["raw"]     (NoArg RawFormat)  "print raw response"
    , Option ['v'] ["verbose"] (NoArg Verbose)    "verbose mode"
    , Option ['n'] ["dry-run"] (NoArg DryRunMode) "dry-run mode"
    , Option ['t']
             ["formatter-test"]
             (NoArg FormatterTestMode)
             "read file as response data (for debug)"
    , Option ['h'] ["help"] (NoArg Help) "print this help message and exit"
    ]

getMode :: [Argument] -> Mode
getMode optionArgs = if DryRunMode `elem` optionArgs
    then DryRun
    else if FormatterTestMode `elem` optionArgs then FormatterTest else Standard

getFormat :: [Argument] -> Format
getFormat optionArgs = if OneLineFormat `elem` optionArgs
    then OneLine
    else if RawFormat `elem` optionArgs then Raw else Classic

getValidatorUrl :: [Argument] -> Maybe String
getValidatorUrl optionArgs = foldr getValidatorUrl' Nothing optionArgs
  where
    -- If multiple URLs are specified, use the last one.
    getValidatorUrl' (ValidatorUrl url) acc = Just url
    getValidatorUrl' _                  acc = acc

getExcludedDirs :: [Argument] -> [String]
getExcludedDirs optionArgs = foldr getExcludedDir [] optionArgs
  where
    getExcludedDir (ExcludedDir x) acc = x : acc
    getExcludedDir _               acc = acc
