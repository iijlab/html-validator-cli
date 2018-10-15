module Lib
    ( Mode(Standard, DryRun, FormatterTest)
    , Format(Classic, OneLine, Raw)
    , run
    )
where

import           Codec.Binary.UTF8.String     (decodeString)
import           Control.Monad                (filterM, forM, forM_, when)
import           Data.Aeson                   (decode)
import qualified Data.ByteString.Lazy.Char8   as L8
import           Service.HtmlChecker.Client   (checkHtml)
import           Service.HtmlChecker.Response (ValidationResult, countErrors,
                                               decodeResult, getMessages,
                                               showMessage)
import           System.Console.ANSI          (hSupportsANSI)
import           System.Directory             (doesDirectoryExist,
                                               doesFileExist, doesPathExist)
import           System.FilePath.Finder       (findFiles)
import           System.IO                    (hPutStrLn, stderr, stdout)


data Mode = Standard | DryRun | FormatterTest

data Format = Classic | OneLine | Raw deriving (Eq)

run :: Mode -> Format -> Bool -> String -> [String] -> [FilePath] -> IO Int
run mode format isVerbose validatorUrl excludedDirs paths = do
    debugLog isVerbose "validator" validatorUrl

    files <- findHtmlFiles excludedDirs paths
    when (null files) (errorWithoutStackTrace $ "HTML files not found")

    case mode of
        DryRun -> do
            -- Current dry-run mode just output the found files.
            mapM_ putStrLn files
            return 0
        _ -> do
            errorCounts <- forM files $ \file -> do
                debugLog isVerbose "file" file

                jsonResponse <- case mode of
                    FormatterTest -> L8.readFile file
                    _             -> checkHtml validatorUrl file
                debugLog isVerbose "response" $ byteStringToString jsonResponse

                case format of
                    Raw -> do
                        putStrLn $ byteStringToString jsonResponse
                        return 0
                    _ -> do
                        -- When specified just a file, omit the file name
                        let prefix =
                                if paths == [file] then "" else file ++ ": "
                        let validationResult = decodeResult jsonResponse
                        printResult format prefix validationResult
                        return $ countErrors validationResult

            return $ sum errorCounts

printResult :: Format -> String -> Maybe ValidationResult -> IO ()
printResult format prefix (Just result) = do
    let isOneLine = format == OneLine
    isColored <- hSupportsANSI stdout
    forM_ (getMessages result)
        $ \msg -> putStrLn $ prefix ++ showMessage isOneLine isColored msg
printResult _ prefix Nothing = putStrLn $ prefix ++ "Couldn't parse response."

byteStringToString :: L8.ByteString -> String
byteStringToString = decodeString . L8.unpack

findHtmlFiles :: [String] -> [FilePath] -> IO [FilePath]
findHtmlFiles excludedDirs paths = do
    -- Abort when the spcified files not found.
    forM_ paths $ \path -> do
        found <- doesPathExist path
        when
            (not found)
            (errorWithoutStackTrace $ path ++ ": No such file or directory")

    -- Never check the file extension here.
    files       <- filterM doesFileExist paths

    dirs        <- filterM doesDirectoryExist paths
    filesInDirs <- mapM (findFiles [".html", ".htm"] excludedDirs) dirs

    return $ concat (files : filesInDirs)

debugLog :: Bool -> String -> String -> IO ()
debugLog isDebug name value = if isDebug
    then hPutStrLn stderr $ "* " ++ name ++ ": " ++ value
    else return ()
