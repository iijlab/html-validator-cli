{-# LANGUAGE OverloadedStrings #-}

module Service.HtmlChecker.ResponseSpec
    ( spec
    )
where

import qualified Data.ByteString.Lazy.Char8   as L8
import           Paths_html_validator_cli
import           Service.HtmlChecker.Response
import           System.FilePath.Posix        ((</>))
import           Test.Hspec


spec :: Spec
spec = do
    describe "decodeResult" $ do
        it
                "returns validation result when arg is valid schema JSON includes some messages"
            $ do
                  dir          <- getDataDir
                  responseJson <- L8.readFile $ dir </> "response1.json"
                  resultText   <- readFile $ dir </> "result1.txt"
                  (decodeResult responseJson)
                      `shouldBe` Just (read resultText :: ValidationResult)

        it
                "returns validation result when arg is valid schema JSON includes no messages"
            $ do
                  (decodeResult "{\"messages\": []}")
                      `shouldBe` Just (ValidationResult {messages = []})

        it "returns Nothing when arg is not JSON" $ do
            (decodeResult "not json") `shouldBe` Nothing

        it "returns Nothing when arg is invalid schema JSON" $ do
            (decodeResult "{\"unknown\": null") `shouldBe` Nothing

    describe "countErros" $ do
        it "returns the number of error messages in validation result" $ do
            let validationResult = ValidationResult
                    { messages = [errorMsg, warningMsg, fatalMsg, errorMsg]
                    }
            (countErrors (Just validationResult)) `shouldBe` 3

        it "returns 0 when validation result has no error messages" $ do
            let validationResult =
                    ValidationResult {messages = [warningMsg, infoMsg]}
            (countErrors (Just validationResult)) `shouldBe` 0

        it "returns 0 when validation result has no messages" $ do
            let validationResult = ValidationResult {messages = []}
            (countErrors (Just validationResult)) `shouldBe` 0

        it "returns 1 when arg is Nothing" $ do
            (countErrors Nothing) `shouldBe` 1

    describe "isError" $ do
        it "returns True when arg is an error message" $ do
            (isError errorMsg) `shouldBe` True

        it "returns False when arg is not an error message" $ do
            (isError warningMsg) `shouldBe` False

        it "returns True when arg is a message which has no type" $ do
            let msg = warningMsg { type_ = Nothing }
            (isError msg) `shouldBe` True

    describe "showMessageInClassic" $ do
        it "returns message as classic format string" $ do
            let expectedValue = unlines
                    [ "[ERROR] hello"
                    , "From line 1, column 11; to line 1, column 15"
                    , "0123456789error56789"
                    , "          ^^^^^"
                    ]
            (showMessageInClassic errorMsg) `shouldBe` expectedValue

    describe "showMessageInOneLine" $ do
        it "returns message as oneline format string" $ do
            let
                expectedValue =
                    unwords
                        [ "[ERROR] hello"
                        , "From line 1, column 11; to line 1, column 15"
                        ]
            (showMessageInOneLine errorMsg) `shouldBe` expectedValue

    describe "getType" $ do
        it "returns [ERROR] when message's type is `error` and no subType" $ do
            (getType errorMsg) `shouldBe` Just "[ERROR]"

        it
                "returns [FATAL] when message's type is `error` and subType is `fatal`"
            $ do
                  (getType fatalMsg) `shouldBe` Just "[FATAL]"

        it "returns [INFO] when message's type is `info` and no subType" $ do
            (getType infoMsg) `shouldBe` Just "[INFO]"

        it
                "returns [WARNING] when message's type is `info` and subType is `warning`"
            $ do
                  (getType warningMsg) `shouldBe` Just "[WARNING]"


exampleMsg = Message
    { type_        = Nothing
    , subType      = Nothing
    , message      = Just "hello"
    , extract      = Just "0123456789error56789"
    , firstLine    = Nothing
    , firstColumn  = Just 11
    , lastLine     = Just 1
    , lastColumn   = Just 15
    , hiliteStart  = Just 10
    , hiliteLength = Just 5
    }

errorMsg = exampleMsg { type_ = Just "error" }

fatalMsg = errorMsg { subType = Just "fatal" }

infoMsg = exampleMsg { type_ = Just "info" }

warningMsg = infoMsg { subType = Just "warning" }
