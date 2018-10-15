{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Service.HtmlChecker.Response where

import           Control.Applicative        ((<|>))
import           Data.Aeson                 (decode, encode)
import           Data.Aeson.TH              (defaultOptions, deriveJSON,
                                             fieldLabelModifier)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char                  (toUpper)
import           Data.List                  (intercalate)
import           Data.Maybe                 (catMaybes)
import qualified Data.Text                  as T


-- https://github.com/validator/validator/wiki/Output-%C2%BB-JSON

data Message = Message
    { type_        :: Maybe String
    , subType      :: Maybe String
    , message      :: Maybe String
    , extract      :: Maybe String
    -- These optional keys are not used.
    -- , offset       :: Maybe Int
    -- , url          :: Maybe String
    , firstLine    :: Maybe Int
    , firstColumn  :: Maybe Int
    , lastLine     :: Maybe Int
    , lastColumn   :: Maybe Int
    , hiliteStart  :: Maybe Int
    , hiliteLength :: Maybe Int
    } deriving (Eq, Read, Show)

deriveJSON (defaultOptions
    { fieldLabelModifier = \s -> case s of
        "type_" -> "type"
        t       -> t
    } ) ''Message

data ValidationResult = ValidationResult
    { messages :: [Message]
    -- These optional keys are not used.
    -- , source   :: Maybe Source
    -- , url      :: Maybe String
    -- , language :: Maybe String
    } deriving (Eq, Read, Show)

deriveJSON defaultOptions ''ValidationResult

decodeResult :: L8.ByteString -> Maybe ValidationResult
decodeResult resultJson = decode resultJson

getMessages :: ValidationResult -> [Message]
getMessages = messages

countErrors :: Maybe ValidationResult -> Int
countErrors (Just result) = length $ filter isError $ getMessages result
countErrors Nothing       = 1

isError :: Message -> Bool
isError msg = case (type_ msg) of
    Just t | (map toUpper t) == "ERROR" -> True
           | otherwise                  -> False
    Nothing -> True

showMessage :: Bool -> Bool -> Message -> String
showMessage isOneLine isColored = case (isOneLine, isColored) of
    (True, True) -> showMessageInOneLineWithColor
    (True, _   ) -> showMessageInOneLine
    (_   , True) -> showMessageInClassicWithColor
    _            -> showMessageInClassic

showMessageInClassic :: Message -> String
showMessageInClassic = showMessageInClassic' getType getExtract getHilite

showMessageInClassicWithColor :: Message -> String
showMessageInClassicWithColor = showMessageInClassic' getTypeWithColor
                                                      getExtractWithColor
                                                      getHiliteWithColor

showMessageInClassic'
    :: (Message -> Maybe String)
    -> (Message -> Maybe String)
    -> (Message -> Maybe String)
    -> Message
    -> String
showMessageInClassic' typeGetter extractGetter hiliteGetter msg =
    unlines $ catMaybes $ map ($ msg) [line1, line2, line3, line4]
  where
    line1 msg = (\t m -> t ++ " " ++ m) <$> typeGetter msg <*> getMessage msg
    line2 = getPoint
    line3 = extractGetter
    line4 = hiliteGetter

showMessageInOneLine :: Message -> String
showMessageInOneLine = showMessageInOneLine' getType

showMessageInOneLineWithColor :: Message -> String
showMessageInOneLineWithColor = showMessageInOneLine' getTypeWithColor

showMessageInOneLine' :: (Message -> Maybe String) -> Message -> String
showMessageInOneLine' typeGetter msg =
    unwords $ catMaybes $ map ($ msg) [typeGetter, getMessage, getPoint]

getType :: Message -> Maybe String
getType msg =
    (\t -> "[" ++ map toUpper t ++ "]") <$> (subType msg <|> type_ msg)

getTypeWithColor :: Message -> Maybe String
getTypeWithColor msg = paint <$> getType msg
  where
    paint s | s `elem` ["[ERROR]", "[FATAL]"]  = (goBold . goRed) s
            | s `elem` ["[INFO]", "[WARNING]"] = (goBold . goYellow) s
            | otherwise                        = id s

getMessage :: Message -> Maybe String
getMessage msg = (T.unpack . T.strip . T.pack) <$> message msg

-- https://github.com/validator/validator/wiki/Output-%C2%BB-JSON#the-firstline-firstcolumn-lastline-and-lastcolumn-numbers
getPoint :: Message -> Maybe String
getPoint msg =
    case (firstLine msg, firstColumn msg, lastLine msg, lastColumn msg) of
        (Just fl, Just fc, Just ll, Just lc) -> Just (showFromTo fl fc ll lc)
        (_, Just fc, Just ll, Just lc)       -> Just (showFromTo ll fc ll lc)
        (_, _, Just ll, Just lc)             -> Just (showAt ll lc)
        _                                    -> Nothing
  where
    showFromTo fl fc ll lc =
        "From " ++ showPoint fl fc ++ "; to " ++ showPoint ll lc
    showAt l c = "At " ++ showPoint l c
    showPoint l c = "line " ++ show l ++ ", column " ++ show c

-- | Get "extract" valude from message.
--
-- The return value is unescaped to display without line break
getExtract :: Message -> Maybe String
getExtract msg = unescapeSomeSpecialChar <$> extract msg

getExtractWithColor :: Message -> Maybe String
getExtractWithColor msg = do
    extract'  <- getExtract msg
    prefixLen <- hiliteStart msg
    hiliteLen <- hiliteLength msg
    let prefix = take prefixLen extract'
    let hilite = goCyan $ take hiliteLen (drop prefixLen extract')
    let suffix = drop (prefixLen + hiliteLen) extract'
    return (prefix ++ hilite ++ suffix)

getHilite :: Message -> Maybe String
getHilite msg = (++) <$> padding <*> indicator
  where
    padding   = (\n -> replicate n ' ') <$> hiliteStart msg
    indicator = (\n -> replicate n '^') <$> hiliteLength msg

getHiliteWithColor :: Message -> Maybe String
getHiliteWithColor msg = goCyan <$> getHilite msg

-- | Convert some control chars to a whitespace.
--
-- To keep alingment, replace '\?' with ' ' instead of "\\?".
--
-- >>> unescapeSomeSpecialChar "Line1\nLine2\nLine3\n"
-- "Line1 Line2 Line3 "
-- >>> unescapeSomeSpecialChar "HEADER\r\n\r\nBODY"
-- "HEADER    BODY"
-- >>> unescapeSomeSpecialChar "\a\b\t\n\v\f\r"
-- "\a\b  \v\f "
unescapeSomeSpecialChar :: String -> String
unescapeSomeSpecialChar (x : xs)
    | x `elem` ['\r', '\n', '\t'] = ' ' : unescapeSomeSpecialChar xs
    | otherwise                   = x : unescapeSomeSpecialChar xs
unescapeSomeSpecialChar "" = ""

goBold :: String -> String
goBold s = "\x1b[1m" ++ s ++ "\x1b[0m"

goRed :: String -> String
goRed s = "\x1b[31m" ++ s ++ "\x1b[0m"

goYellow :: String -> String
goYellow s = "\x1b[33m" ++ s ++ "\x1b[0m"

goBlue :: String -> String
goBlue s = "\x1b[34m" ++ s ++ "\x1b[0m"

goCyan :: String -> String
goCyan s = "\x1b[36m" ++ s ++ "\x1b[0m"
