{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Service.HtmlChecker.Client
    ( checkHtml
    )
where

import           Control.Exception           (handle)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy.Char8  as L8
import           Network.HTTP.Client.Conduit as CC
import           Network.HTTP.Simple         as S

userAgent :: B.ByteString
userAgent = "HTML Validator CLI"

checkHtml :: String -> FilePath -> IO L8.ByteString
checkHtml validatorUrl htmlFilePath = handle httpExceptionHandler $ do
    request' <- CC.parseUrlThrow $ "POST " ++ validatorUrl ++ "?out=json"
    let request =
            S.setRequestBodyFile htmlFilePath
                $ S.setRequestHeaders
                      [("Content-Type", "text/html"), ("User-Agent", userAgent)]
                $ request'
    response <- S.httpLBS request
    return $ S.getResponseBody response

-- TODO: support verbose mode
httpExceptionHandler :: HttpException -> IO L8.ByteString
httpExceptionHandler e = do
    errorWithoutStackTrace $ case e of
        (CC.HttpExceptionRequest _ content) -> case content of
            -- TODO: show statusMessage
            (CC.StatusCodeException response _) ->
                show $ S.getResponseStatus response
            CC.ResponseTimeout            -> "Response Timeout"
            CC.ConnectionTimeout          -> "Connection Timeout"
            (CC.ConnectionFailure _     ) -> "Connection Failure"
            (CC.InternalException detail) -> show detail
            _                             -> show content
        (CC.InvalidUrlException url _) -> "Invalid url: " ++ url
