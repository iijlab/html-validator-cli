-- {-# OPTIONS_GHC -F -pgmF doctest-discover -optF config.json #-}
import           Test.DocTest

main :: IO ()
main = doctest
    [ "-isrc"
    , "src/Service/HtmlChecker/Response.hs"
    , "src/System/FilePath/Finder.hs"
    ]
