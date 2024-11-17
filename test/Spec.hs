import Data.Either
import qualified Data.Text as Text
import System.Exit (exitFailure, exitSuccess)
import Test

isGood r = case r of
    Left e -> do
        putStrLn $ Text.unpack e
        exitSuccess
    Right x -> pure ()

main :: IO ()
main = do
    isGood parserTest
