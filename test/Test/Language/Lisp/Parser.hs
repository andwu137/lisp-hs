{-# LANGUAGE OverloadedStrings #-}

module Test.Language.Lisp.Parser (
    parserTests,
    parserTest,
) where

import Control.Monad (forM_, void)
import qualified Data.Text as Text
import qualified Language.Lisp.IR.AST as AST
import Language.Lisp.Parser
import Test.Utils (assert, maybeToEither)
import qualified Text.Megaparsec as P

parserTests :: [(Text.Text, AST.Expr Int)]
parserTests =
    [
        ( "(defun meaning (life)\n\
          \  \"Return the computed meaning of LIFE\"\n\
          \  (let ((meh \"abc\"))\n\
          \    ;; Invoke krakaboom\n\
          \    (loop 'for x 'across meh\n\
          \       'collect x)))   ; store values into x, then return it\n"
        , AST.EList
            0
            [ AST.EAtom 0 $ AST.AIdent 0 "defun"
            , AST.EAtom 0 $ AST.AIdent 0 "meaning"
            , AST.EList 0 [AST.EAtom 0 $ AST.AIdent 0 "life"]
            , AST.EAtom 0 $ AST.AString 0 "Return the computed meaning of LIFE"
            , AST.EList
                0
                [ AST.EAtom 0 $ AST.AIdent 0 "let"
                , AST.EList
                    0
                    [ AST.EList
                        0
                        [ AST.EAtom 0 $ AST.AIdent 0 "meh"
                        , AST.EAtom 0 $ AST.AString 0 "abc"
                        ]
                    ]
                , AST.EList
                    0
                    [ AST.EAtom 0 $ AST.AIdent 0 "loop"
                    , AST.EQuote 0 $ AST.EAtom 0 $ AST.AIdent 0 "for"
                    , AST.EAtom 0 $ AST.AIdent 0 "x"
                    , AST.EQuote 0 $ AST.EAtom 0 $ AST.AIdent 0 "across"
                    , AST.EAtom 0 $ AST.AIdent 0 "meh"
                    , AST.EQuote 0 $ AST.EAtom 0 $ AST.AIdent 0 "collect"
                    , AST.EAtom 0 $ AST.AIdent 0 "x"
                    ]
                ]
            ]
        )
    ]

parserTest :: Either Text.Text ()
parserTest = do
    forM_ parserTests $ \(t, r) -> do
        case P.parse expr "filename" t of
            Left e -> Left . Text.pack $ P.errorBundlePretty e
            Right x ->
                maybeToEither
                    ( Text.concat
                        [ "example was not equal:\n"
                        , "parse:\n" <> AST.showExprPretty x
                        , "\n"
                        , "example:\n" <> AST.showExprPretty r
                        ]
                    )
                    $ assert ((const 0 <$> x) == r) ()

parserTestPrint :: IO ()
parserTestPrint = do
    forM_ (fst <$> parserTests) $ \t -> do
        putStrLn ">>> Input:"
        putStrLn $ Text.unpack t
        putStrLn "<<< Output:"
        -- parserRunParseShow (fmap (const (0 :: Int)) <$> expr) t
        parserRunParse (AST.showExprPretty <$> expr) t
        putStrLn "--- End Test\n\n"
        void getLine

parserRunParseShow :: (Show a) => Parser a -> Text.Text -> IO ()
parserRunParseShow p s = case P.parse p "filename" s of
    Left e -> putStrLn $ P.errorBundlePretty e
    Right x -> print x

parserRunParse :: Parser Text.Text -> Text.Text -> IO ()
parserRunParse p s = case P.parse p "filename" s of
    Left e -> putStrLn $ P.errorBundlePretty e
    Right x -> putStrLn $ Text.unpack x
