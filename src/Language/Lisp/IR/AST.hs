module Language.Lisp.IR.AST (
    Atom (..),
    Expr (..),
    showAtom,
    showExpr,
    showExprPretty,
) where

import qualified Data.Text as Text

data Atom a
    = AInt a Int
    | AFloat a Float
    | AChar a Char
    | AString a Text.Text
    | AIdent a Text.Text
    deriving (Show, Eq, Ord, Functor, Foldable)

showAtom :: Atom a -> Text.Text
showAtom = \case
    AInt _ i -> Text.pack $ show i
    AFloat _ f -> Text.pack $ show f
    AChar _ c -> Text.pack $ show c
    AString _ s -> Text.pack $ show s
    AIdent _ i -> i

data Expr a
    = EAtom a (Atom a)
    | ETuple a (Expr a) (Expr a)
    | EList a [Expr a]
    | EQuote a (Expr a)
    | EFuncQuote a (Expr a)
    deriving (Show, Eq, Ord, Functor, Foldable)

showExpr :: Expr a -> Text.Text
showExpr = \case
    EAtom _ a -> showAtom a
    ETuple _ e1 e2 -> Text.concat ["(", showExpr e1, ".", showExpr e2, ")"]
    EList _ e -> Text.concat ["(", Text.intercalate " " $ showExpr <$> e, ")"]
    EQuote _ e -> Text.concat ["'(", showExpr e, ")"]
    EFuncQuote _ e -> Text.concat ["#'", showExpr e, ""]

showExprPretty :: Expr a -> Text.Text
showExprPretty =
    go 1
  where
    spacing n = Text.replicate (n * 2) " "
    go n = do
        let space = spacing n
            lineSpace = "\n" <> space
        \case
            EAtom _ a -> showAtom a
            ETuple _ e1 e2 ->
                Text.concat
                    [ "("
                    , go (n + 1) e1
                    , lineSpace <> "."
                    , go (n + 1) e2
                    , ")"
                    ]
            EList _ e ->
                Text.concat
                    [ "("
                    , Text.intercalate lineSpace $ go (n + 1) <$> e
                    , ")"
                    ]
            EQuote _ e -> "'" <> go n e
            EFuncQuote _ e -> "#'" <> go n e
