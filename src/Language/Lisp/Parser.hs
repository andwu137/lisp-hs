module Language.Lisp.Parser (
    Parser,
    expr,
) where

import qualified Data.Text as Text
import Data.Void (Void)
import qualified Language.Lisp.IR.AST as AST
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = P.Parsec Void Text.Text

{- HELPERS -}
withPos ::
    (P.SourcePos -> a -> b) ->
    Parser a ->
    Parser b
withPos f p = f <$> P.getSourcePos <*> p

space :: Parser ()
space = L.space P.space1 skipLineComment skipBlockComment

skipLineComment :: Parser ()
skipLineComment = L.skipLineComment ";"

skipBlockComment :: Parser ()
skipBlockComment = L.skipBlockComment "#|" "|#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text.Text -> Parser Text.Text
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")

{- Reserved -}
reservedSymbols :: [Text.Text]
reservedSymbols = ["(", ")", "'"]

{- Expr -}
expr :: Parser (AST.Expr P.SourcePos)
expr =
    P.choice
        [ efuncquote
        , equote
        , P.try etuple
        , elist
        , eatom
        ]

equote :: Parser (AST.Expr P.SourcePos)
equote =
    withPos AST.EQuote $
        "'" *> expr

efuncquote :: Parser (AST.Expr P.SourcePos)
efuncquote =
    withPos AST.EFuncQuote $
        "#'" *> expr

elist :: Parser (AST.Expr P.SourcePos)
elist =
    withPos AST.EList $
        parens $
            P.some expr

etuple :: Parser (AST.Expr P.SourcePos)
etuple =
    withPos (uncurry . AST.ETuple) $
        parens $
            (,) <$> expr <*> (symbol "." *> expr)

eatom :: Parser (AST.Expr P.SourcePos)
eatom = withPos AST.EAtom aatom

{- Atom -}
aatom :: Parser (AST.Atom P.SourcePos)
aatom =
    lexeme $
        P.choice
            [ P.try afloat
            , aint
            , achar
            , astring
            , aident
            ]

afloat :: Parser (AST.Atom P.SourcePos)
afloat =
    P.label "Float" $
        withPos AST.AFloat L.float

aint :: Parser (AST.Atom P.SourcePos)
aint =
    P.label "Integer" $
        withPos AST.AFloat L.float

aident :: Parser (AST.Atom P.SourcePos)
aident =
    P.label "Identifier"
        . withPos AST.AIdent
        $ Text.pack
            <$> ( (:)
                    <$> P.letterChar
                    <*> P.many (P.notFollowedBy ignore *> P.anySingle)
                )
  where
    ignore = P.choice $ P.string <$> (["\r", "\n", " "] <> reservedSymbols)

achar :: Parser (AST.Atom P.SourcePos)
achar =
    P.label "Char"
        . withPos AST.AChar
        $ P.between (symbol "'") (symbol "'") P.letterChar

astring :: Parser (AST.Atom P.SourcePos)
astring =
    P.label "String"
        . withPos AST.AString
        $ Text.pack <$> (P.char '"' *> P.manyTill L.charLiteral (P.char '"'))
