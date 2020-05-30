{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Lexer
( TokenClass(..), Token
, runLexer ) where

import Nihil.Utils.Source
import Nihil.CommonError
import Nihil.Syntax.Concrete.Core
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL
import Data.Void
import Data.Bifunctor (bimap)
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import qualified Data.List.NonEmpty as NonEmpty
import Data.Char (GeneralCategory(LineSeparator))
import Control.Applicative ((<|>))


type Token = Located TokenClass
type InterToken = Maybe Token

-------------------------------------------------------------

type Parser = MP.Parsec Void Text.Text

runLexer :: Text.Text -> String -> Either (Diagnostic Text.Text) [Token]
runLexer input fileName =
    let res = MP.runParser lexProgram fileName input
    in bimap toDiagnostic toTokens res
  where toTokens = catMaybes

        toDiagnostic MP.ParseErrorBundle{..} =
            let diag = errorDiagnostic
                    `withMessage` "Lexical error on input"
                    `withBatches` [ newBatch `withLabels` (NonEmpty.toList bundleErrors >>= parseErrorToLabel bundlePosState) ]
            in diag

        parseErrorToLabel :: MP.PosState Text.Text -> MP.ParseError Text.Text Void -> [Label]
        parseErrorToLabel pst err =
            let (_, pos)    = MP.reachOffset (MP.errorOffset err) pst
                !source     = fromSourcePos (MP.pstateSourcePos pos)
                msgs        = lines (MP.parseErrorTextPretty err)
            in  if | length msgs == 1 ->
                       [ primaryLabel source `withLabelMessage` (msgs !! 0) ]
                   | length msgs == 2 ->
                       [ primaryLabel source `withLabelMessage` (msgs !! 0)
                       , secondaryLabel source `withLabelMessage` (msgs !! 1) ]
                   | otherwise        ->
                       [ primaryLabel source `withLabelMessage` "unknown parse error" ]

lexProgram :: Parser [InterToken]
lexProgram = do
    tokens <- MP.many anyToken
    eof    <- Just <$> withSourceSpan (TkEOF <$ MP.eof)
    pure (tokens <> [eof])
  where anyToken = MP.choice
            [ white
            , keywordOrLIdent
            , uIdent
            , comment
            , literal
            , symbolOrSpecial
            ]

withSourceSpan :: Parser a -> Parser (Located a)
withSourceSpan p = do
    begin <- fromSourcePos <$> MP.getSourcePos
    res   <- p
    end   <- fromSourcePos <$> MP.getSourcePos
    -- ! For now, discard the end position, but will be used later
    pure (locate res begin)

-----------------------------------------------------------------------

white :: Parser InterToken
white = MP.label "" do
    MP.choice [ Just    <$> withSourceSpan (TkEOL <$ MPC.charCategory LineSeparator)
              , Nothing <$  MPC.spaceChar
              ]

keywordOrLIdent :: Parser InterToken
keywordOrLIdent = Just <$> MP.label "lower identifier" do
    withSourceSpan do
        (f .) . (:) <$> MPC.lowerChar <*> MP.many (MPC.alphaNumChar <|> MPC.char '\'')
  where f "match"   = TkMatch
        f "in"      = TkIn
        f "with"    = TkWith
        f "let"     = TkLet
        f "data"    = TkData
        f "type"    = TkType
        f "where"   = TkWhere
        f "infixl"  = TkInfixL
        f "infixr"  = TkInfixR
        f "λ"       = TkLambda
        f ident     = TkLIdent ident

uIdent :: Parser InterToken
uIdent = Just <$> MP.label "upper identifier" do
    withSourceSpan do
        (f .) . (:) <$> MPC.upperChar <*> MP.many (MPC.alphaNumChar <|> MPC.char '\'')
  where f ident = TkUIdent ident

comment :: Parser InterToken
comment = Just <$> (lineComment <|> multiLineComment)
  where lineComment = withSourceSpan do
            MPC.string' "--" *> MP.notFollowedBy MPC.symbolChar
            TkInlineComment <$> MP.manyTill MP.anySingle MPC.eol
        multiLineComment = withSourceSpan do
            () <$ MPC.string' "{-"
            TkMultilineComment <$> MP.manyTill MP.anySingle (MPC.string' "-}")

symbolOrSpecial :: Parser InterToken
symbolOrSpecial = Just <$> withSourceSpan do
    MP.choice [ TkLParen     <$ MPC.char '('
              , TkRParen     <$ MPC.char ')'
              , TkSemi       <$ MPC.char ';'
              , TkColon      <$ MPC.char ':'
              , TkComma      <$ MPC.char ','
              , TkUnderscore <$ MP.some (MPC.char '_')
              , TkBackslash  <$ MPC.char '\\'
              , TkBacktick   <$ MPC.char '`'
              , TkProd       <$ MPC.char '∏'
              , TkSum        <$ MPC.char '∑'
              , f            <$> MP.some ((MPC.symbolChar <|> MPC.punctuationChar) >>= satisfies (not . unusable))
              ]
  where f "|"     = TkBar
        f "="     = TkEquals
        f "->"    = TkArrow
        f "→"     = TkArrow
        f "=>"    = TkImplies
        f "⇒"     = TkImplies
        f "."     = TkDot
        f symbol  = TkSym symbol

        satisfies f c
            | f c       = pure c
            | otherwise = fail ("Predicate not held for character " <> show c)

        unusable '(' = True
        unusable ')' = True
        unusable ';' = True
        unusable ':' = True
        unusable '`' = True
        unusable '{' = True
        unusable '}' = True
        unusable _   = False

literal :: Parser InterToken
literal = Just <$> MP.label "literal" do
    withSourceSpan do
        MP.choice
            [ TkFloat  <$> MP.try MPL.float MP.<?> "float literal"
            , TkInt    <$> MP.choice [ binL, octL, hexL, MPL.decimal MP.<?> "decimal literal" ]
            , TkChar   <$> (MPC.char '\'' *> MPL.charLiteral <* MPC.char '\'') MP.<?> "character literal"
            , TkString <$> (MPC.char '"' *> MP.manyTill MPL.charLiteral (MPC.char '"')) MP.<?> "string literal"
            ]
  where binL = MPC.string' "0b" *> MPL.binary      MP.<?> "binary literal"
        octL = MPC.string' "0o" *> MPL.octal       MP.<?> "octal literal"
        hexL = MPC.string' "0x" *> MPL.hexadecimal MP.<?> "hexadecimal literal"
