module Intermediate.Parse
  ( parse
  )
  where

import Error (Error, fromParseErrorBundle)
import Data.Int (Int32)
import Text.Megaparsec.Char (space1)

import Text.Megaparsec
  ( Parsec
  , ParseErrorBundle
  , runParser
  , (<|>)
  , some
  , many
  , oneOf
  , single
  , optional
  , between
  , sepBy
  , try
  , eof
  )

import Text.Megaparsec.Char.Lexer
  ( space
  , skipLineComment
  , skipBlockComment
  , lexeme
  , symbol
  , decimal
  , float
  )

import Intermediate.Syntax
  ( Atom
  , nil
  , wrap
  , abstract
  , BinOp (..)
  , BoolOp (..)
  , CompOp (..)
  , Target (..)
  , Operation (..)
  , Chain (..)
  , Program (..)
  )

type Parse = Parsec String String

runParse :: Parse a -> String -> Either (ParseErrorBundle String String) a
runParse action = runParser action ""

parseSpace :: Parse ()
parseSpace = space space1 (skipLineComment "//") (skipBlockComment "/*" "*/")

parseLexeme :: Parse a -> Parse a
parseLexeme = lexeme parseSpace

parseSymbol :: String -> Parse String
parseSymbol = symbol parseSpace

parseIdentifier :: Parse String
parseIdentifier = parseLexeme (some $ oneOf validCharacters) where
  validCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']

parseIdentifiers :: Parse [String]
parseIdentifiers = sepBy parseIdentifier (parseSymbol ",")

parseCurlyBrackets :: Parse a -> Parse a
parseCurlyBrackets = between (parseSymbol "{") (parseSymbol "}")

parseSquareBrackets :: Parse a -> Parse a
parseSquareBrackets = between (parseSymbol "[") (parseSymbol "]")

parseEnvs :: Parse [String]
parseEnvs = parseCurlyBrackets parseIdentifiers

parseArgs :: Parse [String]
parseArgs = parseSquareBrackets parseIdentifiers

parseAtom :: Parse Atom
parseAtom = nil <$ parseSymbol "NIL" <|> wrap <$> parseIdentifier

parseAtoms :: Parse [Atom]
parseAtoms = sepBy parseAtom (parseSymbol ",")

parseEnvAtoms :: Parse [Atom]
parseEnvAtoms = parseCurlyBrackets parseAtoms

parseArgAtoms :: Parse [Atom]
parseArgAtoms = parseSquareBrackets parseAtoms

parseTarget :: Parse Target
parseTarget = Target <$> parseIdentifier <*> parseArgAtoms

parsePure :: Parse Operation
parsePure = Pure <$> (parseSymbol "Pure" *> parseSquareBrackets parseAtom)

parseJump :: Parse Operation
parseJump = Jump <$> (parseSymbol "Jump" *> parseTarget)

parseClosureAlloc :: Parse Operation
parseClosureAlloc =
  ClosureAlloc <$> (parseSymbol "closure.Alloc" *> parseIdentifier) <*> parseEnvAtoms

parseClosureEnter :: Parse Operation
parseClosureEnter =
  ClosureEnter <$> (parseSymbol "closure.Enter" *> parseSquareBrackets parseAtom) <*> parseArgAtoms

parseStructAlloc :: Parse Operation
parseStructAlloc =
  StructAlloc <$> (parseSymbol "struct.Alloc" *> parseArgAtoms)

parseStructSelect :: Parse Operation
parseStructSelect =
  StructSelect <$> (parseSymbol "struct.Select" *> parseSquareBrackets parseAtom) <*> decimal

parseInt32 :: Parse Int32
parseInt32 = parseLexeme (try positive <|> negative) where
  positive = optional (single '+') *> decimal
  negative = single '-' *> (negate <$> decimal)

parseInt32Alloc :: Parse Operation
parseInt32Alloc = Int32Alloc <$> (parseSymbol "int32.Alloc" *> parseInt32)

parseInt32If :: Parse Operation
parseInt32If = do
  atom <- parseSymbol "int32.If" *> parseSquareBrackets parseAtom <* parseSymbol "do"
  truthy <- parseSymbol "true" *> parseSymbol "=" *> parseTarget <* parseSymbol ";"
  falsy <- parseSymbol "false" *> parseSymbol "=" *> parseTarget <* parseSymbol ";"
  Int32If atom truthy falsy <$ parseSymbol "end"

parseInt32Branch :: Parse (Int32, Target)
parseInt32Branch = do
  branch <- parseInt32 <* parseSymbol "="
  target <- parseTarget <* parseSymbol ";"
  return (branch, target)

parseInt32Match :: Parse Operation
parseInt32Match = do
  atom <- parseSymbol "int32.Match" *> parseSquareBrackets parseAtom <* parseSymbol "do"
  branches <- many parseInt32Branch <* parseSymbol "end"
  return (Int32Match atom branches)

parseInt32BinOp :: Parse Operation
parseInt32BinOp = do
  op <- try (Add <$ parseSymbol "int32.Add")
    <|> try (Sub <$ parseSymbol "int32.Sub")
    <|> try (Mul <$ parseSymbol "int32.Mul")
    <|> try (Div <$ parseSymbol "int32.Div")
  
  one <- parseSymbol "[" *> parseAtom <* parseSymbol ","
  other <- parseAtom <* parseSymbol "]"
  return (Int32BinOp op one other)

parseInt32BoolOp :: Parse Operation
parseInt32BoolOp = do
  op <- try (And <$ parseSymbol "int32.And")
    <|> try (Or <$ parseSymbol "int32.Or")
  
  one <- parseSymbol "[" *> parseAtom <* parseSymbol ","
  other <- parseAtom <* parseSymbol "]"
  return (Int32BoolOp op one other)

parseInt32CompOp :: Parse Operation
parseInt32CompOp = do
  op <- try (Eq <$ parseSymbol "int32.Eq")
    <|> try (Ne <$ parseSymbol "int32.Ne")
    <|> try (Lt <$ parseSymbol "int32.Lt")
    <|> try (Le <$ parseSymbol "int32.Le")
    <|> try (Gt <$ parseSymbol "int32.Gt")
    <|> try (Ge <$ parseSymbol "int32.Ge")
  
  one <- parseSymbol "[" *> parseAtom <* parseSymbol ","
  other <- parseAtom <* parseSymbol "]"
  return (Int32CompOp op one other)

parseFlt32 :: Parse Float
parseFlt32 = parseLexeme (try positive <|> negative) where
  positive = optional (single '+') *> float
  negative = single '-' *> (negate <$> float)

parseFlt32Alloc :: Parse Operation
parseFlt32Alloc = Flt32Alloc <$> (parseSymbol "flt32.Alloc" *> parseFlt32)

parseFlt32BinOp :: Parse Operation
parseFlt32BinOp = do
  op <- try (Add <$ parseSymbol "flt32.Add")
    <|> try (Sub <$ parseSymbol "flt32.Sub")
    <|> try (Mul <$ parseSymbol "flt32.Mul")
    <|> try (Div <$ parseSymbol "flt32.Div")
  
  one <- parseSymbol "[" *> parseAtom <* parseSymbol ","
  other <- parseAtom <* parseSymbol "]"
  return (Flt32BinOp op one other)

parseFlt32CompOp :: Parse Operation
parseFlt32CompOp = do
  op <- try (Eq <$ parseSymbol "flt32.Eq")
    <|> try (Ne <$ parseSymbol "flt32.Ne")
    <|> try (Lt <$ parseSymbol "flt32.Lt")
    <|> try (Le <$ parseSymbol "flt32.Le")
    <|> try (Gt <$ parseSymbol "flt32.Gt")
    <|> try (Ge <$ parseSymbol "flt32.Ge")
  
  one <- parseSymbol "[" *> parseAtom <* parseSymbol ","
  other <- parseAtom <* parseSymbol "]"
  return (Flt32CompOp op one other)

parseOperation :: Parse Operation
parseOperation = try parsePure
  <|> try parseJump
  <|> try parseClosureAlloc
  <|> try parseClosureEnter
  <|> try parseStructAlloc
  <|> try parseStructSelect
  <|> try parseInt32Alloc
  <|> try parseInt32If
  <|> try parseInt32Match
  <|> try parseInt32BinOp
  <|> try parseInt32BoolOp
  <|> try parseInt32CompOp
  <|> try parseFlt32Alloc
  <|> try parseFlt32BinOp
  <|> try parseFlt32CompOp

parseChain :: Parse Chain
parseChain = try namedOperation <|> tailOperation where
  namedOperation = do
    name <- parseIdentifier <* parseSymbol "<-"
    operation <- parseOperation <* parseSymbol ";"
    continuation <- parseChain
    return Chain { operation, continuation = Just (abstract [name] continuation) }

  tailOperation = do
    operation <- parseOperation <* parseSymbol "end"
    return Chain { operation, continuation = Nothing }

parseBlock :: Parse Program
parseBlock = do
  name <- parseSymbol "block " *> parseIdentifier
  args <- parseArgs
  chain <- parseSymbol "do" *> parseChain
  return Program { blocks = [(name, abstract args chain)], closures = [] } 

parseClosure :: Parse Program
parseClosure = do
  name <- parseSymbol "closure " *> parseIdentifier
  envs <- parseEnvs
  args <- parseArgs
  chain <- parseSymbol "do" *> parseChain
  return Program { blocks = [], closures = [(name, abstract envs $ abstract args chain)] }

parseProgram :: Parse Program
parseProgram = mconcat <$> many (parseBlock <|> parseClosure)

parse :: String -> Either Error Program
parse source = case runParse (parseSpace *> parseProgram <* eof) source of
  Left bundle -> Left (fromParseErrorBundle bundle)
  Right program -> Right program
