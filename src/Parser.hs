module Parser (parseBF) where

import Text.Parsec
import Text.Parsec.String

import Data.AST

parseBF :: String -> String -> Either ParseError AST
parseBF source filename = parse fullProgram filename source

fullProgram :: Parser AST
fullProgram = program <* eof

program :: Parser AST
program = do
  skipMany comment
  sepEndBy instruction (many comment)


instruction :: Parser Instruction
instruction = simpleOperation <|> loop

simpleOperation :: Parser Instruction
simpleOperation = fmap toInstruction validOperationChar
  where toInstruction '<' = MoveLeft
        toInstruction '>' = MoveRight
        toInstruction '+' = Increment
        toInstruction '-' = Decrement
        toInstruction '.' = PutByte
        toInstruction ',' = GetByte
        toInstruction  _  = error "Unknown character" -- this should never happen

loop :: Parser Instruction
loop = Loop <$> between (char '[') (char ']') program

comment :: Parser Char
comment = noneOf "<>+-.,[]"

validOperationChar :: Parser Char
validOperationChar = oneOf "<>+-.,"
