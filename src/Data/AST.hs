module Data.AST where

data Instruction = MoveLeft           -- '<'
                 | MoveRight          -- '>'
                 | Increment          -- '+'
                 | Decrement          -- '-'
                 | PutByte            -- '.'
                 | GetByte            -- ','
                 | Loop [Instruction] -- '[...]'
                 deriving (Show)

type AST = [Instruction]

