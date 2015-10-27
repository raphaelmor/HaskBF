module Interpreter (runBF) where

import Data.AST
import Data.Tape
import Parser

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy

import Data.Foldable

type Interpreter = StateT Tape IO ()

runBF :: String -> IO ()
runBF source = do
                 let parseRes = parseBF source
                 case parseRes of
                      Left err  -> putStrLn ("Syntax error: " ++ show err)
                      Right ast -> interpretBF ast >> putStrLn ""

interpretBF :: AST -> IO ()
interpretBF ast = runStateT (interpretAST ast) emptyTape >> return ()

interpretAST :: AST -> Interpreter
interpretAST = traverse_ interpretInstruction

interpretInstruction :: Instruction -> Interpreter
interpretInstruction (Loop ast) = interpretLoop ast
interpretInstruction inst = interpretNonLoop inst

interpretLoop :: AST -> Interpreter
interpretLoop ast = do
  tape <- get
  if (Data.Tape.read tape) == 0
     then return ()
     else do interpretAST ast >> interpretLoop ast

interpretNonLoop :: Instruction -> Interpreter
interpretNonLoop MoveLeft = do modify (moveLeft)
interpretNonLoop MoveRight = do modify (moveRight)
interpretNonLoop Increment = do modify (increment)
interpretNonLoop Decrement = do modify (decrement)
interpretNonLoop PutByte   = do
                               tape <- get
                               liftIO . putChar $ Data.Tape.readChar tape
interpretNonLoop GetByte   = do
                               char <- liftIO getChar
                               modify (Data.Tape.writeChar char)
interpretNonLoop _         = fail "error"

