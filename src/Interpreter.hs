module Interpreter (runBF) where

import Data.AST
import Data.Tape
import Parser

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy

import Data.Foldable

type Interpreter = StateT Tape IO ()

runBF :: String -> String -> IO ()
runBF source filename = case parseBF source filename of
                          Left err  -> putStrLn ("Syntax error: " ++ show err)
                          Right ast -> interpretBF ast

interpretBF :: AST -> IO ()
interpretBF ast = evalStateT (interpretAST ast) emptyTape

interpretAST :: AST -> Interpreter
interpretAST = traverse_ interpretInstruction

interpretInstruction :: Instruction -> Interpreter
interpretInstruction MoveLeft   = modify moveLeft
interpretInstruction MoveRight  = modify moveRight
interpretInstruction Increment  = modify increment
interpretInstruction Decrement  = modify decrement
interpretInstruction PutByte    = do tape <- get
                                     liftIO . putChar $ Data.Tape.readChar tape
interpretInstruction GetByte    = do char <- liftIO getChar
                                     modify (Data.Tape.writeChar char)

interpretInstruction loop@(Loop ast) = do tape <- get
                                          unless (Data.Tape.read tape == 0) (interpretAST ast >> interpretInstruction loop)
