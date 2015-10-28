module Main where

import Options.Applicative
import System.IO

import Interpreter

main :: IO ()
main = do
        filePath <- execParser (parseFile `withInfo` "Interpret a Brainfuck file")
        fileHandle <- openFile filePath ReadMode
        contents <- hGetContents fileHandle
        runBF contents filePath

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseFile :: Parser FilePath
parseFile = strOption $
  short 'f' <> long "file" <> metavar "FILE-PATH" <>
  help "Path of Brainfuck file to interpret"
