module Data.Tape where

import Data.Char

data Tape = Tape [Int] -- Left of the pivot element
                  Int  -- Pivot element
                 [Int] -- Right of the pivot element

emptyTape :: Tape
emptyTape = Tape zeros 0 zeros
  where zeros = repeat 0

moveRight :: Tape -> Tape
moveRight (Tape ls x (r:rs)) = Tape (x:ls) r rs
moveRight _ = undefined

moveLeft :: Tape -> Tape
moveLeft (Tape (l:ls) x rs) = Tape ls l (x:rs)
moveLeft _ = undefined

increment :: Tape -> Tape
increment (Tape ls x rs) = Tape ls (x+1) rs

decrement :: Tape -> Tape
decrement (Tape ls x rs) = Tape ls (x-1) rs

read :: Tape -> Int
read (Tape _ x _) = x

readChar :: Tape -> Char
readChar tape = chr . fromEnum $ Data.Tape.read tape

write :: Int -> Tape -> Tape
write x (Tape ls _ rs) = Tape ls x rs

writeChar :: Char -> Tape -> Tape
writeChar c = Data.Tape.write $ ord c

