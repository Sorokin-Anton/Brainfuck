module Brainfuck ( brainfuck ) where

import Data.Bifunctor ( first )
import Data.Word ( Word8 )
import Data.Array.Unboxed ( Array, listArray, (//), (!) )
import Data.Char ( chr, ord )

data BFAction
    = CellRight | CellLeft | Inc | Dec | Output | Input | Boxed BFCommand

type BFCommand = [ BFAction ]

parse :: String -> BFCommand
parse [] = []
parse (x : xs) = case x of
    '>' -> CellRight : parse xs
    '<' -> CellLeft : parse xs
    '+' -> Inc : parse xs
    '-' -> Dec : parse xs
    '.' -> Output : parse xs
    ',' -> Input : parse xs
    '[' -> let f :: Int -> String -> ( String, String )
               f _ [] = ( [], [] )
               f n ('[' : rest) = ('[' :) `first` f (n + 1) rest
               f 1 (']' : rest) = ( [], rest )
               f n (']' : rest) = (']' :) `first` f (n - 1) rest
               f n (c : rest) = (c :) `first` f n rest
               ( a, b ) = f 1 xs in Boxed (parse a) : parse b
    _ -> error "Unexpected character"

data State = State { pointer :: Int, tape
                 :: Array Int Word8, input :: String, output :: String }
    deriving Show

atPointer :: (Int -> Int) -> State -> State
atPointer f st = st { pointer = f $ pointer st }

atCurrent :: (Word8 -> Word8) -> State -> State
atCurrent f st
    = st { tape = tape st // [ ( pointer st, f $ tape st ! pointer st ) ] }

lookCurrent :: State -> Word8
lookCurrent st = tape st ! pointer st

takeChar :: State -> ( Char, State )
takeChar st @ State { input = (x : xs) } = ( x, st { input = xs } )
takeChar _ = error "takeChar: empty input"

giveChar :: Char -> State -> State
giveChar x st @ State { output = xs } = st { output = xs ++ [x] }

bf :: BFCommand -> State -> State
bf [] st = st
bf (c : cs) st = bf cs $ case c of
    CellRight -> atPointer (+ 1) st
    CellLeft -> atPointer (subtract 1) st
    Inc -> atCurrent (+ 1) st
    Dec -> atCurrent (subtract 1) st
    Output -> giveChar (chr . fromIntegral . lookCurrent $ st) st
    Input -> let ( x, new ) = takeChar st in atCurrent
        (const . fromIntegral $ ord x) new
    Boxed cmds -> if lookCurrent st == 0 then st else let new = bf cmds st
        in if lookCurrent new == 0 then new else bf [c] new

brainfuck :: String -> String -> String
brainfuck code input = output . bf (parse code) $
    State { pointer = 0, tape = listArray ( 0, 10000 ) [0, 0 .. ], input = input, output = "" }
