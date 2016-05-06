module Parser (
  IMP (..),
  parse,
  showWhitespace
)where

import Machine
import Data.Maybe (fromMaybe)
import Control.Monad.State

data IMP = PUSH Int 
         | DUP
         | COPY Int
         | SWAP
         | DISC
         | SLIDE Int
         | ADD
         | SUB
         | MUL
         | DIV
         | MOD
         | STORE
         | RETRIEVE
         | MARK String
         | CALL String
         | JUMP String
         | JMPZERO String
         | JMPNEG String
         | RETURN
         | EXIT
         | OUTCHR
         | OUTNUM
         | INCHR
         | INNUM 
         deriving (Show, Eq)

parse :: String -> Machine IMP -> Machine IMP
parse [] m = m
parse code m = (\(imp, code', m') -> parse code' . 
                            incPC $ addCommand [imp] m') (code2exp code m)
  where
    code2exp :: String -> Machine IMP -> (IMP, String, Machine IMP)
    code2exp ('S' : 'S' : code) m = 
      let (num, code') = getNum code in (PUSH num, code', m)
    code2exp ('S' : 'T' : 'S' : code) m = 
      let (num, code') = getNum code in (COPY num, code', m)
    code2exp ('S' : 'T' : 'L' : code) m = 
      let (num, code') = getNum code in (SLIDE num, code', m)
    code2exp ('S' : 'L' : 'S' : code) m = (DUP, code, m) 
    code2exp ('S' : 'L' : 'T' : code) m = (SWAP, code, m) 
    code2exp ('S' : 'L' : 'L' : code) m = (DISC, code, m) 
    code2exp ('T' : 'S' : 'S' : 'S' : code) m = (ADD, code, m) 
    code2exp ('T' : 'S' : 'S' : 'T' : code) m = (SUB, code, m) 
    code2exp ('T' : 'S' : 'S' : 'L' : code) m = (MUL, code, m) 
    code2exp ('T' : 'S' : 'T' : 'S' : code) m = (DIV, code, m) 
    code2exp ('T' : 'S' : 'T' : 'T' : code) m = (MOD, code, m) 
    code2exp ('T' : 'T' : 'S' : code) m = (STORE, code, m) 
    code2exp ('T' : 'T' : 'T' : code) m = (RETRIEVE, code, m)
    code2exp ('L' : 'S' : 'S' : code) m = let (label, code') = getLabel code 
      in (MARK label, code', addLabel label m)
    code2exp ('L' : 'S' : 'T' : code) m = 
      let (label, code') = getLabel code in (CALL label, code', m)
    code2exp ('L' : 'S' : 'L' : code) m =
      let (label, code') = getLabel code in (JUMP label, code', m)
    code2exp ('L' : 'T' : 'S' : code) m =
      let (label, code') = getLabel code in (JMPZERO label, code', m)
    code2exp ('L' : 'T' : 'T' : code) m = 
      let (label, code') = getLabel code in (JMPNEG label, code', m)
    code2exp ('L' : 'T' : 'L' : code) m = (RETURN, code, m)
    code2exp ('L' : 'L' : 'L' : code) m = (EXIT, code, m)
    code2exp ('T' : 'L' : 'S' : 'S': code) m = (OUTCHR, code, m)
    code2exp ('T' : 'L' : 'S' : 'T': code) m = (OUTNUM, code, m)
    code2exp ('T' : 'L' : 'T' : 'S': code) m = (INCHR, code, m)
    code2exp ('T' : 'L' : 'T' : 'T': code) m = (INNUM, code, m)
    code2exp code m = error $ "parse error: head is " ++ take 10 code

getNum :: String -> (Int, String)
getNum (s:code) = (sign * snd (foldr f (0,0) xs), tail code')
  where
    (xs, code') = break (== 'L') code
    f x (b,n) = (b+1, if x == 'S' then n else n + (2^b))
    sign = if s == 'S' then 1 else -1 
 
getLabel :: String -> (String, String)
getLabel code = (label, tail code')
  where 
    (label, code') = break (== 'L') code

showWhitespace :: Char -> Char
showWhitespace ' ' = 'S'
showWhitespace '\t' = 'T'
showWhitespace '\n' = 'L'
showWhitespace '\r' = 'L'
showWhitespace _ = 'C'
