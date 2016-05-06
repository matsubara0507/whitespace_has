module Whitespace (
  runEval,
  eval,
  showWhitespace
)where

import Machine
import Parser
import Data.Char (chr, ord)
import Data.Maybe (fromMaybe)
import Control.Monad.State
import System.IO (hFlush, stdout)

type Env = Machine IMP
type Exp = [IMP]
type Eval a = StateT Env IO a

runEval :: Env -> Eval a -> IO (a, Env)
runEval st ev = runStateT ev st

eval :: Exp -> Eval ()
eval (PUSH n : cmd) = get >>= put . incPC . push n >> eval cmd
eval (DUP : cmd) = 
  get >>= (\m -> let one = top m in put . incPC $ push one m) >> eval cmd
eval (COPY n : cmd) = 
  get >>= (\m -> let i = copyNth n m in put . incPC $ push i m) >> eval cmd
  where 
    copyNth n m = if n <= 0 then top m else copyNth (n-1) (snd $ pop m)
eval (SWAP : cmd) = 
  get >>= (\m -> let (one, m1) = pop m; (two, m2) = pop m1 in 
                  put . incPC . push two $ push one m2) >> eval cmd
eval (DISC : cmd) = 
  get >>= (\m -> let (_, m') = pop m in put $ incPC m') >> eval cmd
eval (SLIDE n : cmd) = get >>= (\m -> let i = top m; m' = slideN n m in 
                                      put . incPC $ push i m') >> eval cmd
  where
     slideN n m = if n < 0 then m else slideN (n-1) (snd $ pop m)
eval (ADD : cmd) = get >>= (\m -> let (n1, m1) = pop m; (n2, m2) = pop m1 in 
                              put . incPC $ push (n2 + n1) m2) >> eval cmd
eval (SUB : cmd) = get >>= (\m -> let (n1, m1) = pop m; (n2, m2) = pop m1 in 
                              put . incPC $ push (n2 - n1) m2) >> eval cmd
eval (MUL : cmd) = get >>= (\m -> let (n1, m1) = pop m; (n2, m2) = pop m1 in 
                              put . incPC $ push (n2 * n1) m2) >> eval cmd
eval (DIV : cmd) = get >>= (\m -> let (n1, m1) = pop m; (n2, m2) = pop m1 in 
                              put . incPC $ push (n2 `div` n1) m2) >> eval cmd
eval (MOD : cmd) = get >>= (\m -> let (n1, m1) = pop m; (n2, m2) = pop m1 in 
                              put . incPC $ push (n2 `mod` n1) m2) >> eval cmd
eval (STORE : cmd) = get >>= (\m -> let (dat, m1) = pop m; (adr, m2) = pop m1 in 
                              put . incPC $ store adr dat m2) >> eval cmd
eval (RETRIEVE : cmd) = 
  get >>= (\m -> let (adr, m') = pop m; 
                     dat = fromMaybe (emessage adr m') $ retrieve adr m' in 
                 put . incPC $ push dat m') >> eval cmd
  where 
    emessage adr m = error $ "error: address " ++ show adr ++ " is not found." 
      ++ "VM show: " ++ show m
eval (MARK label : cmd) = get >>= put . incPC >> eval cmd
eval (CALL label : cmd) = get >>= (\(cmd', m) -> put m >> eval cmd') 
                        . jumpIf True label cmd . pushCallStack . incPC
eval (JUMP label : cmd) = 
  get >>= (\(cmd', m) -> put m >> eval cmd') . jumpIf True label cmd . incPC
eval (JMPZERO label : cmd) = 
  get >>= (\(dat, m) -> (\(cmd', m') -> put m' >> eval cmd') 
                        $ jumpIf (dat == 0) label cmd (incPC m)) . pop
eval (JMPNEG label : cmd) = 
  get >>= (\(dat, m) -> (\(cmd', m') -> put m' >> eval cmd') 
                        $ jumpIf (dat < 0) label cmd (incPC m)) . pop
eval (RETURN : cmd) = get >>= (\(cmd', m) -> put m >> eval cmd') . popCallStack
eval (EXIT : cmd) = return ()
eval (OUTCHR : cmd) = get >>= (\(dat, m) -> 
  lift (putChar (chr dat) >> hFlush stdout) >> put (incPC m)) . pop >> eval cmd
eval (OUTNUM : cmd) = get >>= (\(dat, m) -> 
  lift (putStr (show dat) >> hFlush stdout) >> put (incPC m)) . pop >> eval cmd
eval (INCHR : cmd) = get >>= (\m -> lift getChar >>= 
  (\c -> put . incPC $ store (top m) (ord c) m)) >> eval cmd
eval (INNUM : cmd) = (get >>= (\m -> lift getLine >>= 
  (\n -> put . incPC $ store (top m) (read n) m))) >> eval cmd
eval cmd = error $ "semantic error: head is " ++ show (take 10 cmd)

jumpIf :: Bool -> String -> Exp -> Machine IMP -> (Exp, Machine IMP)
jumpIf p label cmd m = 
  if p then 
    let (cmd', m') = jumpLabel label m in (fromMaybe (emessage label) cmd', m')
  else (cmd, m)
  where
    emessage label = error ("error: Label " ++ show label ++ " is not found.")
