module Machine (
  Machine (..), 
  empty, 
  addCommand, 
  store, 
  retrieve, 
  push, 
  pop, 
  top,
  resetPC,
  incPC,
  addPC,
  jumpLabel,
  addLabel,
  pushCallStack,
  popCallStack
  ) where

import qualified Stack
import qualified Data.Map.Strict as Map
import Control.Arrow (first)

type Heap a = Map.Map Int a
data Machine a = M { getStack :: Stack.Stack Int
                   , getHeap :: Heap Int
                   , getPC :: Int
                   , getCommand :: [a]
                   , getLabels :: Map.Map String Int
                   , getCallStack :: Stack.Stack Int
                   } deriving (Show, Eq)

empty :: Machine a
empty = M Stack.empty Map.empty 0 [] Map.empty Stack.empty

addCommand :: [a] -> Machine a -> Machine a
addCommand cmd (M s h pc cmd' ls cs) = M s h pc (cmd' ++ cmd) ls cs

store :: Int -> Int -> Machine a -> Machine a
store k v (M s h pc cmd ls cs) = M s (Map.insert k v h) pc cmd ls cs 

retrieve :: Int -> Machine a -> Maybe Int
retrieve k = Map.lookup k . getHeap

push :: Int -> Machine a -> Machine a
push a (M s h pc cmd ls cs) = M (Stack.push a s) h pc cmd ls cs

top :: Machine a -> Int
top = Stack.top . getStack

pop :: Machine a -> (Int, Machine a)
pop (M s h pc cmd ls cs) = 
  let (a, s') = Stack.pop s in (a, M s' h pc cmd ls cs)

resetPC :: Machine a -> Machine a
resetPC (M s h pc cmd ls cs) = M s h 0 cmd ls cs

incPC :: Machine a -> Machine a
incPC (M s h pc cmd ls cs) = M s h (pc + 1) cmd ls cs

addPC :: Int -> Machine a -> Machine a
addPC n (M s h pc cmd ls cs) = M s h (pc + n) cmd ls cs

jumpLabel :: String -> Machine a -> (Maybe [a], Machine a)
jumpLabel label (M s h pc cmd ls cs)
  | Just n <- Map.lookup label ls = (Just (drop (n+1) cmd), M s h (n+1) cmd ls cs)
  | otherwise = (Nothing, M s h pc cmd ls cs)

addLabel :: String -> Machine a -> Machine a
addLabel label (M s h pc cmd ls cs) = M s h pc cmd (Map.insert label pc ls) cs

pushCallStack :: Machine a -> Machine a
pushCallStack (M s h pc cmd ls cs) = M s h pc cmd ls (Stack.push pc cs) 

popCallStack :: Machine a -> ([a], Machine a)
popCallStack (M s h pc cmd ls cs) = 
  let (pc', cs') = Stack.pop cs in (drop pc' cmd, M s h pc' cmd ls cs') 
 