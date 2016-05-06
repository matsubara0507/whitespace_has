import Whitespace
import Parser as P
import System.Environment (getArgs)
import Machine as M

main :: IO ()
main = do
  [fileName] <- getArgs
  code <- filter (/= 'C') . fmap P.showWhitespace <$> readFile fileName
  --putStrLn code
  let m = parse code M.empty
  --print $ getCommand m
  _ <- runEval (resetPC m) $ eval (getCommand m)
  putStrLn ""
