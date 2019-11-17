import System.IO
import System.Exit(exitSuccess)
import Control.Monad(when)
import Data.Maybe

prompt = "calc> "
opers = "*-+"

handleMyError = do
  putStrLn "I cannot calculate that!"
  main

main :: IO ()
main = do
  putStrLn $ prompt ++ "Give me something to calculate or type quit to quit"
  userInput <- getLine
  let newInput = words userInput
  when ((unwords newInput) == "quit") $ exitSuccess
  when (length newInput /= 3 || length (newInput !! 1) /= 1)
    $ handleMyError

  let secondElem = head (newInput !! 1)
      num1 = readMaybe (head newInput) :: Maybe Int
      num2 = readMaybe (last newInput) :: Maybe Int
      operOK = elem secondElem opers
      isInputFail = null num1 || null num2 || operOK /= True
  when (isInputFail == True)
    $ handleMyError
  
  -- Taking values out of Just values after checking for Nothing (null)
  let a = fromJust (num1)
      b = fromJust (num2)

  putStrLn $ show $ calculate secondElem a b
  main

calculate :: Char -> Int -> Int -> Int 
calculate o n1 n2
  | o == '*' = n1 * n2
  | o == '-' = n1 - n2 
  | o == '+' = n1 + n2 

readMaybe :: (Read a) => String -> Maybe a  
readMaybe st = case reads st of [(x,"")] -> Just x  
                                _ -> Nothing 