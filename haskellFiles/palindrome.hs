import Data.Char

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

main :: IO ()
main = do
  putStrLn "Please enter a phrase:"
  l <- getLine
  if isPalindrome l then putStrLn "yes" else putStrLn "no"
