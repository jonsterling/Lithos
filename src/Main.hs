module Main where

import Text.Lithos
import Text.ParserCombinators.Parsec (parse)

main = do
  s <- getContents
  case parse literateDocument "stdin" s of
    Left err -> putStr ("Error: " ++ show err ++ "\n")
    Right doc -> putStrLn $ writeHtmlString doc