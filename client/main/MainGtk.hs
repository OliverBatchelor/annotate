module Main where

import qualified Client.Main as M

import Language.Javascript.JSaddle.WebKitGTK

main :: IO ()
main = run $ Map.main "localhost:3000"
  
