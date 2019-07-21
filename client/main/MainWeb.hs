module Main where

import qualified Client.Main as M

import Reflex.Dom.Location

main :: IO ()
main = getLocationHost >>= M.main
  
