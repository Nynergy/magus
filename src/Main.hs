module Main where

import Brick

import Magus
import MagusTypes

main :: IO AppState
main = do
    state <- initialState
    defaultMain app state
