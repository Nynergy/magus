module Main where

import Brick

import Magus

main :: IO AppState
main = do
    state <- initialState
    defaultMain app state
