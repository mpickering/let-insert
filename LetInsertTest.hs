{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices -dcore-lint #-}
module LetInsertTest where

import LetInsert

test = $$(res) [A,B,A,B]

{-
- LetInsertTest.hs:7:10-14: Splicing expression
    res
  ======>
    let
      n_a4zR a_a4zS
        = case a_a4zS of
            (A : r_a4zT) -> n_a4zN r_a4zT
            (B : r_a4zU) -> n_a4zR r_a4zU
            [] -> False
      n_a4zN a_a4zO
        = case a_a4zO of
            (A : r_a4zP) -> n_a4zJ r_a4zP
            (B : r_a4zQ) -> n_a4zR r_a4zQ
            [] -> False
      n_a4zJ a_a4zK
        = case a_a4zK of
            (A : r_a4zL) -> n_a4zJ r_a4zL
            (B : r_a4zM) -> n_a4zN r_a4zM
            [] -> True
    in n_a4zJ
    -}
