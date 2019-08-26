module Main where

import System.Environment

import Data.Proxy

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Ned.Data.FingerTree.Test
import Ned.Data.FingerTreeZip.Test
import Ned.Data.Seq.Test
import Ned.Data.ReflectNat.Test


import Ned.Data.Buffer.Test
import Ned.Data.TextBox.Test
import Ned.Data.RunLengthEncoding.Test
import Ned.Data.MeasureText.Test
import Ned.Data.ScreenOffset.Test

import Lang.Data.Type.Test
import Lang.Data.Module.Test
import Lang.Data.Interpreter.Test
import Lang.Data.Parser.Test


main :: IO ()
main = do
  setEnv "TASTY_NUM_THREADS" "6"
  setEnv "TASTY_QUICKCHECK_MAX_SIZE" "300"
  setEnv "TASTY_QUICKCHECK_TESTS" "10000"
  putStrLn "\n"
  defaultMain $ testGroup "All Tests"
    [ -- test_FingerTree
   -- , test_FingerTreeZip
   -- , test_Seq
   -- , test_RunLengthEncoding
   -- , test_ReflectNat
   -- , test_ScreenOffset
   -- , test_MeasureText
   -- , test_Buffer
   --   test_TextBox

      test_Type
    , test_Module
    , test_Interpreter
    , test_Parser

  -- panel
  -- tile
  -- tab
    ]
