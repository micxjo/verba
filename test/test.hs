{-# LANGUAGE OverloadedStrings #-}

import NLP.Verba

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests = testGroup "Verba Tests" [parseTests]

parseTests = testGroup "Parse Tests" [
  testCase "Inflection File Parse" $ do
      Right inflections <- readInflectionsFile
      length inflections @?= 1785
  ,testCase "Dict File Parse" $ do
      Right dict <- readDictEntriesFile
      length dict @?= 39337]
