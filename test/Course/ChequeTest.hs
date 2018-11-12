{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.ChequeTest where

import Control.Monad ((>>), return)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Course.Cheque
import Course.Core
import Course.List (List(..), listh)
import Course.Optional

test_Cheque :: TestTree
test_Cheque =
  testGroup "Cheque" [chequeDollarsTest, cleanDollarsTest, toDigitsTest]

runTestCheque :: IO ()
runTestCheque = defaultMain test_Cheque

cleanDollarsTest :: TestTree
cleanDollarsTest =
  testGroup
    "cleanDollars"
    [ testCase "16000000456.13" $
      cleanDollars "16000000456.13" @?= "16000000456.13"
    , testCase "9abc9def9ghi.jkl9mno" $
      cleanDollars "9abc9def9ghi.jkl9mno" @?= "999.9"
    , testCase "9abc9def9ghi.jkl9 . 9mno" $
      cleanDollars "9abc9def9ghi.jkl9 . 9mno" @?= "999.99"
    ]

toDigitsTest :: TestTree
toDigitsTest =
  testGroup
    "toDigitsTest"
    [ testCase "123.45" $
      toDigits "123.45" @?=
      Full (One :. Two :. Three :. Nil, Four :. Five :. Nil)
    , testCase "0.34567889" $
      toDigits "0.3456789" @?=
      Full (Zero :. Nil, Three :. Four :. Nil)
    , testCase "toDigit3 [One, Zero, Two, Four]" $
      toDigit3 (One :. Zero :. Two :. Four :. Nil) @?=
      (D1 One :. D3 Zero Two Four :. Nil)
    , testCase "wrapIllions (D1 One :. D3 Zero Two Four :. Nil)" $
      wrapIllions (listh [D1 One, D3 Zero Two Four]) @?=
      listh [(D1 One, "thousand"), (D3 Zero Two Four, "")]
    , testCase "illionToString (D3 One Zero Two, \"thousand\")" $
      illionToString (D3 One Zero Two, "thousand") @?= "one hundred and two thousand"
    , testCase "illionToString (D1 One, \"\")" $
      illionToString (D1 One, Nil) @?= "one"

    , testCase "showDigit3" $ do
        showDigit3 (D3 One Two Three) @?= "one hundred and twenty-three"
        showDigit3 (D3 Zero Two Three) @?= "twenty-three"
        showDigit3 (D3 One Zero Three) @?= "one hundred and three"
        showDigit3 (D3 Three Zero Three) @?= "three hundred and three"
        showDigit3 (D2 Zero Three) @?= "three"
    ]

chequeDollarsTest :: TestTree
chequeDollarsTest =
  testGroup
    "jsonObject"
    [ testCase "empty" $ dollars "0" @?= "zero dollars and zero cents"
    , testCase "dollars '1'" $ dollars "1" @?= "one dollar and zero cents"
    , testCase "dollars '0.1'" $ dollars "0.1" @?= "zero dollars and ten cents"
    , testCase "dollars '1.'" $ dollars "1." @?= "one dollar and zero cents"
    , testCase "dollars '0.'" $ dollars "0." @?= "zero dollars and zero cents"
    , testCase "dollars '0.0'" $ dollars "0.0" @?= "zero dollars and zero cents"
    , testCase "dollars '.34'" $
      dollars ".34" @?= "zero dollars and thirty-four cents"
    , testCase "dollars '0.3456789'" $
      dollars "0.3456789" @?= "zero dollars and thirty-four cents"
    , testCase "dollars '1.0'" $ dollars "1.0" @?= "one dollar and zero cents"
    , testCase "dollars '1.01'" $ dollars "1.01" @?= "one dollar and one cent"
    , testCase "dollars 'a1a'" $ dollars "a1a" @?= "one dollar and zero cents"
    , testCase "dollars 'a1a.a0.7b'" $
      dollars "a1a.a0.7b" @?= "one dollar and seven cents"
    , testCase "dollars '100'" $
      dollars "100" @?= "one hundred dollars and zero cents"
    , testCase "dollars '100.0'" $
      dollars "100.0" @?= "one hundred dollars and zero cents"
    , testCase "dollars '100.00'" $
      dollars "100.00" @?= "one hundred dollars and zero cents"
    , testCase "dollars '100.00000'" $
      dollars "100.00000" @?= "one hundred dollars and zero cents"
    , testCase "dollars '1000456.13'" $
      dollars "1000456.13" @?=
      "one million four hundred and fifty-six dollars and thirteen cents"
    , testCase "dollars '1001456.13'" $
      dollars "1001456.13" @?=
      "one million one thousand four hundred and fifty-six dollars and thirteen cents"
    , testCase "dollars '16000000456.13'" $
      dollars "16000000456.13" @?=
      "sixteen billion four hundred and fifty-six dollars and thirteen cents"
    , testCase "dollars '100.45'" $
      dollars "100.45" @?= "one hundred dollars and forty-five cents"
    , testCase "dollars '100.07'" $
      dollars "100.07" @?= "one hundred dollars and seven cents"
    , testCase "dollars '9abc9def9ghi.jkl9mno'" $
      dollars "9abc9def9ghi.jkl9mno" @?=
      "nine hundred and ninety-nine dollars and ninety cents"
    , testCase "dollars '12345.67'" $
      dollars "12345.67" @?=
      "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
    , testCase
        "dollars '456789123456789012345678901234567890123456789012345678901234567890.12'" $
      dollars
        "456789123456789012345678901234567890123456789012345678901234567890.12" @?=
      "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
    ]
