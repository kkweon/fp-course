module Course.MoreParserTest where

import Control.Monad ((>>), return)
import Course.Core
import Course.List (List((:.), Nil))
import Course.MoreParser
import Course.Parser
import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase)

runTestMoreParser :: IO ()
runTestMoreParser = defaultMain test_moreParser

test_moreParser :: TestTree
test_moreParser = testGroup "MoreParser" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "UnitTest"
    [ testCase "spaces" $ do
        parse spaces " abc" @?= Result "abc" " "
        parse spaces "abc" @?= Result "abc" ""
    , testCase "tok" $ do
        parse (tok (is 'a')) "a bc" @?= Result "bc" 'a'
        parse (tok (is 'a')) "abc" @?= Result "bc" 'a'
    , testCase "charTok" $ do
        parse (charTok 'a') "abc" @?= Result "bc" 'a'
        isErrorResult (parse (charTok 'a') "dabc") @?= True
    , testCase "commaTok" $ do
        parse commaTok ",123" @?= Result "123" ','
        isErrorResult (parse commaTok "1,23") @?= True
    , testCase "quote" $ do
        parse quote "'abc" @?= Result "abc" '\''
        parse quote "\"abc" @?= Result "abc" '"'
        isErrorResult (parse quote "abc") @?= True
    , testCase "string" $ do
        parse (string "abc") "abcdef" @?= Result "def" "abc"
        isErrorResult (parse (string "abc") "bcdef") @?= True
    , testCase "stringTok" $ do
        parse (stringTok "abc") "abc  " @?= Result "" "abc"
        isErrorResult (parse (stringTok "abc") "bc  ") @?= True
    , testCase "option" $ do
        parse (option 'x' character) "abc" @?= Result "bc" 'a'
        parse (option 'x' character) "" @?= Result "" 'x'
    , testCase "digits1" $ do
        parse digits1 "123" @?= Result "" "123"
        isErrorResult (parse digits1 "abc123") @?= True
    , testCase "oneof" $ do
        parse (oneof "abc") "bcdef" @?= Result "cdef" 'b'
        isErrorResult (parse (oneof "abc") "def") @?= True
    , testCase "noneof" $ do
        parse (noneof "bcd") "abc" @?= Result "bc" 'a'
        isErrorResult (parse (noneof "abcd") "abc") @?= True
    , testCase "between" $ do
        parse (between (is '[') (is ']') character) "[a]" @?= Result "" 'a'
        isErrorResult (parse (between (is '[') (is ']') character) "[abc]") @?=
          True
        isErrorResult (parse (between (is '[') (is ']') character) "[abc") @?=
          True
        isErrorResult (parse (between (is '[') (is ']') character) "abc]") @?=
          True
    , testCase "betweenCharTok" $ do
        parse (betweenCharTok '[' ']' character) "[a]" @?= Result "" 'a'
        isErrorResult (parse (betweenCharTok '[' ']' character) "[abc]") @?=
          True
        isErrorResult (parse (betweenCharTok '[' ']' character) "[abc") @?= True
        isErrorResult (parse (betweenCharTok '[' ']' character) "abc]") @?= True
    , testCase "hex" $ do
        parse hex "0010" @?= Result "" '\DLE'
        parse hex "0a1f" @?= Result "" '\2591'
        isErrorResult (parse hex "001") @?= True
        isErrorResult (parse hex "0axf") @?= True
    , testCase "hexu" $ do
        parse hexu "u0010" @?= Result "" '\DLE'
        parse hexu "u0a1f" @?= Result "" '\2591'
        isErrorResult (parse hexu "0010") @?= True
        isErrorResult (parse hexu "u001") @?= True
        isErrorResult (parse hexu "u0axf") @?= True
    , testCase "sepby1" $ do
        parse (sepby1 character (is ',')) "a" @?= Result "" "a"
        parse (sepby1 character (is ',')) "a,b,c" @?= Result "" "abc"
        parse (sepby1 character (is ',')) "a,b,c,,def" @?= Result "def" "abc,"
        isErrorResult (parse (sepby1 character (is ',')) "") @?= True
    , testCase "sepby" $ do
        parse (sepby character (is ',')) "" @?= Result "" ""
        parse (sepby character (is ',')) "a" @?= Result "" "a"
        parse (sepby character (is ',')) "a,b,c" @?= Result "" "abc"
        parse (sepby character (is ',')) "a,b,c,,def" @?= Result "def" "abc,"
    , testCase "eof" $ do
        parse eof "" @?= Result "" ()
        isErrorResult (parse eof "abc") @?= True
    , testCase "satisfyAll" $ do
        parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "ABC" @?=
          Result "BC" 'A'
        parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "ABc" @?=
          Result "Bc" 'A'
        isErrorResult (parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "XBc") @?=
          True
        isErrorResult (parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "") @?=
          True
        isErrorResult (parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "abc") @?=
          True
    , testCase "satisfyAny" $ do
        parse (satisfyAny (isLower :. (/= 'X') :. Nil)) "abc" @?=
          Result "bc" 'a'
        parse (satisfyAny (isLower :. (/= 'X') :. Nil)) "ABc" @?=
          Result "Bc" 'A'
        isErrorResult (parse (satisfyAny (isLower :. (/= 'X') :. Nil)) "XBc") @?=
          True
        isErrorResult (parse (satisfyAny (isLower :. (/= 'X') :. Nil)) "") @?=
          True
    , testCase "betweenSepbyComma" $ do
        parse (betweenSepbyComma '[' ']' lower) "[a]" @?= Result "" "a"
        parse (betweenSepbyComma '[' ']' lower) "[]" @?= Result "" ""
        isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[A]") @?= True
        isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[abc]") @?= True
        isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[a") @?= True
        isErrorResult (parse (betweenSepbyComma '[' ']' lower) "a]") @?= True
    ]
