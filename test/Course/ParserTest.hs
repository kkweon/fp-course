module Course.ParserTest
  ( test_Parser
  , runTestParser
  ) where

import Control.Monad ((>>), return)
import Course.Applicative ((*>), (<*>), pure)
import Course.Core
import Course.Functor (Functor, (<$>))
import Course.Gens (genList, shrinkList)
import Course.List
import Course.Monad ((=<<))
import Course.Parser
import Course.Person (Person(Person))
import Data.Char (Char, toUpper)
import Data.String (fromString)
import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.QuickCheck as QC hiding (shrinkList)

test_Parser :: TestTree
test_Parser = testGroup "ParserTest" [propTests, unitTests]

runTestParser :: IO ()
runTestParser = defaultMain test_Parser

propTests :: TestTree
propTests =
  testGroup
    "Prop Test"
    [ QC.testProperty "parse character chars" $
      forAllShrink (genList :: Gen (List Char)) shrinkList $ \chars ->
        case chars of
          (x :. xs) -> parse character chars == Result xs x
          _ -> parse character chars == UnexpectedEof
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Test"
    [ testCase "Parser is a functor" $
      parse (toUpper <$> character) "amz" @?= Result "mz" 'A'
    , testCase "valueParser" $ parse (valueParser 3) "abc" @?= Result "abc" 3
    , testCase "|||" $ do
        parse (character ||| valueParser 'v') "" @?= Result "" 'v'
        parse (constantParser UnexpectedEof ||| valueParser 'v') "" @?=
          Result "" 'v'
        parse (character ||| valueParser 'v') "abc" @?= Result "bc" 'a'
        parse (constantParser UnexpectedEof ||| valueParser 'v') "abc" @?=
          Result "abc" 'v'
    , testCase "=<<" $ do
        parse
          ((\c ->
              if c == 'x'
                then character
                else valueParser 'v') =<<
           character)
          "abc" @?=
          Result "bc" 'v'
        parse
          ((\c ->
              if c == 'x'
                then character
                else valueParser 'v') =<<
           character)
          "a" @?=
          Result "" 'v'
        parse
          ((\c ->
              if c == 'x'
                then character
                else valueParser 'v') =<<
           character)
          "xabc" @?=
          Result "bc" 'a'
        isErrorResult
          (parse
             ((\c ->
                 if c == 'x'
                   then character
                   else valueParser 'v') =<<
              character)
             "") @?=
          True
        isErrorResult
          (parse
             ((\c ->
                 if c == 'x'
                   then character
                   else valueParser 'v') =<<
              character)
             "x") @?=
          True
    , testCase "<*>" $
      let pf = pure (+ 1)
          pa = valueParser (3 :: Integer)
       in parse (pf <*> pa) "hashdf" @?= Result "hashdf" 4
    , testCase "list" $ do
        parse (list character) "" @?= Result "" ""
        parse (list digit) "123abc" @?= Result "abc" "123"
        parse (list digit) "abc" @?= Result "abc" ""
        parse (list character) "abc" @?= Result "" "abc"
        parse (list (character *> valueParser 'v')) "abc" @?= Result "" "vvv"
        parse (list (character *> valueParser 'v')) "" @?= Result "" ""
    , testCase "sequenceParser" $ do
        parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "axCdef" @?=
          Result "def" "axC"
        isErrorResult
          (parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "abCdef") @?=
          True
    , testCase "thisMany" $ do
        parse (thisMany 4 upper) "ABCDef" @?= Result "ef" "ABCD"
        isErrorResult (parse (thisMany 4 upper) "ABcDef") @?= True
    , testCase "ageParser" $ do
        parse ageParser "120" @?= Result "" 120
        isErrorResult (parse ageParser "abc") @?= True
        isErrorResult (parse ageParser "-120") @?= True
    , testCase "firstNameParser" $ do
        parse firstNameParser "Abc" @?= Result "" "Abc"
        isErrorResult (parse firstNameParser "abc") @?= True
    , testCase "surnameParser" $ do
        parse surnameParser "Abcdef" @?= Result "" "Abcdef"
        parse surnameParser "Abcdefghijklmnopqrstuvwxyz" @?= Result "" "Abcdefghijklmnopqrstuvwxyz"
        isErrorResult (parse surnameParser "Abc") @?= True
        isErrorResult (parse surnameParser "abc") @?= True
    , testCase "smokerParser" $ do
        parse smokerParser "yabc" @?= Result "abc" True
        parse smokerParser "nabc" @?= Result "abc" False
        isErrorResult (parse smokerParser "abc") @?= True
    , testCase "phoneBodyParser" $ do
        parse phoneBodyParser "123-456" @?= Result "" "123-456"
        parse phoneBodyParser "123-4a56" @?= Result "a56" "123-4"
        parse phoneBodyParser "a123-456" @?= Result "a123-456" ""
    , testCase "phoneParser" $ do
        parse phoneParser "123-456#" @?= Result "" "123-456"
        parse phoneParser "123-456#abc" @?= Result "abc" "123-456"
        isErrorResult (parse phoneParser "123-456") @?= True
        isErrorResult (parse phoneParser "a123-456") @?= True
        isErrorResult (parse phoneParser "-123-456.789#") @?= True
    , testCase "personParser" $ do
        isErrorResult (parse personParser "") @?= True
        isErrorResult (parse personParser "12x Fred Clarkson y 123-456.789#") @?= True
        isErrorResult (parse personParser "123 fred Clarkson y 123-456.789#") @?= True
        isErrorResult (parse personParser "123 Fred Cla y 123-456.789#") @?= True
        isErrorResult (parse personParser "123 Fred clarkson y 123-456.789#") @?= True
        isErrorResult (parse personParser "123 Fred Clarkson x 123-456.789#") @?= True
        isErrorResult (parse personParser "123 Fred Clarkson y 1x3-456.789#") @?= True
        isErrorResult (parse personParser "123 Fred Clarkson y -123-456.789#") @?= True
        isErrorResult (parse personParser "123 Fred Clarkson y 123-456.789") @?= True
        parse personParser "123 Fred Clarkson y 123-456.789#" @?= Result "" (Person 123 "Fred" "Clarkson" True "123-456.789")
        parse personParser "123 Fred Clarkson y 123-456.789# rest" @?= Result " rest" (Person 123 "Fred" "Clarkson" True "123-456.789")
        parse personParser "123  Fred   Clarkson    y     123-456.789#" @?= Result "" (Person 123 "Fred" "Clarkson" True "123-456.789")
    ]
