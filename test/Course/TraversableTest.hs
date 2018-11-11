{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.TraversableTest
  ( test_Traversable
  ) where

import Control.Monad ((>>), return)
import qualified Prelude as P (String, (++))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.QuickCheck (testProperty)

import Course.Applicative ((<*>), pure)
import Course.Compose (Compose(Compose))
import Course.Core
import Course.ExactlyOne (ExactlyOne(..))
import Course.Functor ((<$>))
import Course.Gens (forAllLists)
import Course.List (List(..), flatMap, listh)
import Course.Monad ((=<<), (>>=))
import Course.Optional (Optional(..))
import Course.State (put, runState)
import Course.Traversable (Coproduct(..), Product(Product), sequenceA, traverse)

test_Traversable :: TestTree
test_Traversable =
  testGroup "Traversable" [testInstance, testProduct, testCoProduct]

testInstance :: TestTree
testInstance =
  testGroup
    "Instance"
    [ testCase "Traversable List" $
      let xs = 1 :. 10 :. Nil
       in traverse
            (\x ->
               if x < 10
                 then Full x
                 else Empty)
            xs @?=
          Empty
    , testCase "Traversable ExactlyOne" $
      traverse Full (ExactlyOne 10) @?= Full (ExactlyOne 10)
    , testCase "Traversable Optional" $
      traverse (:. Nil) (Full 10) @?= Full 10 :. Nil
    , testCase "Traversable Compose f g" $
      let compose :: Compose List Optional Int
          compose = Compose (Full 3 :. Nil)
       in traverse Full compose @?= Full compose
    , testCase "sequenceA" $ do
        sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil) @?=
          ExactlyOne (7 :. 8 :. 9 :. Nil)
        sequenceA (Full (ExactlyOne 7)) @?= ExactlyOne (Full 7)
        sequenceA (Full (* 10)) 6 @?= Full 60
    ]

testProduct :: TestTree
testProduct =
  testGroup
    "Product"
    [ testCase "<$>" $
      let product :: Product Optional Optional Int = Product (Full 3) Empty
       in (+ 1) <$> product @?= Product (Full 4) Empty
    , testCase "Traversable Product" $
      let product :: Product Optional List Int =
            Product (Full 10) (1 :. 2 :. Nil)
       in traverse (:. Nil) product @?= Product (Full 10) (1 :. 2 :. Nil) :. Nil
    ]

testCoProduct :: TestTree
testCoProduct =
  testGroup
    "CoProduct"
    [ testCase "<$>" $
      let coproduct :: Coproduct Optional List Int = InL (Full 10)
       in (+ 1) <$> coproduct @?= InL (Full 11)
    , testCase "traverse" $
      let coproduct :: Coproduct Optional List Int = InR (10 :. Nil)
       in traverse (:. Nil) coproduct @?= InR (10 :. Nil) :. Nil
    ]
