{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}
module Course.Cheque where

import Course.Applicative
import Course.Core
import Course.Functor
import Course.List
import Course.Monad
import Course.Optional
import Course.Parser (Input, ParseResult(..), Parser(..), parse)
import Course.State (State, eval, get, put)

(|>) :: a -> (a -> b) -> b
a |> f = f a

infixl 0 |>

-- $setup
-- >>> :set -XOverloadedStrings
-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion :: List Chars
illion =
  let preillion :: List (Chars -> Chars)
      preillion =
        listh
          [ const ""
          , const "un"
          , const "do"
          , const "tre"
          , const "quattuor"
          , const "quin"
          , const "sex"
          , const "septen"
          , const "octo"
          , \q ->
              if "n" `isPrefixOf` q
                then "novem"
                else "noven"
          ]
      postillion :: List Chars
      postillion =
        listh
          [ "vigintillion"
          , "trigintillion"
          , "quadragintillion"
          , "quinquagintillion"
          , "sexagintillion"
          , "septuagintillion"
          , "octogintillion"
          , "nonagintillion"
          , "centillion"
          , "decicentillion"
          , "viginticentillion"
          , "trigintacentillion"
          , "quadragintacentillion"
          , "quinquagintacentillion"
          , "sexagintacentillion"
          , "septuagintacentillion"
          , "octogintacentillion"
          , "nonagintacentillion"
          , "ducentillion"
          , "deciducentillion"
          , "vigintiducentillion"
          , "trigintaducentillion"
          , "quadragintaducentillion"
          , "quinquagintaducentillion"
          , "sexagintaducentillion"
          , "septuagintaducentillion"
          , "octogintaducentillion"
          , "nonagintaducentillion"
          , "trecentillion"
          , "decitrecentillion"
          , "vigintitrecentillion"
          , "trigintatrecentillion"
          , "quadragintatrecentillion"
          , "quinquagintatrecentillion"
          , "sexagintatrecentillion"
          , "septuagintatrecentillion"
          , "octogintatrecentillion"
          , "nonagintatrecentillion"
          , "quadringentillion"
          , "deciquadringentillion"
          , "vigintiquadringentillion"
          , "trigintaquadringentillion"
          , "quadragintaquadringentillion"
          , "quinquagintaquadringentillion"
          , "sexagintaquadringentillion"
          , "septuagintaquadringentillion"
          , "octogintaquadringentillion"
          , "nonagintaquadringentillion"
          , "quingentillion"
          , "deciquingentillion"
          , "vigintiquingentillion"
          , "trigintaquingentillion"
          , "quadragintaquingentillion"
          , "quinquagintaquingentillion"
          , "sexagintaquingentillion"
          , "septuagintaquingentillion"
          , "octogintaquingentillion"
          , "nonagintaquingentillion"
          , "sescentillion"
          , "decisescentillion"
          , "vigintisescentillion"
          , "trigintasescentillion"
          , "quadragintasescentillion"
          , "quinquagintasescentillion"
          , "sexagintasescentillion"
          , "septuagintasescentillion"
          , "octogintasescentillion"
          , "nonagintasescentillion"
          , "septingentillion"
          , "deciseptingentillion"
          , "vigintiseptingentillion"
          , "trigintaseptingentillion"
          , "quadragintaseptingentillion"
          , "quinquagintaseptingentillion"
          , "sexagintaseptingentillion"
          , "septuagintaseptingentillion"
          , "octogintaseptingentillion"
          , "nonagintaseptingentillion"
          , "octingentillion"
          , "decioctingentillion"
          , "vigintioctingentillion"
          , "trigintaoctingentillion"
          , "quadragintaoctingentillion"
          , "quinquagintaoctingentillion"
          , "sexagintaoctingentillion"
          , "septuagintaoctingentillion"
          , "octogintaoctingentillion"
          , "nonagintaoctingentillion"
          , "nongentillion"
          , "decinongentillion"
          , "vigintinongentillion"
          , "trigintanongentillion"
          , "quadragintanongentillion"
          , "quinquagintanongentillion"
          , "sexagintanongentillion"
          , "septuagintanongentillion"
          , "octogintanongentillion"
          , "nonagintanongentillion"
          ]
   in listh
        [ ""
        , "thousand"
        , "million"
        , "billion"
        , "trillion"
        , "quadrillion"
        , "quintillion"
        , "sextillion"
        , "septillion"
        , "octillion"
        , "nonillion"
        , "decillion"
        , "undecillion"
        , "duodecillion"
        , "tredecillion"
        , "quattuordecillion"
        , "quindecillion"
        , "sexdecillion"
        , "septendecillion"
        , "octodecillion"
        , "novemdecillion"
        ] ++
      lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit
  = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Show, Ord)

showDigit :: Digit -> Chars
showDigit Zero = "zero"
showDigit One = "one"
showDigit Two = "two"
showDigit Three = "three"
showDigit Four = "four"
showDigit Five = "five"
showDigit Six = "six"
showDigit Seven = "seven"
showDigit Eight = "eight"
showDigit Nine = "nine"

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3
  = D1 Digit
  | D2 Digit
       Digit
  | D3 Digit
       Digit
       Digit
  deriving (Show, Eq)

-- Possibly convert a character to a digit.
fromChar :: Char -> Optional Digit
fromChar '0' = Full Zero
fromChar '1' = Full One
fromChar '2' = Full Two
fromChar '3' = Full Three
fromChar '4' = Full Four
fromChar '5' = Full Five
fromChar '6' = Full Six
fromChar '7' = Full Seven
fromChar '8' = Full Eight
fromChar '9' = Full Nine
fromChar _ = Empty

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
dollars :: Chars -> Chars
dollars xs =
  xs |> cleanDollars |> toDigits |> \case
    Full (_dollars, _cents) ->
      let dollarsText =
            _dollars |> toDigit3 |> wrapIllions |> map illionToString |> paste
          centsText = _cents |> normalizeCents |> toDigit3 |> flatMap showDigit3
          dollarsUnit =
            case _dollars of
              Nil -> "zero dollars and "
              (One :. Nil) -> " dollar and "
              _ -> " dollars and "
          centsUnit =
            case _cents of
              (Zero :. One :. Nil) -> " cent"
              _ -> " cents"
       in dollarsText ++ dollarsUnit ++ centsText ++ centsUnit
    Empty -> ""

normalizeCents :: List Digit -> List Digit
normalizeCents Nil = Zero :. Nil
normalizeCents (x :. Nil) = x :. Zero :. Nil
normalizeCents xs = xs

illionToString :: (Digit3, Chars) -> Chars
illionToString (D3 Zero Zero Zero, _) = ""
illionToString (d3, unit) = paste $ listh [showDigit3 d3, unit]

showDigit3 :: Digit3 -> Chars
showDigit3 d =
  let showd x = toLower <$> showDigit x
      x .++. y =
        x ++
        if y == Zero
          then Nil
          else '-' :. showd y
   in case d of
        D1 a -> showd a
        D2 Zero b -> showd b
        D2 One b ->
          case b of
            Zero -> "ten"
            One -> "eleven"
            Two -> "twelve"
            Three -> "thirteen"
            Four -> "fourteen"
            Five -> "fifteen"
            Six -> "sixteen"
            Seven -> "seventeen"
            Eight -> "eighteen"
            Nine -> "nineteen"
        D2 Two b -> "twenty" .++. b
        D2 Three b -> "thirty" .++. b
        D2 Four b -> "forty" .++. b
        D2 Five b -> "fifty" .++. b
        D2 Six b -> "sixty" .++. b
        D2 Seven b -> "seventy" .++. b
        D2 Eight b -> "eighty" .++. b
        D2 Nine b -> "ninety" .++. b
        D3 Zero Zero Zero -> ""
        D3 Zero b c -> showDigit3 (D2 b c)
        D3 a Zero Zero -> showd a ++ " hundred"
        D3 a b c -> showd a ++ " hundred and " ++ showDigit3 (D2 b c)

paste :: List Chars -> Chars
paste =
  foldRight
    (\value acc ->
       case (value, acc) of
         (Nil, Nil) -> Nil
         (Nil, _acc) -> _acc
         (v, Nil) -> v
         (v, a) -> v ++ " " ++ a)
    ""

cleanDollars :: Chars -> Chars
cleanDollars text =
  case parse dollarParser text of
    Result _ chars -> chars
    _ -> ""

-- | Returns (Paper bills, Cents)
-- >>> toDigits "123.45"
-- ([One, Two, Three], [Four, Five])
toDigits :: Chars -> Optional (List Digit, List Digit)
toDigits xs =
  let (_fst, _snd) = split' (== '.') xs
   in case (sequence (map fromChar _fst), sequence (map fromChar (take 2 _snd))) of
        (Full xxs, Full yys) -> pure (xxs, yys)
        _ -> Empty

toDigit3rev :: List Digit -> List Digit3
toDigit3rev Nil = Nil
toDigit3rev (x1 :. x2 :. x3 :. rest) = D3 x3 x2 x1 :. toDigit3rev rest
toDigit3rev (x1 :. x2 :. rest) = D2 x2 x1 :. toDigit3rev rest
toDigit3rev (x1 :. rest) = D1 x1 :. toDigit3rev rest

toDigit3 :: List Digit -> List Digit3
toDigit3 = reverse . toDigit3rev . reverse

wrapIllions :: List Digit3 -> List (Digit3, Chars)
wrapIllions xs =
  let reversed = reverse xs
   in reverse $ zip reversed illion

split' :: (a -> Bool) -> List a -> (List a, List a)
split' = go False (Nil, Nil)
  where
    go _ acc _ Nil = acc
    go _ acc p (x :. xs)
      | p x = go True acc p xs
    go True (_fst, _snd) p (x :. xs) = go True (_fst, _snd ++ x :. Nil) p xs
    go False (_fst, _snd) p (x :. xs) = go False (_fst ++ x :. Nil, _snd) p xs

dollarParser :: Parser Chars
dollarParser =
  P (\case
       Nil -> UnexpectedEof
       xs -> eval (go "" xs) False)
  where
    go :: Chars -> Input -> State Bool (ParseResult Chars)
    go acc Nil = return $ Result Nil acc
    go acc (x :. xs)
      | isDigit x = go (acc ++ x :. Nil) xs
    go acc (x :. xs)
      | x == '.' =
        get >>= \seenDot ->
          if seenDot
            then go acc xs
            else put True >> go (acc ++ x :. Nil) xs
    go acc (_ :. xs) = go acc xs
