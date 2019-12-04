module AdventOfCode2019.Day04 exposing (..)

-- Imports ---------------------------------------------------------------------
import Basics.Extra as Basics
import List.Extra as List
import Parser exposing (Parser, Trailing (..), Step (..), (|.), (|=))

-- Types -----------------------------------------------------------------------
type Password
  = Password Int Int Int Int Int Int

type alias Range
  = (Int, Int)

-- Input -----------------------------------------------------------------------
--
parseInput : String -> Result String Range
parseInput input =
  Parser.run inputParser input
    |> Result.mapError Parser.deadEndsToString

--
inputParser : Parser Range
inputParser =
  Parser.succeed Tuple.pair
    |= Parser.int
    |. Parser.symbol "-"
    |= Parser.int
    |. Parser.spaces
    |. Parser.end

--
password : Parser Password
password =
  Parser.succeed Password
    |= digit
    |= digit
    |= digit
    |= digit
    |= digit
    |= digit

--
digit : Parser Int
digit =
  Parser.oneOf
    [ Parser.token "0" |> Parser.map (always 0)
    , Parser.token "1" |> Parser.map (always 1)
    , Parser.token "2" |> Parser.map (always 2)
    , Parser.token "3" |> Parser.map (always 3)
    , Parser.token "4" |> Parser.map (always 4)
    , Parser.token "5" |> Parser.map (always 5)
    , Parser.token "6" |> Parser.map (always 6)
    , Parser.token "7" |> Parser.map (always 7)
    , Parser.token "8" |> Parser.map (always 8)
    , Parser.token "9" |> Parser.map (always 9)
    ]

-- Conversions -----------------------------------------------------------------
--
toList : Password -> List Int
toList (Password a b c d e f) =
  [ a, b, c, d, e, f ]

--
toInt : Password -> Int
toInt (Password a b c d e f) =
  a * 100000 +
  b * 10000 +
  c * 1000 +
  d * 100 +
  e * 10 +
  f

--
fromInt : Int -> Password
fromInt n =
  String.fromInt n
    |> Parser.run password
    |> Result.withDefault (Password 0 0 0 0 0 0)

-- Functions -------------------------------------------------------------------
--
generateValidPasswords : List (Password -> Bool) -> Range -> List Password
generateValidPasswords predicates (lowerBound, upperBound) =
  List.range lowerBound upperBound |> List.filterMap (fromInt >> \pwd ->
    if validate predicates pwd then
      Just pwd
    else
      Nothing
  )

--
validate : List (Password -> Bool) -> Password -> Bool
validate validators pwd =
  List.map ((|>) pwd) validators
    |> List.foldl (&&) True

--
singleDigits : Password -> Bool
singleDigits pwd =
  toList pwd
    |> List.map (\n -> 0 <= n && n <= 9)
    |> List.foldl (&&) True

--
containsAdjacentDuplicate : Password -> Bool
containsAdjacentDuplicate (Password a b c d e f) =
  List.member True
    [ a == b
    , b == c
    , c == d
    , d == e
    , e == f
    ]

--
containsAdjacentPairs : Password -> Bool
containsAdjacentPairs (Password a b c d e f) =
  List.member True
    [ a == b && b /= c
    , a /= b && b == c && c /= d
    , b /= c && c == d && d /= e
    , c /= d && d == e && e /= f
    , d /= e && e == f
    ]

--
alwaysAscending : Password -> Bool
alwaysAscending pwd =
  toList pwd
    |> List.sort
    |> (==) (toList pwd)

-- Solvers ---------------------------------------------------------------------
part1 : String -> Result String Int
part1 input =
  let
    generator =
      generateValidPasswords
        [ singleDigits
        , alwaysAscending
        , containsAdjacentDuplicate
        ]
  in
  parseInput input
    |> Result.map (generator >> List.length)

part2 : String -> Result String Int
part2 input =
  let
    generator =
      generateValidPasswords
        [ singleDigits
        , alwaysAscending
        , containsAdjacentPairs
        ]
  in
  parseInput input
    |> Result.map (generator >> List.length)