port module Main exposing
  (main
 )

-- Imports ---------------------------------------------------------------------
import AdventOfCode2015.Day01
-- << INJECT 2015 IMPORT >>

import AdventOfCode2016.Day01
-- << INJECT 2016 IMPORT >>

import AdventOfCode2017.Day01
-- << INJECT 2017 IMPORT >>

import AdventOfCode2018.Day01
-- << INJECT 2018 IMPORT >>

import AdventOfCode2019.Day01
import AdventOfCode2019.Day02
import AdventOfCode2019.Day03
import AdventOfCode2019.Day04
-- << INJECT 2019 IMPORT >>

import Dict exposing (Dict)
import Json.Encode as Encode

-- Ports -----------------------------------------------------------------------
port fromJs : (Question -> msg) -> Sub msg
port fromElm : Solution -> Cmd msg

-- JavaScript solution ---------------------------------------------------------
type alias Solution
  = Encode.Value

toSolution : Result String Int -> Solution
toSolution result =
  case result of
    Ok n ->
      Encode.object
        [ ("status", Encode.string "Ok")
        , ("result", Encode.int n)
        ]

    Err notice ->
      Encode.object
        [ ("status", Encode.string "Error")
        , ("notice", Encode.string notice)
        ]

-- Main ------------------------------------------------------------------------
main : Program () () Question
main =
  Platform.worker
    { init = always ((), Cmd.none)
    , update = \q _ -> update q |> Cmd.map never |> Tuple.pair ()
    , subscriptions = subscriptions
    }

type alias Question =
  { day : Int
  , part : Int
  , year : Int
  , input : String
  }

answerMap : Dict (Int, Int, Int) (String -> Result String Int)
answerMap =
  Dict.fromList
    [ Tuple.pair (1, 1, 2015) AdventOfCode2015.Day01.part1
    , Tuple.pair (1, 2, 2015) AdventOfCode2015.Day01.part2
    -- << INJECT 2015 SOLUTION >>
    , Tuple.pair (1, 1, 2016) AdventOfCode2016.Day01.part1
    , Tuple.pair (1, 2, 2016) AdventOfCode2016.Day01.part2
    -- << INJECT 2016 SOLUTION >>
    , Tuple.pair (1, 1, 2017) AdventOfCode2017.Day01.part1
    , Tuple.pair (1, 2, 2017) AdventOfCode2017.Day01.part2
    -- << INJECT 2017 SOLUTION >>
    , Tuple.pair (1, 1, 2018) AdventOfCode2018.Day01.part1
    , Tuple.pair (1, 2, 2018) AdventOfCode2018.Day01.part2
    -- << INJECT 2018 SOLUTION >>
    , Tuple.pair (1, 1, 2019) AdventOfCode2019.Day01.part1
    , Tuple.pair (1, 2, 2019) AdventOfCode2019.Day01.part2
    , Tuple.pair (2, 1, 2019) AdventOfCode2019.Day02.part1
    , Tuple.pair (2, 2, 2019) AdventOfCode2019.Day02.part2
    , Tuple.pair (3, 1, 2019) AdventOfCode2019.Day03.part1
    , Tuple.pair (3, 2, 2019) AdventOfCode2019.Day03.part2
    , Tuple.pair (4, 1, 2019) AdventOfCode2019.Day04.part1
    , Tuple.pair (4, 2, 2019) AdventOfCode2019.Day04.part2
    ] -- << INJECT 2019 SOLUTION >>

-- Update ----------------------------------------------------------------------
update : Question -> Cmd Never
update { day, part, year, input } =
  let
    errorMessage = 
      "I don't have a solution for day " ++ String.fromInt day
        ++ " part " ++ String.fromInt part
        ++ " year " ++ String.fromInt year
        ++ "!"
  in
  Dict.get (day, part, year) answerMap
    |> Result.fromMaybe errorMessage
    |> Result.andThen ((|>) input)
    |> toSolution
    |> fromElm

-- Subscriptions ---------------------------------------------------------------
subscriptions : () -> Sub Question
subscriptions _ =
  fromJs identity