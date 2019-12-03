port module Main exposing
  ( main
  )

-- Imports ---------------------------------------------------------------------
-- import AdventOfCode2018.Day01
-- << INJECT 2018 IMPORT >>

import AdventOfCode2019.Day01
import AdventOfCode2019.Day02
import AdventOfCode2019.Day03
-- << INJECT 2019 IMPORT >>

import Dict exposing ( Dict )
import Utils

-- Ports -----------------------------------------------------------------------
port fromJs : ( Question -> msg ) -> Sub msg
port fromElm : Int -> Cmd msg

-- Main ------------------------------------------------------------------------
main : Program () () Question
main =
  Platform.worker
    { init = always ( Utils.withCmdNone () )
    , update = \q _ -> update q |> Cmd.map never |> Tuple.pair ()
    , subscriptions = subscriptions
    }

type alias Question =
  { day : Int
  , part : Int
  , year : Int
  , input : String
  }

answerMap : Dict ( Int, Int, Int ) ( String -> Int )
answerMap =
  Dict.fromList
    -- [ Tuple.pair ( 1, 1, 2018 ) AdventOfCode2018.Day01.part1
    -- , Tuple.pair ( 1, 2, 2018 ) AdventOfCode2018.Day01.part2
    -- << INJECT 2018 SOLUTION >>
    [ Tuple.pair ( 1, 1, 2019 ) AdventOfCode2019.Day01.part1
    , Tuple.pair ( 1, 2, 2019 ) AdventOfCode2019.Day01.part2
    , Tuple.pair ( 2, 1, 2019 ) AdventOfCode2019.Day02.part1
    , Tuple.pair ( 2, 2, 2019 ) AdventOfCode2019.Day02.part2
    , Tuple.pair ( 3, 1, 2019 ) AdventOfCode2019.Day03.part1
    , Tuple.pair ( 3, 2, 2019 ) AdventOfCode2019.Day03.part2
    ] -- << INJECT 2019 SOLUTION >>

-- Update ----------------------------------------------------------------------
update : Question -> Cmd Never
update { day, part, year, input } =
  Dict.get ( day, part, year ) answerMap
    |> Maybe.map ((|>) input)
    |> Maybe.withDefault 0
    |> fromElm

-- Subscriptions ---------------------------------------------------------------
subscriptions : () -> Sub Question
subscriptions _ =
  fromJs identity