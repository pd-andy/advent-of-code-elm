module AdventOfCode2019.Day03 exposing
  (..
 )

-- Imports ---------------------------------------------------------------------
import Basics.Extra as Basics
import Dict exposing (Dict)
import List.Extra as List
import Parser exposing (Parser, Trailing (..), Step (..), (|.), (|=))
import Set exposing (Set)

-- Types -----------------------------------------------------------------------
type Command
  = Up Int
  | Down Int
  | Left Int
  | Right Int

type alias Coordinate =
  (Int, Int)

type alias Wire =
  Dict Coordinate Int

-- Input -----------------------------------------------------------------------
commandParser : Parser Command
commandParser =
  Parser.oneOf
    [ Parser.succeed Up
        |. Parser.token "U"
        |= Parser.int
    , Parser.succeed Down
        |. Parser.token "D"
        |= Parser.int
    , Parser.succeed Left
        |. Parser.token "L"
        |= Parser.int
    , Parser.succeed Right
        |. Parser.token "R"
        |= Parser.int
    ]

commandListParser : Parser (List Command)
commandListParser =
  Parser.sequence
    { start = ""
    , separator = ","
    , end = ""
    , spaces = Parser.succeed ()
    , item = commandParser
    , trailing = Forbidden
    }

inputParser : Parser (List (List Command))
inputParser =
  Parser.sequence
    { start = ""
    , separator = "\n"
    , end = ""
    , spaces = Parser.succeed ()
    , item = commandListParser
    , trailing = Optional
    }

parseInput : String -> List (List Command)
parseInput input =
  Parser.run inputParser input
    |> Result.withDefault []
    |> List.filter (List.length >> (/=) 0)


-- Functions -------------------------------------------------------------------
--
up : (Coordinate, Int) -> Wire -> Wire
up ((x, y), length) wire =
  Dict.insert (x, y + 1) (length + 1) wire

--
down : (Coordinate, Int) -> Wire -> Wire
down ((x, y), length) wire =
  Dict.insert (x, y - 1) (length + 1) wire

--
left : (Coordinate, Int) -> Wire -> Wire
left ((x, y), length) wire =
  Dict.insert (x - 1, y) (length + 1) wire

--
right : (Coordinate, Int) -> Wire -> Wire
right ((x, y), length) wire =
  Dict.insert (x + 1, y) (length + 1) wire

--
move : Command -> Coordinate -> Wire -> (Coordinate, Wire)
move command ((x,y) as coordinate) wire =
  let
    length =
      Dict.get coordinate wire 
        |> Maybe.withDefault 0
  in
  case command of
    Up steps ->
      List.repeat steps coordinate
        |> List.indexedMap (\i (_, y1) -> up ((x, y1 + i), length + i))
        |> List.foldl (<|) wire
        |> Tuple.pair (x, y + steps)

    Down steps ->
      List.repeat steps coordinate
        |> List.indexedMap (\i (_, y1) -> down ((x, y1 - i), length + i))
        |> List.foldl (<|) wire
        |> Tuple.pair (x, y - steps)

    Left steps ->
      List.repeat steps coordinate
        |> List.indexedMap (\i (x1, _) -> left ((x1 - i, y), length + i))
        |> List.foldl (<|) wire
        |> Tuple.pair (x - steps, y)

    Right steps ->
      List.repeat steps coordinate
        |> List.indexedMap (\i (x1, _) -> right ((x1 + i, y), length + i))
        |> List.foldl (<|) wire
        |> Tuple.pair (x + steps, y)

--
run : List Command -> Wire
run commands =
  let
    step : List Command -> (Coordinate, Wire) -> Wire
    step cmds (coordinate, wire) =
      case cmds of
        command :: rest ->
          step rest (move command coordinate wire)

        [] ->
          wire
  in
  step commands ((0, 0), Dict.empty)

--
distance : Coordinate -> Coordinate -> Int
distance (x1, y1) (x2, y2) =
  abs x1 + abs x2 + abs y1 + abs y2

--
totalLengths : Dict Coordinate Int -> Dict Coordinate Int -> Dict Coordinate Int
totalLengths a b =
  let
    keys : List Coordinate
    keys = 
      Set.intersect (Dict.keys a |> Set.fromList) (Dict.keys b |> Set.fromList)
        |> Set.toList

    unsafeGet : Coordinate -> Dict Coordinate Int -> Int
    unsafeGet k dict =
      Dict.get k dict
        |> Maybe.withDefault 0

    sum : Coordinate -> Dict Coordinate Int -> Dict Coordinate Int
    sum k dict =
      Dict.insert k (unsafeGet k a + unsafeGet k b) dict
  in
  List.foldl sum Dict.empty keys

--
commonElements : List (List comparable) -> List comparable
commonElements input =
  case input of
    head :: tail ->
      List.foldl Set.intersect (Set.fromList head) (List.map Set.fromList tail)
        |> Set.toList

    [] ->
      []

-- Solvers ---------------------------------------------------------------------
part1 : String -> Result String Int
part1 input =
  parseInput input
    |> List.map (run >> Dict.keys)
    |> commonElements
    |> List.map (distance (0, 0))
    |> List.minimum
    |> Maybe.withDefault 0
    |> Ok

part2 : String -> Result String Int
part2 input =
  parseInput input
    |> List.map run
    |> List.uncons
    |> Maybe.map (Basics.uncurry <| List.foldl totalLengths)
    |> Maybe.andThen (Dict.values >> List.minimum)
    |> Maybe.withDefault 0
    |> Ok