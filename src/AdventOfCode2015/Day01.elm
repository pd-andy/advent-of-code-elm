module AdventOfCode2015.Day01 exposing (part1, part2)

-- Imports ---------------------------------------------------------------------
import Basics.Extra as Basics
import Parser exposing (Parser, Trailing (..), Step (..), (|.), (|=))

-- Types -----------------------------------------------------------------------
type alias Floor
  = Int

type alias Position
  = Int

-- Input -----------------------------------------------------------------------
--
parseInput : String -> Result String (Floor, Position)
parseInput input =
  Parser.run inputParser input
    |> Result.mapError Parser.deadEndsToString

--
inputParser : Parser (Floor, Position)
inputParser =
  Parser.loop (0, 0) <| (\(floor, position) ->
    Parser.oneOf
      [ leftParenthesis (floor, position)
      , rightParenthesis (floor, position)
      , Parser.succeed (Done (floor, position))
          |. Parser.end
      ]
 )

leftParenthesis : (Floor, Position) -> Parser (Step (Floor, Position) (Floor, Position))
leftParenthesis (floor, position) =
  Parser.succeed () |. Parser.symbol "(" |. Parser.spaces |> Parser.map (\_ ->
    Loop (floor + 1, position + 1)
  )

rightParenthesis : (Floor, Position) -> Parser (Step (Floor, Position) (Floor, Position))
rightParenthesis (floor, position) =
  Parser.succeed () |. Parser.symbol ")" |. Parser.spaces |> Parser.map (\_ ->
    if floor == 0 then
      Done (floor - 1, position + 1)
    else
      Loop (floor - 1, position + 1)
  )

-- Functions -------------------------------------------------------------------


-- Solvers ---------------------------------------------------------------------
part1 : String -> Result String Int
part1 input =
  parseInput input
    |> Result.map Tuple.first

part2 : String -> Result String Int
part2 input =
  parseInput input
    |> Result.map Tuple.second
