module AdventOfCode2019.Day01 exposing (part1, part2)

-- Imports ---------------------------------------------------------------------
import Basics.Extra as Basics
import Parser exposing (Parser, Trailing (..), Step (..), (|.), (|=))

-- Types -----------------------------------------------------------------------
type alias Mass
  = Int

type alias FuelCost
  = Int

-- Input -----------------------------------------------------------------------
--
parseInput : String -> Result String (List Mass)
parseInput input =
  Parser.run inputParser input
    |> Result.mapError Parser.deadEndsToString

--
inputParser : Parser (List Mass)
inputParser =
  Parser.loop [] <| (\xs ->
    Parser.oneOf
      [ Parser.succeed (\x -> Loop (x :: xs))
          |= Parser.int
          |. Parser.spaces
      , Parser.succeed (\_ -> Done (List.reverse xs))
          |= Parser.end
      ]
 )


-- Functions -------------------------------------------------------------------
-- Fuel required to launch a given module is based on its mass. Specifically,
-- to find the fuel required for a module, take its mass, divide by three,
--round down, and subtract 2.
calculateFuelCost : Mass -> FuelCost
calculateFuelCost mass =
  mass // 3 - 2

-- Fuel itself requires fuel just like a module - take its mass, divide by three,
-- round down, and subtract 2. However, that fuel also requires fuel, and that 
-- fuel requires fuel, and so on. Any mass that would require negative fuel 
-- should instead be treated as if it requires zero fuel; the remaining mass,
-- if any, is instead handled by wishing really hard, which has no mass and is
--outside the scope of this calculation.
calculateTotalFuelCost : Mass -> FuelCost
calculateTotalFuelCost mass =
  calculateFuelCost mass |> (\cost -> 
    if cost <= 6 then 
      cost 
    else 
      cost + calculateTotalFuelCost cost
 )

-- Solvers ---------------------------------------------------------------------
part1 : String -> Result String Int
part1 input =
  parseInput input
    |> Result.map (List.map calculateFuelCost >>List.sum)

part2 : String -> Result String Int
part2 input =
  parseInput input
    |> Result.map (List.map calculateTotalFuelCost >> List.sum)