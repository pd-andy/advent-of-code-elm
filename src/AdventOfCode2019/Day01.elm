module AdventOfCode2019.Day01 exposing
  ( part1
  , part2
  )

-- Imports ---------------------------------------------------------------------
import Utils

-- Functions -------------------------------------------------------------------
--
calculateFuel : Int -> Int
calculateFuel =
  Utils.flip (//) 3 >> Utils.flip (-) 2

--
calculateFuelRecursively : Int -> Int
calculateFuelRecursively =
  calculateFuel >> \x -> if x <= 6 then x else x + calculateFuelRecursively x

--
solveWith : ( Int -> Int ) -> String -> Int
solveWith solver input =
  String.lines input
    |> List.filterMap ( String.toInt >> Maybe.map solver )
    |> List.sum

-- Solvers ---------------------------------------------------------------------
part1 : String -> Int
part1 =
 solveWith calculateFuel

part2 : String -> Int
part2 =
  solveWith calculateFuelRecursively