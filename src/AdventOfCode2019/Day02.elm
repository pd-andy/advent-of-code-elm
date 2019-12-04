module AdventOfCode2019.Day02 exposing (part1, part2)

-- Imports ---------------------------------------------------------------------
import AdventOfCode2019.Day02.Parser as Parser
import AdventOfCode2019.Day02.Program as Program

import Array

-- Functions -------------------------------------------------------------------
--
calculateFirstInput : Int -> List Program.Instruction -> Program.Memory -> Int
calculateFirstInput target instructions memory =
  let
    helper : Int -> Int
    helper input =
      Program.run instructions memory input 0
        |> \n -> if n > target then input - 1 else helper (input + 1)
  in
  helper 0

--
calculateSecondInput : Int -> Int -> List Program.Instruction -> Program.Memory -> Int
calculateSecondInput target firstInput instructions memory =
  let
    helper : Int -> Int
    helper input =
      Program.run instructions memory firstInput input
        |> \n -> if n == target then input else helper (input + 1)
  in
  helper 0

-- Solvers ---------------------------------------------------------------------
part1 : String -> Result String Int
part1 input =
  let
    instructions : List Program.Instruction
    instructions = Parser.run input

    memory : Program.Memory
    memory = 
      String.split "," input
        |> List.filterMap String.toInt
        |> Array.fromList

  in
  Program.run instructions memory 12 2
    |> Ok

part2 : String -> Result String Int
part2 input =
  let
    target : Int
    target = 19690720

    instructions : List Program.Instruction
    instructions = Parser.run input

    memory : Program.Memory
    memory =
      String.split "," input
        |> List.filterMap String.toInt
        |> Array.fromList

  in
  calculateFirstInput target instructions memory
    |> (\x -> (x, x))
    |> Tuple.mapSecond (\n -> calculateSecondInput target n instructions memory)
    |> (\(x, y) -> x + y)
    |> (*) 100
    |> Ok