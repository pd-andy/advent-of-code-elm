module AdventOfCode2019.Day02 exposing
  ( part1
  , part2
  )

-- Imports ---------------------------------------------------------------------
import AdventOfCode2019.Day02.Parser as Parser
import AdventOfCode2019.Day02.Program as Program
import Utils

-- Functions -------------------------------------------------------------------
--
calculateFirstInput : Int -> List Program.Instruction -> Program.Memory -> Int
calculateFirstInput target instructions memory =
  let
    helper : Int -> Int
    helper input =
      Program.run instructions memory input 0
        |> \n -> if n > target then input - 1 else helper ( input + 1 )
  in
  helper 0

--
calculateSecondInput : Int -> Int -> List Program.Instruction -> Program.Memory -> Int
calculateSecondInput target firstInput instructions memory =
  let
    helper : Int -> Int
    helper input =
      Program.run instructions memory firstInput input
        |> \n -> if n == target then input else helper ( input + 1 )
  in
  helper 0

-- Solvers ---------------------------------------------------------------------
part1 : String -> Float
part1 input =
  let
    instructions : List Program.Instruction
    instructions = Parser.run input

    memory : Program.Memory
    memory = Utils.parseIntArray "," input

  in
  Program.run instructions memory 12 2
    |> Basics.toFloat

part2 : String -> Float
part2 input =
  let
    target : Int
    target = 19690720

    instructions : List Program.Instruction
    instructions = Parser.run input

    memory : Program.Memory
    memory = Utils.parseIntArray "," input

  in
  calculateFirstInput target instructions memory
    |> Utils.toTuple
    |> Tuple.mapSecond (\n -> calculateSecondInput target n instructions memory)
    |> Utils.sumTuple
    |> (*) 100
    |> Basics.toFloat