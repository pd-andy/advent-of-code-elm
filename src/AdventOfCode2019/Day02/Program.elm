module AdventOfCode2019.Day02.Program exposing
  (Memory, Address, Opcode (..), Instruction, Program
  , run
 )

-- Imports ---------------------------------------------------------------------
import Array exposing (Array)

-- Types -----------------------------------------------------------------------
type alias Memory
  = Array Int

type alias Address
  = Int

type Opcode
  = Add
  | Multiply
  | EOF

type alias Instruction =
  { opcode : Opcode
  , l_operand : Address
  , r_operand : Address
  , destination : Address
  }

type Program
  = Running (List Instruction) Memory
  | Complete Memory

-- Functions -------------------------------------------------------------------
--
run : List Instruction -> Array Int -> Int -> Int -> Int
run instructions memory noun verb =
  Running instructions (setInputs noun verb memory)
    |> step
    |> return

--
setInputs : Int -> Int -> Memory -> Memory
setInputs noun verb =
  Array.set 1 noun >> Array.set 2 verb

--
return : Program -> Int
return program =
  case program of
    Complete memory ->
      get 0 memory

    Running _ _ ->
      step program |> return

--
step : Program -> Program
step program =
  case program of
    Complete _ ->
      program

    Running instructions memory ->
      executeNextInstruction instructions memory
        |> step

--
executeNextInstruction : List Instruction -> Memory -> Program
executeNextInstruction instructions memory =
  case instructions of
    (({ opcode } as instruction) :: rest) ->
      case opcode of
        EOF -> Complete memory
        _   -> Running rest (executeInstruction instruction memory)

    [] ->
      Complete memory

--
executeInstruction : Instruction -> Memory -> Memory
executeInstruction { opcode, l_operand, r_operand, destination } memory =
  case opcode of
    Add       -> add l_operand r_operand destination memory
    Multiply  -> multiply l_operand r_operand destination memory
    EOF       -> memory

--
get : Int -> Memory -> Int
get i memory =
  Array.get i memory |> Maybe.withDefault 0

--
add : Int -> Int -> Int -> Memory -> Memory
add l_operand r_operand destination memory =
  memory |> Array.set destination
    (get l_operand memory
    + get r_operand memory
   )

--
multiply : Int -> Int -> Int -> Memory -> Memory
multiply l_operand r_operand destination memory =
  memory |> Array.set destination
    (get l_operand memory
    * get r_operand memory
   )