module AdventOfCode2019.Day05 exposing (..)

-- Imports ---------------------------------------------------------------------
import Array exposing (Array)
import Basics.Extra as Basics
import Parser exposing (Parser, Trailing (..), Step (..), (|.), (|=))

-- Types -----------------------------------------------------------------------
type Instruction
  = Add ParameterMode ParameterMode
  | Multiply ParameterMode ParameterMode
  | Read
  | Write ParameterMode
  | JumpIfTrue ParameterMode ParameterMode
  | JumpIfFalse ParameterMode ParameterMode
  | LessThan ParameterMode ParameterMode
  | EqualsTo ParameterMode ParameterMode
  | End

type alias Parameter =
  (Int, ParameterMode)

type ParameterMode
  = Position
  | Immediate

type alias Program =
  { input : String
  , position : Int
  , output : List Int
  , tape : Array String
  }

type State
  = Running (Program, Instruction)
  | Finished (List Int)
  | Crashed String

-- Input -----------------------------------------------------------------------
--
parseInput : String -> String -> Result String Program
parseInput input programInput =
  String.trim input 
    |> String.split ","
    |> List.map (\intcode ->
      if String.contains "-" intcode then
        intcode
      else
        String.padLeft 4 '0' intcode
    )
    |> Array.fromList
    |> Program programInput 0 []
    |> Ok

--
parseNextInstruction : Program -> State
parseNextInstruction ({ position, tape } as program) =
  Array.get position tape
    |> Result.fromMaybe "Pointer out of bounds."
    |> Result.andThen (Parser.run parseInstruction
    >> Result.mapError (always "Could not parse instruction."))
    |> (\result ->
      case result of
        Ok instr    -> Running (program, instr)
        Err message -> Crashed message
    )

--
parseInstruction : Parser Instruction
parseInstruction =
  Parser.succeed identity
    |= Parser.oneOf 
      [ Parser.backtrackable parseAdd
      , Parser.backtrackable parseMultiply
      , Parser.backtrackable parseRead
      , Parser.backtrackable parseWrite
      , Parser.backtrackable parseJumpIfTrue
      , Parser.backtrackable parseJumpIfFalse
      , Parser.backtrackable parseLessThan
      , Parser.backtrackable parseEqualsTo
      , parseEnd 
      ]
    |. Parser.spaces
    |. Parser.end

--
parameterMode : Parser ParameterMode
parameterMode =
  Parser.oneOf
    [ Parser.succeed Position
        |. Parser.token "0"
        |> Parser.backtrackable
    , Parser.succeed Immediate
        |. Parser.token "1"
        |> Parser.backtrackable
    ]

--
parseAdd : Parser Instruction
parseAdd =
  Parser.succeed (Basics.flip Add)
    |= parameterMode
    |= parameterMode
    |. Parser.token "01"

--
parseMultiply : Parser Instruction
parseMultiply =
  Parser.succeed (Basics.flip Multiply)
    |= parameterMode
    |= parameterMode
    |. Parser.token "02"

--
parseRead : Parser Instruction
parseRead =
  Parser.succeed Read
    |. parameterMode
    |. parameterMode
    |. Parser.token "03"

--
parseWrite : Parser Instruction
parseWrite =
  Parser.succeed Write
    |. parameterMode
    |= parameterMode
    |. Parser.token "04"

--
parseJumpIfTrue : Parser Instruction
parseJumpIfTrue =
  Parser.succeed (Basics.flip JumpIfTrue)
    |= parameterMode
    |= parameterMode
    |. Parser.token "05" 

--
parseJumpIfFalse : Parser Instruction
parseJumpIfFalse =
  Parser.succeed (Basics.flip JumpIfFalse)
    |= parameterMode
    |= parameterMode
    |. Parser.token "06"

--
parseLessThan : Parser Instruction
parseLessThan =
  Parser.succeed (Basics.flip LessThan)
    |= parameterMode
    |= parameterMode
    |. Parser.token "07"

--
parseEqualsTo : Parser Instruction
parseEqualsTo =
  Parser.succeed (Basics.flip EqualsTo)
    |= parameterMode
    |= parameterMode
    |. Parser.token "08"

--
parseEnd : Parser Instruction
parseEnd =
  Parser.succeed End
    |. Parser.token "0099"

-- Functions -------------------------------------------------------------------
--                                                  
debug : Program -> Program
debug ({ position, tape } as program) =
  let
    _ = Debug.log "position" position
    _ = Debug.log "tape" (Array.toList tape |> List.drop position |> List.take 8)
  in
  program

--
run : Program -> Result String (List Int)
run ({ input, position, output, tape } as program) =
  let
    andThen : (Program -> State) -> State -> State
    andThen f state =
      case state of
        Running (p, _)  -> f p
        _               -> state
           
    helper : State -> Result String (List Int)
    helper state =
      case state of
        Running p   -> execute p |> andThen parseNextInstruction |> helper
        Finished o  -> Ok o
        Crashed e   -> Err e
 
  in
  parseNextInstruction program
    |> helper

--
read : ParameterMode -> Int -> Program -> Maybe Int 
read mode position ({ tape } as program) =
  case mode of
    Position ->
      Array.get position tape
        |> Maybe.andThen String.toInt
        |> Maybe.andThen (\p -> read Immediate p program)

    Immediate ->
      Array.get position tape
        |> Maybe.andThen String.toInt

--
writeRaw : (Int, String) -> Program -> Program
writeRaw (position, value) ({ tape } as program) =
  Array.set position value tape
    |> (\t -> { program | tape = t })

--
write : (Int, Int) -> Program -> Program
write (position, value) ({ tape } as program) =
  if value < 0 then
    String.fromInt value
      |> (\v -> Array.set position v tape )
      |> (\t -> { program | tape = t })
  else
    String.fromInt value
      |> String.padLeft 4 '0'
      |> (\v -> Array.set position v tape )
      |> (\t -> { program | tape = t })

--
move : Int -> Program -> Program
move offset ({ position } as program) =
  { program | position = position + offset }

--
moveTo : Int -> Program -> Program
moveTo target ({ position } as program) =
  { program | position = target }

--
stdIn : Program -> Maybe Program
stdIn ({ input, position } as program) =
  Maybe.map2 Tuple.pair (read Immediate (position + 1) program) (Just input)
    |> Maybe.map (\(i, v) -> writeRaw (i, v) program |> move 2)

stdOut : ParameterMode -> Program -> Maybe Program
stdOut mode ({ output, position } as program) =
  read mode (position + 1) program
    |> Maybe.map (\v -> { program | output = v :: output } |> move 2)

--
add : ParameterMode -> ParameterMode -> Program -> Maybe Program
add l_mode r_mode ({ position } as program) =
  Maybe.map2 (+) (read l_mode (position + 1) program) (read r_mode (position + 2) program)
    |> Maybe.map2 Tuple.pair (read Immediate (position + 3) program)
    |> Maybe.map (\(i, v) -> write (i, v) program |> move 4)

--
multiply : ParameterMode -> ParameterMode -> Program -> Maybe Program
multiply l_mode r_mode ({ position } as program) =
  Maybe.map2 (*) (read l_mode (position + 1) program) (read r_mode (position + 2) program)
    |> Maybe.map2 Tuple.pair (read Immediate (position + 3) program)
    |> Maybe.map (\(i, v) -> write (i, v) program |> move 4)

--
jump : ParameterMode -> ParameterMode -> (Int -> Bool) -> Program -> Maybe Program
jump t_mode p_mode test ({ position } as program) =
  read t_mode (position + 1) program 
    |> Maybe.map test 
    |> Maybe.map2 Tuple.pair (read p_mode (position + 2) program)
    |> Maybe.map (\(p, t) -> if t then moveTo p program else move 3 program)

--
compare : ParameterMode -> ParameterMode -> (Int -> Int -> Bool) -> Program -> Maybe Program
compare l_mode r_mode test ({ position } as program) =
  Maybe.map2 test (read l_mode (position + 1) program) (read r_mode (position + 2) program)
    |> Maybe.map (\t -> if t then writeRaw (position + 3, "0001") program else writeRaw (position + 3, "0000") program)
    |> Maybe.map (move 4)

--
execute : (Program, Instruction) -> State
execute (({ input, position, output, tape } as program), instr) =
  case instr of
    Add l_mode r_mode ->
      add l_mode r_mode program |> (\maybe ->
        case maybe of
          Just p  -> Running (p, instr)
          Nothing -> Crashed "ADD: Error performing add operation."
      )

    Multiply l_mode r_mode ->
      multiply l_mode r_mode program |> (\maybe ->
        case maybe of
          Just p  -> Running (p, instr)
          Nothing -> Crashed "MULTIPLY: Error performing add operation."
      )

    Read ->
      stdIn program |> (\maybe ->
        case maybe of
          Just p  -> Running (p, instr)
          Nothing -> Crashed "READ: Error reading from input."                                                          
      )

    Write mode ->
      stdOut mode program |> (\maybe ->
        case maybe of
          Just p  -> Running (p, instr)
          Nothing -> Crashed "WRITE: Error writing to output."
      )

    JumpIfTrue t_mode p_mode ->
      jump t_mode p_mode ((/=) 0) program |> (\maybe ->
        case maybe of
          Just p  -> Running (p, instr)
          Nothing -> Crashed "JUMP: "
      )
                                
    JumpIfFalse t_mode p_mode ->
      jump t_mode p_mode ((==) 0) program |> (\maybe ->
        case maybe of
          Just p  -> Running (p, instr)
          Nothing -> Crashed "JUMP: "
      )

    LessThan l_mode r_mode ->
      compare l_mode r_mode (<) program |> (\maybe ->
        case maybe of
          Just p  -> Running (p, instr)
          Nothing -> Crashed "COMP:"
      )

    EqualsTo l_mode r_mode ->
      compare l_mode r_mode (==) program |> (\maybe ->
        case maybe of
          Just p  -> Running (p, instr)
          Nothing -> Crashed "COMP:"
      )

    End ->
      Finished output


-- Solvers ---------------------------------------------------------------------
part1 : String -> Result String Int
part1 input =
  parseInput input "1"
    |> Result.andThen run
    |> Result.andThen (List.head >> Result.fromMaybe "")

part2 : String -> Result String Int
part2 input =
  parseInput input "5"
    |> Result.andThen run
    |> Result.andThen (List.head >> Result.fromMaybe "")
