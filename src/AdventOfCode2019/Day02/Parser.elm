module AdventOfCode2019.Day02.Parser exposing
  ( run
  )

-- Imports ---------------------------------------------------------------------
import Parser exposing ( Parser, Trailing (..), Step (..), (|.), (|=) )
import AdventOfCode2019.Day02.Program exposing ( Instruction, Opcode (..) )

-- Parsers ---------------------------------------------------------------------
--
separator : Parser ()
separator =
  Parser.succeed ()
    |. Parser.oneOf [ Parser.symbol ",", Parser.spaces ]

--
addOpcode : Parser Opcode
addOpcode =
  Parser.succeed Add
    |. Parser.token "1"

--
addInstruction : Parser Instruction
addInstruction =
  Parser.succeed Instruction
    |= addOpcode
    |. separator
    |= Parser.int
    |. separator 
    |= Parser.int
    |. separator 
    |= Parser.int
    |. separator

--
multiplyOpcode : Parser Opcode
multiplyOpcode =
  Parser.succeed Multiply
    |. Parser.token "2"

--
multiplyInstruction : Parser Instruction
multiplyInstruction =
  Parser.succeed Instruction
    |= multiplyOpcode
    |. separator
    |= Parser.int
    |. separator 
    |= Parser.int
    |. separator 
    |= Parser.int
    |. separator 

--
eofOpcode : Parser Opcode
eofOpcode =
  Parser.succeed EOF
    |. Parser.token "99"

--
eofInstruction : Parser Instruction
eofInstruction =
  Parser.succeed Instruction
    |= eofOpcode
    |. separator
    |= Parser.succeed 0
    |= Parser.succeed 0
    |= Parser.succeed 0

--
instructions : List Instruction -> Parser ( Step (List Instruction) (List Instruction) )
instructions rest =
  Parser.oneOf
    [ addInstruction
        |> Parser.map (\i -> Loop ( i :: rest ))
    , multiplyInstruction
        |> Parser.map (\i -> Loop ( i :: rest ))
    , eofInstruction
        |> Parser.map (\i -> Done ( i :: rest |> List.reverse ))
    , Parser.succeed ()
        |. Parser.spaces
        |. Parser.end
        |> Parser.map (\_ -> Done ( List.reverse rest ))
    ]

--
program : Parser ( List Instruction )
program =
  Parser.loop [] instructions

-- Functions -------------------------------------------------------------------
--
run : String -> List Instruction
run input =
  Parser.run program input
    |> Result.withDefault []

