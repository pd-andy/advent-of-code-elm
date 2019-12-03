module Utils exposing
  ( withCmd, withCmdNone
  -- Function Utils ------------------------------------------------------------
  , apply, flip, uncurry
  -- String Utils --------------------------------------------------------------
  , parseInt, parseIntArray, parseFloat
  -- Math Utils ----------------------------------------------------------------
  , divBy, subBy
  -- Tuple Utils ---------------------------------------------------------------
  , toTuple, sumTuple
   -- List Utils ---------------------------------------------------------------
  , range, commonElements
  )

-- Imports ---------------------------------------------------------------------
import Array exposing ( Array )
import Set exposing ( Set )

-- Update Utils ----------------------------------------------------------------
withCmd : Cmd msg -> model -> ( model, Cmd msg )
withCmd cmd model =
  ( model, cmd )

withCmdNone : model -> ( model, Cmd msg )
withCmdNone model =
  withCmd Cmd.none model

-- Function Utils --------------------------------------------------------------
apply : a -> ( a -> b ) -> b
apply a f =
  f a

flip : ( a -> b -> c ) -> b -> a -> c
flip f b a =
  f a b

uncurry : (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) =
  f a b

-- String Utils ----------------------------------------------------------------
parseInt : String -> Int
parseInt =
  String.toInt >> Maybe.withDefault 0

parseFloat : String -> Float
parseFloat =
  String.toFloat >> Maybe.withDefault 0

parseIntArray : String -> String -> Array Int
parseIntArray separator input =
  String.split separator input
    |> List.filterMap String.toInt
    |> Array.fromList

-- Math Utils ------------------------------------------------------------------
divBy : Float -> Float -> Float
divBy y x =
  x / y

subBy : number -> number -> number
subBy y x =
  x - y

-- Tuple Utils -----------------------------------------------------------------
toTuple : a -> ( a, a )
toTuple a =
  ( a, a )

sumTuple : ( number, number ) -> number
sumTuple ( x, y ) =
  x + y

-- List Utils ------------------------------------------------------------------
range : Int -> Int -> List Int
range start end =
  if end > start then
    List.range start end
  else
    range end start
      |> List.reverse

commonElements : List (List comparable) -> List comparable
commonElements input =
  case input of
    head :: tail ->
      List.foldl Set.intersect (Set.fromList head) (List.map Set.fromList tail)
        |> Set.toList

    [] ->
      []