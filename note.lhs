> data NoteName = A | B | C | D | E | F | G

> instance Show NoteName where
>   show A = "a"
>   show B = "b"
>   show C = "c"
>   show D = "d"
>   show E = "e"
>   show F = "f"
>   show F = "g"

Octave:
An Octave can be any natural number.
It may be reasonable to limit the range

> data Octave = Oct Int

> instance Show Octave where
>   show (Oct a) = show a

Duration:
Duration can be powers of 2 up to 256

> data Duration = Dur Int --2 | 4 | 8 | 16 | 32 | 64 | 128 | 256

> instance Show Duration where
>   show (Dur a) = show a

Accidental:
TODO: add addition Accidentals specified in documentation
http://music-encoding.org/documentation/3.0.0/data.ACCIDENTAL.EXPLICIT/

> data Accidental = Sharp   | DSharp | TSharp
>                 | Flat    | DFlat  | TFlat
>                 | Natural | None

> instance Show Accidental where
>   show Sharp   = "s"
>   show DSharp  = "ss"
>   show TSharp  = "ts"
>   show Flat    = "f"
>   show DFlat   = "ff"
>   show TFlat   = "tf"
>   show Natural = "n"
>   show None    = ""
>   show _       = "Unknown Accidental"


> data Note = MkNote NoteName Octave Duration Accidental

> instance Show Note where
>   show (MkNote a b c d) = 
>     "(" ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d ++ ")"

> data Measure = MkMeasure [Note]
>   deriving Show
