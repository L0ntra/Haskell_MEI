> module Mei where

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


Note:
A note is made up of a combinations of a NoteName, Octave, Duration and Accidental
The NoteName and Octave work together to place the node on the staff
The Duration specifies what type of note it is (Ex: 4 = Quarter Note)
The Accidental specifies if the note is sharp or flat

> data Note = MkNote NoteName Octave Duration Accidental

> instance Show Note where
>   show (MkNote a b c d) = 
>     "(" ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d ++ ")"


Layer
A Layer is comprised of the Layer Number and one or more Notes
Chords can be formed by specifying notes on different layers that occur
at the same time.

> data Layer   = MkLayer Int [Note]
>   deriving Show


Staff:
A Staff is comprised of the Staff Number and one or more Layers
Multiple Staffs can be used to specify multipl clefs or instruments that are
being played at the same time

> data Staff   = MkStaff Int [Layer]
>   deriving Show


Measure:
A Measure is comprised of the Measure Number and one or more Staffs

> data Measure = MkMeasure Int [Staff]
>   deriving Show


Section:
A Section is comprised of one or more Measures

> data Section = MkSection [Measure]
>   deriving Show


Score:
A Score is comprised of one or more Sections.
Currently there is no need to have more than one section.

> data Score   = MkScore {-- scoreDef --} [Section]
>   deriving Show

Ex:

MkScore 
  [ MkSection 
    [ MkMeasure 1 
      [ MkStaff 1 
        [ MkLayer 1 
          [ MkNote C (Oct 4) (Dur 2) None,
            MkNote E (Oct 4) (Dur 2) None 
          ],
          MkLayer 2
          [ MkNote E (Oct 4) (Dur 2) None,
            MkNote G (Cot 4) (Dur 2) None
          ]
        ]
      ]
    ]
  ]

Represents:     
[--|--------------]
[--|--------|-----]
[--|---|----|-----]
[--|---|---O|-----]
[--|--O|---O`-----]
     -O'-
  
