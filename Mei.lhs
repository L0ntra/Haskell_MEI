> module Mei where
> import IOActions
> import Prelude hiding ((^^))

> indent  :: Int -> String
> indent n = '\n': replicate (n*2) ' '

> class Render a where
>   render :: a -> String


> data NoteName = A | B | C | D | E | F | G

> instance Show NoteName where
>   show A = "a"
>   show B = "b"
>   show C = "c"
>   show D = "d"
>   show E = "e"
>   show F = "f"
>   show G = "g"

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

> instance Render Note where
>   render (MkNote a b c d) = indent 8 ++ "<note "
>                          ++ "pname=\"" ++ show a ++ "\" "
>                          ++ "oct=\""   ++ show b ++ "\" "
>                          ++ "dur=\""   ++ show c ++ "\" "
>                          ++ "accid=\"" ++ show d ++ "\" />"


Layer
A Layer is comprised of the Layer Number and one or more Notes
Chords can be formed by specifying notes on different layers that occur
at the same time.

> data Layer   = MkLayer Int [Note]
>   deriving Show

> instance Render Layer where
>   render (MkLayer n b) = indent 7 ++ "<layer n=\"" ++ show n ++ "\">"
>                       ++ (concat (map render b))
>                       ++ indent 7 ++ "</layer>"


Staff:
A Staff is comprised of the Staff Number and one or more Layers
Multiple Staffs can be used to specify multipl clefs or instruments that are
being played at the same time

> data Staff   = MkStaff Int [Layer]
>   deriving Show

> instance Render Staff where
>   render (MkStaff n b) = indent 6 ++ "<staff n=\"" ++ show n ++ "\">"
>                       ++ (concat (map render b))
>                       ++ indent 6 ++ "</staff>"

Measure:
A Measure is comprised of the Measure Number and one or more Staffs

> data Measure = MkMeasure Int [Staff]
>   deriving Show

> instance Render Measure where
>   render (MkMeasure n b) = indent 5 ++ "<measure n=\"" ++ show n ++ "\">"
>                         ++ (concat (map render b))
>                         ++ indent 5 ++ "</measure>"


Section:
A Section is comprised of one or more Measures

> data Section = MkSection [Measure]
>   deriving Show

> instance Render Section where
>   render (MkSection b) = indent 4 ++ "<section>"
>                       ++ (concat (map render b))
>                       ++ indent 4 ++ "</section>"

Score:
A Score is comprised of one or more Sections.
Currently there is no need to have more than one section.

> data Score   = MkScore {-- ScoreDef --} [Section]
>   deriving Show

> instance Render Score where
>   render (MkScore b) = indent 0 ++ "<music>" ++ indent 1 ++ "<body>" ++ indent 2 ++ "<mdiv>" ++ indent 3 ++ "<score>"
>                       ++ (render MkScoreDef)
>                       ++ (concat (map render b))
>                       ++ indent 3 ++ "</score>" ++ indent 2 ++ "</mdiv>" ++ indent 1 ++ "</body>" ++ indent 0 ++ "</music>"
> data ScoreDef = MkScoreDef

> instance Show ScoreDef where
>   show a = indent 4 ++ "<scoreDef "
>            ++ "key.sig=\"0\" "
>            ++ "meter.count=\"4\" "
>            ++ "meter.unit=\"4\" "
>            ++ "meter.sym=\"common\">"
>               ++ indent 5 ++ "<staffGrp>"
>                  ++ indent 6 ++ "<staffDef "
>                     ++ "clef.shape=\"G\" "
>                     ++ "clef.line=\"2\" "
>                     ++ "n=\"1\" "
>                     ++ "lines=\"5\" />"
>               ++ indent 5 ++ "</staffGrp>"
>            ++ indent 4 ++ "</scoreDef>"
>

> instance Render ScoreDef where
>   render = show 


TODO: Simplify the infix operators to be easir to use:
EX: 
        !@ Note    -> Layer
Note    @@ Note    -> Layer
Note    @@ Layer   -> Layer
Layer   @@ Note    -> Layer
Layer   @@ Layer   -> Layer

        !~ Layer   -> Staff
Layer   ~~ Layer   -> Staff
Layer   ~~ Staff   -> Staff
Staff   ~~ Layer   -> Staff
Staff   ~~ Staff   -> Staff

        !^ Staff   -> Measure
Staff   ^^ Staff   -> Measure
Staff   ^^ Measure -> Measure
Measure ^^ Staff   -> Measure
Measure ^^ Measure -> Measure

        !/ Measure -> Section
Measure // Measure -> Section
Measure // Section -> Section
Section // Measure -> Section
Section // Section -> Section

        !% Section -> Score
Section %% Section -> Score
Section %% Score   -> Score
Score   %% Section -> Score
Score   %% Score   -> Score

Operators to build Layers out of Layes and Notes
------------------------------------------------

Combine Two Notes together to make a Layer

> infixl @@
> (@@) :: Note -> Note -> Layer
> a @@ b = MkLayer 1 [a, b]

Append a Note to a layer

> infixl ~@
> (~@) :: Layer -> Note -> Layer
> (MkLayer n as) ~@ b = MkLayer n (as ++ [b])

Prepend a Note to a Layer

> infixl @~ 
> (@~) :: Note -> Layer -> Layer
> a @~ (MkLayer n bs) = MkLayer n (a:bs)

Combine the notes from two layers together to create a single layer

> infixl ~~~
> (~~~) :: Layer -> Layer -> Layer
> (MkLayer n1 as) ~~~ (MkLayer n2 bs) = MkLayer n1 (as ++ bs) --TODO: Make sure there are no bugs here

Build a Layer out of a Single Note

> (!^) :: Note -> Layer
> (!^) a = MkLayer 1 [a]

Operators to build Staffs out of Layers and Staffs
--------------------------------------------------

Combine Two Layers together to make a Staff

> infixl ~~^
> (~~^) :: Layer -> Layer -> Staff
> (MkLayer n1 as)  ~~^ (MkLayer n2 bs) = MkStaff 1 [MkLayer n1 as, MkLayer (n1 + 1) bs]

Append a Layer to a Staff

> infixl ^~
> (^~) :: Staff -> Layer -> Staff
> (MkStaff sn as) ^~ (MkLayer ln bs) = MkStaff sn (as ++ [MkLayer (length as +1) bs])

Prepend a Layer to a Staff

> infixl ~^
> (~^) :: Layer -> Staff -> Staff
> a ~^ (MkStaff n bs) = MkStaff n (a:bs)

Combine the Layers from tow Staffs to make a Single Staff

> infixl ^^^
> (^^^) :: Staff -> Staff -> Staff
> (MkStaff n1 as) ^^^ (MkStaff n2 bs) = MkStaff n1 (as ++ bs) --TODO: Make sure there are no Bugs here

Build a Staff out of a single Layer

> (!~) :: Layer -> Staff
> (!~) a = MkStaff 1 [a]

Operators to build Measures out of Staffs and Measures
------------------------------------------------------

Combine Two staffs to make a Measure

> infixl ^^/
> (^^/) :: Staff -> Staff -> Measure
> (MkStaff n1 as) ^^/ (MkStaff n2 bs) = MkMeasure 1 [MkStaff n1 as, MkStaff (n1 + 1) bs]

Append a Measure to a Staff

> infixl /^
> (/^) :: Measure -> Staff -> Measure
> (MkMeasure nm as) /^ (MkStaff ns bs) = MkMeasure nm (as ++ [MkStaff (length as +1) bs])

Prepend a Measure to a Staff

> infixl ^/
> (^/) :: Staff -> Measure -> Measure
> a ^/ (MkMeasure n1 bs) = MkMeasure n1 (a:bs)

Combine the Staffs in two Measures to make a single Measure

> infixl ///
> (///) :: Measure -> Measure -> Measure
> (MkMeasure n1 as) /// (MkMeasure n2 bs) = MkMeasure n1 (as ++ bs) --TODO: Make sure there are no bugs here

Build a Measure out of a single Staff

> (!/) :: Staff -> Measure
> (!/) a = MkMeasure 1 [a]

Operators to build Sections out of Measures and Sections
--------------------------------------------------------

Combine two Measures to make a Section

> infixl //%
> (//%) :: Measure -> Measure -> Section
> (MkMeasure n1 as) //% (MkMeasure n2 bs) = MkSection [MkMeasure n1 as, MkMeasure (n1 + 1) bs]

Append a Measure to a Section

> infixl %/
> (%/) :: Section -> Measure -> Section
> (MkSection as) %/ MkMeasure n b = MkSection (as ++ [MkMeasure (length as + 1) b])

Prepend a Measure to a Section

> infixl /%
> (/%) :: Measure -> Section -> Section
> a /% (MkSection bs) = MkSection (a:bs) --TODO add 1 to all measure numbers

Combine the Measures in two Sections to Create a Single Section

> infixl %%%
> (%%%) :: Section -> Section -> Section
> MkSection a %%% MkSection b = MkSection (a ++ b) --TODO: Make sure there are no bugs here

Build A section out of a single Measure

> (!%) :: Measure -> Section
> (!%) a = MkSection [a]

Operators to build the Score out of Sections
--------------------------------------------

Combine Two Sections to create a Score

> infixl %%>
> (%%>) :: Section -> Section -> Score
> a %%> b = MkScore (a:[b])

Append a Section to a Score

> infix >%
> (>%) :: Score -> Section -> Score
> MkScore as >% b = MkScore (as ++ [b])

Prepend a Section to a Score

> infix %>
> (%>) :: Section -> Score -> Score
> a %> MkScore bs = MkScore (a:bs)

Create a Score from a Single Section

> (!>) :: Section -> Score
> (!>) a = MkScore [a]

> testNoteA    = MkNote A (Oct 4) (Dur 4) None
> testNoteB    = MkNote B (Oct 4) (Dur 4) None

> testLayer1   = testNoteA    @@  testNoteB
> testLayer2   = testNoteA    @~  testLayer1
> testLayer3   = testLayer1   ~@  testNoteA
> testLayer4   = testLayer1   ~~~ testLayer2

> testStaff1   = testLayer1   ~~^ testLayer2
> testStaff2   = testLayer1   ~^  testStaff1
> testStaff3   = testStaff1   ^~  testLayer1
> testStaff4   = testStaff1   ^^^ testStaff2

> testMeasure1 = testStaff1   ^^/ testStaff2
> testMeasure2 = testStaff1   ^/  testMeasure1
> testMeasure3 = testMeasure1 /^  testStaff1
> testMeasure4 = testMeasure1 /// testMeasure2

> testSection1 = testMeasure1 //% testMeasure2
> testSection2 = testMeasure1 /%  testSection1
> testSection3 = testSection1 %/  testMeasure1
> testSection4 = testSection1 %%% testSection2

> testScore1   = testSection1 %%> testSection2
> testScore2   = testSection1 %>  testScore1
> testScore3   = testScore1   >%  testSection1
> testScore4   = (!>) testSection1


> testRender  = render (MkScore [MkSection [MkMeasure 1 [MkStaff 1 [MkLayer 1 [MkNote A (Oct 4) (Dur 4) None]]]]])
> testRender2 = render (MkScore [MkSection [MkMeasure 1 [MkStaff 1 [MkLayer 1 [MkNote C (Oct 4) (Dur 2) None,
>                                                                              MkNote E (Oct 4) (Dur 2) None],
>                                                                   MkLayer 2 [MkNote E (Oct 4) (Dur 2) None,
>                                                                              MkNote G (Oct 4) (Dur 2) None]]]]])


Mary had a little lamb written in operator notation

> lamb = (!>) $ 
>   (!%) ( (!/) ( (!~) 
>   (  (MkNote E (Oct 4) (Dur 4) None)
>   @@ (MkNote D (Oct 4) (Dur 4) None)
>   ~@ (MkNote C (Oct 4) (Dur 4) None)
>   ~@ (MkNote D (Oct 4) (Dur 4) None)
>   ))) %/ (
>   (!/) ( (!~) 
>   (  (MkNote E (Oct 4) (Dur 4) None)
>   @@ (MkNote E (Oct 4) (Dur 4) None)
>   ~@ (MkNote E (Oct 4) (Dur 2) None)  
>   ))) %/ ( 
>   (!/) ( (!~) 
>   (  (MkNote D (Oct 4) (Dur 4) None)
>   @@ (MkNote D (Oct 4) (Dur 4) None)
>   ~@ (MkNote D (Oct 4) (Dur 2) None)
>   ))) %/ (
>   (!/) ( (!~)
>   (  (MkNote E (Oct 4) (Dur 4) None)
>   @@ (MkNote G (Oct 4) (Dur 4) None)
>   ~@ (MkNote G (Oct 4) (Dur 2) None)
>   ))) %/ (
>   (!/) ( (!~)
>   (  (MkNote E (Oct 4) (Dur 4) None)
>   @@ (MkNote D (Oct 4) (Dur 4) None)
>   ~@ (MkNote C (Oct 4) (Dur 4) None)
>   ~@ (MkNote D (Oct 4) (Dur 4) None)
>   ))) %/ (
>   (!/) ( (!~)
>   (  (MkNote E (Oct 4) (Dur 4) None)
>   @@ (MkNote E (Oct 4) (Dur 4) None)
>   ~@ (MkNote E (Oct 4) (Dur 4) None)
>   ~@ (MkNote E (Oct 4) (Dur 4) None)
>   ))) %/ (
>   (!/) ( (!~)
>   (  (MkNote D (Oct 4) (Dur 4) None)
>   @@ (MkNote D (Oct 4) (Dur 4) None)
>   ~@ (MkNote E (Oct 4) (Dur 4) None)
>   ~@ (MkNote D (Oct 4) (Dur 4) None)
>   ))) %/ (
>   (!/) ( (!~) (
>   (!^) ( (MkNote C (Oct 4) (Dur 1) None)))))


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




