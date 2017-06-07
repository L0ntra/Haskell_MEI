> module Mei where
> import IOActions
> import Prelude hiding ((^^))


Helper to function to indent during the render process

> indent  :: Int -> String
> indent n = '\n': replicate (n*2) ' '


Items that are renderable are members of the render class.
Only the rend method is required to be implemented in order to be
a member of the render class

> class Render a where
>   render  :: [a] -> String
>   render a = concat $ zipWith rend [1..] a
>
>   rend    :: Int -> a -> String


> data NoteName =      C -- | Cs --TODO: Implement Logic for sharps
>               | Db | D -- | Ds
>               | Eb | E
>                    | F -- | Fs
>               | Gb | G -- | Gs
>               | Ab | A -- | As
>               | Bb | B
>    deriving (Show, Enum, Eq)

> instance Render NoteName where
>   rend n C  = "pname=\"c\" "
>   -- rend n Cs = "pname=\"c\" accid=\"s\" "
>   rend n Db = "pname=\"d\" accid=\"f\" "
>   rend n D  = "pname=\"d\" "
>   -- rend n Ds = "pname=\"d\" accid=\"s\" "
>   rend n Eb = "pname=\"e\" accid=\"f\" "
>   rend n E  = "pname=\"e\" "
>   rend n F  = "pname=\"f\" "
>   -- rend n Fs = "pname=\"f\" accid=\"s\" "
>   rend n Gb = "pname=\"g\" accid=\"f\" "
>   rend n G  = "pname=\"g\" "
>   -- rend n Gs = "pname=\"g\" accid=\"s\" "
>   rend n Ab = "pname=\"a\" accid=\"f\" "
>   rend n A  = "pname=\"a\" "
>   -- rend n As = "pname=\"a\" accid=\"s\" "
>   rend n Bb = "pname=\"b\" accid=\"f\" "
>   rend n B  = "pname=\"b\" "

> namesToEnum :: [NoteName] -> [Int]
> namesToEnum  = map fromEnum

> enumToNames   :: [Int] -> [NoteName]
> enumToNames ns = [(toEnum a) :: NoteName | a <- ns ]


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

Note:
A note is made up of a combinations of a NoteName, Octave, Duration and Accidental
The NoteName and Octave work together to place the node on the staff
The Duration specifies what type of note it is (Ex: 4 = Quarter Note)
The Accidental specifies if the note is sharp or flat

> data Note = MkNote NoteName Octave Duration

> instance Show Note where
>   show (MkNote a b c) = 
>     "(" ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ ")"

> instance Render Note where
>   rend n (MkNote a b c) = indent 8 ++ "<note "
>                        ++ rend 0 a  
>                        ++ "oct=\""   ++ show b ++ "\" "
>                        ++ "dur=\""   ++ show c ++ "\" />"

Layer
A Layer is comprised of the Layer Number and one or more Notes
Chords can be formed by specifying notes on different layers that occur
at the same time.

> data Layer   = MkLayer [Note]
>   deriving Show

> instance Render Layer where
>   rend n (MkLayer notes) = indent 7 ++ "<layer n=\"" ++ show n ++ "\">"
>                       ++ render notes
>                       ++ indent 7 ++ "</layer>"


Staff:
A Staff is comprised of the Staff Number and one or more Layers
Multiple Staffs can be used to specify multipl clefs or instruments that are
being played at the same time

> data Staff   = MkStaff [Layer]
>   deriving Show

> instance Render Staff where
>   rend n (MkStaff layers) = indent 6 ++ "<staff n=\"" ++ show n ++ "\">"
>                        ++ render layers
>                        ++ indent 6 ++ "</staff>"


Measure:
A Measure is comprised of the Measure Number and one or more Staffs

> data Measure = MkMeasure [Staff]
>   deriving Show

> instance Render Measure where
>   rend n (MkMeasure staffs) = indent 5 ++ "<measure n=\"" ++ show n ++ "\">"
>                          ++ render staffs
>                          ++ indent 5 ++ "</measure>"


Section:
A Section is comprised of one or more Measures

> data Section = MkSection [Measure]
>   deriving Show

> instance Render Section where
>   rend n (MkSection sects) = indent 4 ++ "<section>"
>                           ++ render sects
>                          ++ indent 4 ++ "</section>"

Score:
A Score is comprised of one or more Sections.
Currently there is no need to have more than one section.

> data Score   = MkScore {-- ScoreDef --} [Section]
>   deriving Show

> instance Render Score where
>   rend n (MkScore b) = indent 0 ++ "<music>" ++ indent 1 ++ "<body>" ++ indent 2 ++ "<mdiv>" ++ indent 3 ++ "<score>"
>                   ++ show MkScoreDef
>                   ++ render b
>                   ++ indent 3 ++ "</score>" ++ indent 2 ++ "</mdiv>" ++ indent 1 ++ "</body>" ++ indent 0 ++ "</music>"


> renderScore  :: Score -> String
> renderScore s = render [s]

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


TODO: Further Simplify the Opertors to make writing a piece more intuitive

Operators to build Layers out of Layes and Notes
------------------------------------------------

Combine Two Notes together to make a Layer

> infixr @@
> (@@) :: Note -> Note -> Layer
> a @@ b = MkLayer [a, b]

Prepend a Note to a Layer

> infixr @|
> (@|) :: Note -> Layer -> Layer
> a @| (MkLayer bs) = MkLayer (a:bs)

Combine the notes from two layers together to create a single layer

> infixr @++
> (@++) :: Layer -> Layer -> Layer
> (MkLayer as) @++ (MkLayer bs) = MkLayer (as ++ bs) 

Build a Layer out of a Single Note

> (!@) :: Note -> Layer
> (!@) a = MkLayer [a]

Operators to build Staffs out of Layers and Staffs
--------------------------------------------------

Combine Two Layers together to make a Staff

> infixr ~~
> (~~) :: Layer -> Layer -> Staff
> (MkLayer as)  ~~ (MkLayer bs) = MkStaff [MkLayer as, MkLayer bs]

Prepend a Layer to a Staff

> infixr ~|
> (~|) :: Layer -> Staff -> Staff
> a ~| (MkStaff bs) = MkStaff (a:bs)

Combine the Layers from tow Staffs to make a Single Staff

> infixr ~++
> (~++) :: Staff -> Staff -> Staff
> (MkStaff as) ~++ (MkStaff bs) = MkStaff (as ++ bs)

Build a Staff out of a single Layer

> (!~) :: Layer -> Staff
> (!~) a = MkStaff [a]

Operators to build Measures out of Staffs and Measures
------------------------------------------------------

Combine Two staffs to make a Measure

> infixr //
> (//) :: Staff -> Staff -> Measure
> (MkStaff as) // (MkStaff bs) = MkMeasure [MkStaff as, MkStaff bs]

Prepend a Measure to a Staff

> infixr /|
> (/|) :: Staff -> Measure -> Measure
> a /| (MkMeasure bs) = MkMeasure (a:bs)

Combine the Staffs in two Measures to make a single Measure

> infixr /++
> (/++) :: Measure -> Measure -> Measure
> (MkMeasure as) /++ (MkMeasure bs) = MkMeasure (as ++ bs)

Build a Measure out of a single Staff

> (!/) :: Staff -> Measure
> (!/) a = MkMeasure [a]

Operators to build Sections out of Measures and Sections
--------------------------------------------------------

Combine two Measures to make a Section

> infixr %%
> (%%) :: Measure -> Measure -> Section
> (MkMeasure as) %% (MkMeasure bs) = MkSection [MkMeasure as, MkMeasure bs]

Prepend a Measure to a Section

> infixr %|
> (%|) :: Measure -> Section -> Section
> a %| (MkSection bs) = MkSection (a:bs)

Combine the Measures in two Sections to Create a Single Section

> infixr %++
> (%++) :: Section -> Section -> Section
> MkSection a %++ MkSection b = MkSection (a ++ b)

Build A section out of a single Measure

> (!%) :: Measure -> Section
> (!%) a = MkSection [a]

Operators to build the Score out of Sections
--------------------------------------------

Combine Two Sections to create a Score

> infixr ^^
> (^^) :: Section -> Section -> Score
> a ^^ b = MkScore (a:[b])

Prepend a Section to a Score

> infixr ^|
> (^|) :: Section -> Score -> Score
> a ^| MkScore bs = MkScore (a:bs)

Create a Score from a Single Section

> (!^) :: Section -> Score
> (!^) a = MkScore [a]




Testing:
--------

> testNoteA    = MkNote A (Oct 4) (Dur 4)
> testNoteB    = MkNote B (Oct 4) (Dur 4)

> testLayer1   = testNoteA    @@  testNoteB
> testLayer2   = testNoteA    @|  testLayer1
> testLayer3   = testLayer1   @++ testLayer2
> testLayer4   = (!@) testNoteA

> testStaff1   = testLayer1   ~~ testLayer2
> testStaff2   = testLayer1   ~|  testStaff1
> testStaff3   = testStaff1   ~++ testStaff2
> testStaff4   = (!~) testLayer1

> testMeasure1 = testStaff1   // testStaff2
> testMeasure2 = testStaff1   /|  testMeasure1
> testMeasure3 = testMeasure1 /++ testMeasure2
> testMeasure4 = (!/) testStaff1

> testSection1 = testMeasure1 %% testMeasure2
> testSection2 = testMeasure1 %|  testSection1
> testSection3 = testSection1 %++ testSection2
> testSection4 = (!%) testMeasure1

> testScore1   = testSection1 ^^ testSection2
> testScore2   = testSection1 ^|  testScore1
> testScore3   = (!^) testSection1


> testRender  = renderScore (MkScore [MkSection [MkMeasure [MkStaff [MkLayer [MkNote A (Oct 4) (Dur 4)]]]]])
> testRender2 = renderScore (MkScore [MkSection [MkMeasure [MkStaff [MkLayer [MkNote C (Oct 4) (Dur 2),
>                                                                             MkNote E (Oct 4) (Dur 2)],
>                                                                    MkLayer [MkNote E (Oct 4) (Dur 2),
>                                                                             MkNote G (Oct 4) (Dur 2)]]]]])



Example Piece
Mary had a little lamb Written in operator notation

> lamb = (!^) $  
>     ((!/) ( (!~) (  
>       (MkNote E (Oct 4) (Dur 4))
>       @| (MkNote D (Oct 4) (Dur 4))
>       @| (MkNote C (Oct 4) (Dur 4))
>       @@ (MkNote D (Oct 4) (Dur 4))
>       ))) %| (
>     (!/) ( (!~) (
>       (MkNote E (Oct 4) (Dur 4))
>       @| (MkNote E (Oct 4) (Dur 4))
>       @@ (MkNote E (Oct 4) (Dur 2))  
>       ))) %| (
>     (!/) ( (!~) (
>       (MkNote D (Oct 4) (Dur 4))
>       @| (MkNote D (Oct 4) (Dur 4))
>       @@ (MkNote D (Oct 4) (Dur 2))
>       ))) %| (
>     (!/) ( (!~) (
>       (MkNote E (Oct 4) (Dur 4))
>       @| (MkNote G (Oct 4) (Dur 4))
>       @@ (MkNote G (Oct 4) (Dur 2))
>       ))) %| (
>     (!/) ( (!~) (
>       (MkNote E (Oct 4) (Dur 4))
>       @| (MkNote D (Oct 4) (Dur 4))
>       @| (MkNote C (Oct 4) (Dur 4))
>       @@ (MkNote D (Oct 4) (Dur 4))
>       ))) %| (
>     (!/) ( (!~) (
>       (MkNote E (Oct 4) (Dur 4))
>       @| (MkNote E (Oct 4) (Dur 4))
>       @| (MkNote E (Oct 4) (Dur 4))
>       @@ (MkNote E (Oct 4) (Dur 4))
>       ))) %| (
>     (!/) ( (!~) (
>       (MkNote D (Oct 4) (Dur 4))
>       @| (MkNote D (Oct 4) (Dur 4))
>       @| (MkNote E (Oct 4) (Dur 4))
>       @@ (MkNote D (Oct 4) (Dur 4))
>       ))) %% (
>     (!/) ( (!~) ( (!@) (
>       (MkNote C (Oct 4) (Dur 1))
>       ))))

Example Piece
Mary Had a little lamb Written using Constructors

> lamb2 = 
>   MkScore [
>     MkSection [
>       MkMeasure [
>         MkStaff [
>           MkLayer [
>             (MkNote E (Oct 4) (Dur 4)),
>             (MkNote D (Oct 4) (Dur 4)),
>             (MkNote C (Oct 4) (Dur 4)),
>             (MkNote D (Oct 4) (Dur 4))]]],
>       MkMeasure [
>         MkStaff [
>           MkLayer [
>             (MkNote E (Oct 4) (Dur 4)),
>             (MkNote E (Oct 4) (Dur 4)),
>             (MkNote E (Oct 4) (Dur 2))]]], 
>       MkMeasure [
>         MkStaff [
>           MkLayer [
>             (MkNote D (Oct 4) (Dur 4)),
>             (MkNote D (Oct 4) (Dur 4)),
>             (MkNote D (Oct 4) (Dur 2))]]],
>       MkMeasure [
>         MkStaff [
>           MkLayer [
>             (MkNote E (Oct 4) (Dur 4)),
>             (MkNote G (Oct 4) (Dur 4)),
>             (MkNote G (Oct 4) (Dur 2))]]],
>       MkMeasure [
>         MkStaff [
>           MkLayer [
>             (MkNote E (Oct 4) (Dur 4)),
>             (MkNote D (Oct 4) (Dur 4)),
>             (MkNote C (Oct 4) (Dur 4)),
>             (MkNote D (Oct 4) (Dur 4))]]],
>       MkMeasure [
>         MkStaff [
>           MkLayer [
>             (MkNote E (Oct 4) (Dur 4)),
>             (MkNote E (Oct 4) (Dur 4)),
>             (MkNote E (Oct 4) (Dur 4)),
>             (MkNote E (Oct 4) (Dur 4))]]],
>       MkMeasure [
>         MkStaff [
>           MkLayer [
>             (MkNote D (Oct 4) (Dur 4)),
>             (MkNote D (Oct 4) (Dur 4)),
>             (MkNote E (Oct 4) (Dur 4)),
>             (MkNote D (Oct 4) (Dur 4))]]],
>       MkMeasure [
>         MkStaff [
>           MkLayer [
>            (MkNote C (Oct 4) (Dur 1))]]]]]

> chromaticScale = 
>   (!^) $ (!%) $ (!/) $ (!~) $
>   (MkNote C (Oct 4) (Dur 4))
>   @| (MkNote Db (Oct 4) (Dur 4))
>   @| (MkNote D  (Oct 4) (Dur 4))
>   @| (MkNote Eb (Oct 4) (Dur 4))
>   @| (MkNote E (Oct 4) (Dur 4))
>   @| (MkNote F (Oct 4) (Dur 4))
>   @| (MkNote Gb (Oct 4) (Dur 4))
>   @| (MkNote G (Oct 4) (Dur 4))
>   @| (MkNote Ab (Oct 4) (Dur 4))
>   @| (MkNote A (Oct 4) (Dur 4))
>   @| (MkNote Bb (Oct 4) (Dur 4))
>   @| (MkNote B (Oct 4) (Dur 4))
>   @@ (MkNote C (Oct 5) (Dur 4))





