> module Mei where
> import Prelude hiding ((^^))

Render Class:
  Items that are renderable are members of the render class.
  Only the rend method is required to be implemented in order to be
  a member of the render class

> class Render a where
>   render  :: [a] -> String
>   render a = concat $ zipWith rend [1..] a
>
>   rend    :: Int -> a -> String

Helper to function to indent during the render process

> indent  :: Int -> String
> indent n = '\n': replicate (n*2) ' '


NoteName:
  The names of all the notes (currently excluding Sharps) from C to B;
  in MEI C is the lowest note and B the highest for a given Octave.
  NoteNames are enumerated so that shifting of the notes can be performed
  more easily

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
  It may be reasonable to limit the number range in the future
  Inspiration for Smar Constructor: https://wiki.haskell.org/Smart_constructors

> data Octave = Oct Int

> instance Show Octave where
>   show (Oct a) = show a

> oct                     :: Int -> Octave
> oct n | (elem n [0..10]) = Oct n
> oct _                    = error "Invalid value for Octave"


Duration:
  Duration can be powers of 2 up to 256
  Inspiration for Smar Constructor: https://wiki.haskell.org/Smart_constructors

> data Duration = Dur Int

> instance Show Duration where
>   show (Dur a) = show a

> dur                                          :: Int -> Duration
> dur n | (elem n (zipWith (^) [2,2..] [0..8])) = Dur n
> dur _                                         = error "Invalid value for Duration"


Note:
  A Note is either a Singe Note, a Rest, or a Chord
  A Single Note is made up of a combinations of a NoteName, Octave, Duration
    The NoteName and Octave work together to place the node on the staff
    The Duration specifies what type of note it is (Ex: 4 = Quarter Note)
  A Rest ...
  A Chord

> data Note = MkNote {name :: NoteName, octave :: Octave, duration :: Duration}
>           | MkRest {duration :: Duration}
>           | MkChord { nts :: [Note] }

> instance Show Note where
>   show (MkNote name octave duration) = 
>     "(" ++ show name ++ " " ++ show octave ++ " " ++ show duration ++ ")"
>   show (MkRest duration) =
>     "(rest " ++ show duration ++ ")"
>   show (MkChord notes) =
>     "(chord " ++ show notes ++ ")"

> instance Render Note where
>   rend n (MkNote name octave duration) = indent 8 ++ "<note "
>                                       ++ rend 0 name  
>                                       ++ "oct=\"" ++ show octave   ++ "\" "
>                                       ++ "dur=\"" ++ show duration ++ "\" />"
>   rend n (MkRest duration)             = indent 8 ++ "<rest "
>                                       ++ "dur=\"" ++ show duration ++ "\" />"
>   rend n (MkChord notes)               = indent 8 ++ "<chord>"
>                                       ++ render notes
>                                       ++ indent 8 ++ "</chord>"


Layer:
  A Layer is comprised of one or more Notes
  Chords can be formed by specifying notes on different layers that occur
  at the same time.

> data Layer   = MkLayer { notes :: [Note] }
>   deriving Show

> instance Render Layer where
>   rend n layer = indent 7 ++ "<layer n=\"" ++ show n ++ "\">"
>               ++ render (notes layer)
>               ++ indent 7 ++ "</layer>"


Staff:
  A Staff is comprised of one or more Layers
  Multiple Staffs can be used to specify multipl clefs or instruments that are
  being played at the same time

> data Staff   = MkStaff { layers :: [Layer] }
>   deriving Show

> instance Render Staff where
>   rend n staff = indent 6 ++ "<staff n=\"" ++ show n ++ "\">"
>               ++ render (layers staff)
>               ++ indent 6 ++ "</staff>"


Measure:
  A Measure is comprised of one or more Staffs
  Measures are the primary building blocks for Sections

> data Measure = MkMeasure { staffs :: [Staff] }
>   deriving Show

> instance Render Measure where
>   rend n measure = indent 5 ++ "<measure n=\"" ++ show n ++ "\">"
>                 ++ render (staffs measure)
>                 ++ indent 5 ++ "</measure>"


Section:
  A Section is comprised of one or more Measures
  Sections are the primary building blocks for Scores

> data Section = MkSection { measures :: [Measure] } 
>   deriving Show

> instance Render Section where
>   rend n section = indent 4 ++ "<section>"
>                 ++ render (measures section)
>                 ++ indent 4 ++ "</section>"

Score:
  A Score is comprised of one or more Sections.
  Currently there is no need to have more than one section.

> data Score   = MkScore { {-- scoreDef :: ScoreDef, --} sections :: [Section] }
>   deriving Show

> instance Render Score where
>   rend n score = indent 0 ++ "<music>" ++ indent 1 ++ "<body>" ++ indent 2 ++ "<mdiv>" ++ indent 3 ++ "<score>"
>                   ++ show MkScoreDef
>                   ++ render (sections score)
>                   ++ indent 3 ++ "</score>" ++ indent 2 ++ "</mdiv>" ++ indent 1 ++ "</body>" ++ indent 0 ++ "</music>"


> renderScore  :: Score -> String
> renderScore s = rend 0 s

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
> (@@)             :: Note -> Note -> Layer
> noteA @@ noteB    = MkLayer [noteA, noteB]

Cons a Note to a Layer

> infixr @|
> (@|)             :: Note -> Layer -> Layer
> note @| layer     = MkLayer $ note:(notes layer)

Combine the notes from two layers together to create a single layer

> infixr @++
> (@++)            :: Layer -> Layer -> Layer
> layerA @++ layerB = MkLayer $ (notes layerA) ++ (notes layerB) 

Build a Layer out of a Single Note

> (!@)             :: Note -> Layer
> (!@) note         = MkLayer [note]


Operators to build Staffs out of Layers and Staffs
--------------------------------------------------

Combine Two Layers together to make a Staff

> infixr ~~
> (~~)             :: Layer -> Layer -> Staff
> layerA ~~ layerB  = MkStaff [layerA, layerB]

Cons a Layer to a Staff

> infixr ~|
> (~|)             :: Layer -> Staff -> Staff
> layer ~| staff    = MkStaff $ layer:(layers staff)

Combine the Layers from tow Staffs to make a Single Staff

> infixr ~++
> (~++)            :: Staff -> Staff -> Staff
> staffA ~++ staffB = MkStaff $ (layers staffA ++ layers staffB)

Create a Staff from a single Layer

> (!~)             :: Layer -> Staff
> (!~) layer        = MkStaff [layer]

Operators to build Measures out of Staffs and Measures
------------------------------------------------------

Combine Two staffs to make a Measure

> infixr //
> (//)                 :: Staff -> Staff -> Measure
> staffA // staffB      = MkMeasure [staffA, staffB]

Cons a Measure to a Staff

> infixr /|
> (/|)                 :: Staff -> Measure -> Measure
> staff /| measure      = MkMeasure $ staff:(staffs measure)

Combine the Staffs in two Measures to make a single Measure

> infixr /++
> (/++)                :: Measure -> Measure -> Measure
> measureA /++ measureB = MkMeasure $ (staffs measureA) ++ (staffs measureB)

Create a Measure from a single Staff

> (!/)                 :: Staff -> Measure
> (!/) staff            = MkMeasure [staff]


Operators to build Sections out of Measures and Sections
--------------------------------------------------------

Combine two Measures to make a Section

> infixr %%
> (%%)                 :: Measure -> Measure -> Section
> measureA %% measureB  = MkSection [measureA, measureB]

Cons a Measure to a Section

> infixr %|
> (%|)                 :: Measure -> Section -> Section
> measure %| section    = MkSection $ measure:(measures section)

Combine the Measures in two Sections to Create a Single Section

> infixr %++
> (%++)                :: Section -> Section -> Section
> sectionA %++ sectionB = MkSection $ (measures sectionA) ++ (measures sectionB)

Create a Section from a single Measure

> (!%)                 :: Measure -> Section
> (!%) measure          = MkSection [measure]


Operators to build the Score out of Sections
--------------------------------------------

Combine Two Sections to create a Score

> infixr ^^
> (^^)                 :: Section -> Section -> Score
> sectionA ^^ sectionB  = MkScore [sectionA, sectionB]

Cons a Section to a Score

> infixr ^|
> (^|)                 :: Section -> Score -> Score
> section ^| score      = MkScore $ section:(sections score)

Create a Score from a Single Section

> (!^)                 :: Section -> Score
> (!^) section          = MkScore [section]




Testing:
--------

> testNoteA    = MkNote A (oct 4) (dur 4)
> testNoteB    = MkNote B (oct 4) (dur 4)

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


> testRender  = renderScore (MkScore [MkSection [MkMeasure [MkStaff [MkLayer [MkNote A (oct 4) (dur 4)]]]]])
> testRender2 = renderScore (MkScore [MkSection [MkMeasure [MkStaff [MkLayer [MkNote C (oct 4) (dur 2),
>                                                                             MkNote E (oct 4) (dur 2)],
>                                                                    MkLayer [MkNote E (oct 4) (dur 2),
>                                                                             MkNote G (oct 4) (dur 2)]]]]])



Example Piece
Mary had a little lamb Written in operator notation

> lamb = (!^) $  
>     ((!/) ( (!~) (  
>       (MkNote E (oct 4) (dur 4))
>       @| (MkNote D (oct 4) (dur 4))
>       @| (MkNote C (oct 4) (dur 4))
>       @@ (MkNote D (oct 4) (dur 4))
>       ))) %| (
>     (!/) ( (!~) (
>       (MkNote E (oct 4) (dur 4))
>       @| (MkNote E (oct 4) (dur 4))
>       @@ (MkNote E (oct 4) (dur 2))  
>       ))) %| (
>     (!/) ( (!~) (
>       (MkNote D (oct 4) (dur 4))
>       @| (MkNote D (oct 4) (dur 4))
>       @@ (MkNote D (oct 4) (dur 2))
>       ))) %| (
>     (!/) ( (!~) (
>       (MkNote E (oct 4) (dur 4))
>       @| (MkNote G (oct 4) (dur 4))
>       @@ (MkNote G (oct 4) (dur 2))
>       ))) %| (
>     (!/) ( (!~) (
>       (MkNote E (oct 4) (dur 4))
>       @| (MkNote D (oct 4) (dur 4))
>       @| (MkNote C (oct 4) (dur 4))
>       @@ (MkNote D (oct 4) (dur 4))
>       ))) %| (
>     (!/) ( (!~) (
>       (MkNote E (oct 4) (dur 4))
>       @| (MkNote E (oct 4) (dur 4))
>       @| (MkNote E (oct 4) (dur 4))
>       @@ (MkNote E (oct 4) (dur 4))
>       ))) %| (
>     (!/) ( (!~) (
>       (MkNote D (oct 4) (dur 4))
>       @| (MkNote D (oct 4) (dur 4))
>       @| (MkNote E (oct 4) (dur 4))
>       @@ (MkNote D (oct 4) (dur 4))
>       ))) %% (
>     (!/) ( (!~) ( (!@) (
>       (MkNote C (oct 4) (dur 1))
>       ))))

Example Piece
Mary Had a little lamb Written using Constructors

> lamb2 = 
>   MkScore [
>     MkSection [
>       MkMeasure [
>         MkStaff [
>           MkLayer [
>             (MkNote E (oct 4) (dur 4)),
>             (MkNote D (oct 4) (dur 4)),
>             (MkNote C (oct 4) (dur 4)),
>             (MkNote D (oct 4) (dur 4))]]],
>       MkMeasure [
>         MkStaff [
>           MkLayer [
>             (MkNote E (oct 4) (dur 4)),
>             (MkNote E (oct 4) (dur 4)),
>             (MkNote E (oct 4) (dur 2))]]], 
>       MkMeasure [
>         MkStaff [
>           MkLayer [
>             (MkNote D (oct 4) (dur 4)),
>             (MkNote D (oct 4) (dur 4)),
>             (MkNote D (oct 4) (dur 2))]]],
>       MkMeasure [
>         MkStaff [
>           MkLayer [
>             (MkNote E (oct 4) (dur 4)),
>             (MkNote G (oct 4) (dur 4)),
>             (MkNote G (oct 4) (dur 2))]]],
>       MkMeasure [
>         MkStaff [
>           MkLayer [
>             (MkNote E (oct 4) (dur 4)),
>             (MkNote D (oct 4) (dur 4)),
>             (MkNote C (oct 4) (dur 4)),
>             (MkNote D (oct 4) (dur 4))]]],
>       MkMeasure [
>         MkStaff [
>           MkLayer [
>             (MkNote E (oct 4) (dur 4)),
>             (MkNote E (oct 4) (dur 4)),
>             (MkNote E (oct 4) (dur 4)),
>             (MkNote E (oct 4) (dur 4))]]],
>       MkMeasure [
>         MkStaff [
>           MkLayer [
>             (MkNote D (oct 4) (dur 4)),
>             (MkNote D (oct 4) (dur 4)),
>             (MkNote E (oct 4) (dur 4)),
>             (MkNote D (oct 4) (dur 4))]]],
>       MkMeasure [
>         MkStaff [
>           MkLayer [
>            (MkNote C (oct 4) (dur 1))]]]]]

> chromaticScale = 
>   (!^) $ (!%) $ (!/) $ (!~) $
>   (MkNote C (oct 4) (dur 4))
>   @| (MkNote Db (oct 4) (dur 4))
>   @| (MkNote D  (oct 4) (dur 4))
>   @| (MkNote Eb (oct 4) (dur 4))
>   @| (MkNote E (oct 4) (dur 4))
>   @| (MkNote F (oct 4) (dur 4))
>   @| (MkNote Gb (oct 4) (dur 4))
>   @| (MkNote G (oct 4) (dur 4))
>   @| (MkNote Ab (oct 4) (dur 4))
>   @| (MkNote A (oct 4) (dur 4))
>   @| (MkNote Bb (oct 4) (dur 4))
>   @| (MkNote B (oct 4) (dur 4))
>   @@ (MkNote C (oct 5) (dur 4))
