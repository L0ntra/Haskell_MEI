> module Mei where
> import Prelude hiding ((^^)) --TODO: Find  better symbol to use for Scores


MeiElement Class:
-----------------
Items that are renderable are members of the render class.
Only the rend method is required to be implemented in order to be
a member of the render class

> class MeiElement a where
>   render    :: [a] -> String
>   render a   = concat $ zipWith rend [1..] a
>
>   rend      :: Int -> a -> String
>   transpose :: (Int -> Int) -> a -> a
>   harmonize :: (Int -> Int) -> a -> a


Helper to function to indent during the render process.
Indentation for render functions can generally use a static value
due to the consistant, nested structure of XML.
(Eg: All tags of a particular type are indented the same amount)
TODO: If additional, optional layers of the MEI standard are 
implementd, the indentation strategy will need to be revisited

> indent  :: Int -> String
> indent n = '\n': replicate (n*2) ' '



DATATYPES TO DEFINE THE STRUCTURE OF A MUSICAL SCORE:
=====================================================

NoteName:
---------
The names of all the notes (currently excluding Sharps) from C to B;
in MEI C is the lowest note and B the highest for a given Octave.
NoteNames are enumerated so that shifting of the notes can be performed
more easily

> data NoteName =      C -- | Cs --TODO: Figure out a way to allow for sharps
>               | Db | D -- | Ds --      and other accidentals
>               | Eb | E
>                    | F -- | Fs
>               | Gb | G -- | Gs
>               | Ab | A -- | As
>               | Bb | B
>    deriving (Show, Enum, Eq)

> renderNoteName :: NoteName -> String
> renderNoteName C  = "pname=\"c\" "
> -- renderNoteName Cs = "pname=\"c\" accid=\"s\" "
> renderNoteName Db = "pname=\"d\" accid=\"f\" "
> renderNoteName D  = "pname=\"d\" "
> -- renderNoteName Ds = "pname=\"d\" accid=\"s\" "
> renderNoteName Eb = "pname=\"e\" accid=\"f\" "
> renderNoteName E  = "pname=\"e\" "
> renderNoteName F  = "pname=\"f\" "
> -- renderNoteName Fs = "pname=\"f\" accid=\"s\" "
> renderNoteName Gb = "pname=\"g\" accid=\"f\" "
> renderNoteName G  = "pname=\"g\" "
> -- renderNoteName Gs = "pname=\"g\" accid=\"s\" "
> renderNoteName Ab = "pname=\"a\" accid=\"f\" "
> renderNoteName A  = "pname=\"a\" "
> -- renderNoteName As = "pname=\"a\" accid=\"s\" "
> renderNoteName Bb = "pname=\"b\" accid=\"f\" "
> renderNoteName B  = "pname=\"b\" "

> namesToEnum :: [NoteName] -> [Int]
> namesToEnum  = map fromEnum

> enumToNames   :: [Int] -> [NoteName]
> enumToNames ns = [(toEnum a) :: NoteName | a <- ns ]


Octave:
-------
An Octave can be any natural number in the range [0..10].
It may be reasonable to expand or contract this range to to future experience
Inspiration for Smart Constructor: https://wiki.haskell.org/Smart_constructors

> data Octave = Oct { octa :: Int }

> instance Show Octave where
>   show (Oct a) = show a

> oct                     :: Int -> Octave
> oct n | (elem n [0..10]) = Oct n
> oct _                    = error "Invalid value for Octave"


Duration:
---------
Duration can be powers of 2 up to 256
Inspiration for Smart Constructor: https://wiki.haskell.org/Smart_constructors

> data Duration = Dur Int

> instance Show Duration where
>   show (Dur a) = show a

> dur                                          :: Int -> Duration
> dur n | (elem n (zipWith (^) [2,2..] [0..8])) = Dur n
> dur _                                         = error "Invalid value for Duration"


Note:
-----
A Note is either a Singe Note, a Rest, or a Chord:
  A Single Note is made up of a combinations of a NoteName, Octave, Duration
    The NoteName and Octave work together to place the node on the staff
    The Duration specifies what type of note it is (Ex: 4 = Quarter Note)
  A Rest ... TODO:
  A Chord ... TODO:
Notes are the primary building blocks for Layers

> data Note = MkNote {name :: NoteName, octave :: Octave, duration :: Duration}
>           | MkRest {duration :: Duration}
>           | MkChord { nts :: [(NoteName, Octave)], duration :: Duration }

> instance Show Note where
>   show (MkNote name octave duration) = "(" ++ show name ++ " " ++ show octave ++ " " ++ show duration ++ ")"
>   show (MkRest duration)             = "(rest " ++ show duration ++ ")"
>   show (MkChord notes duration)      = "(chord ( " ++ concat (map (\ n -> show (fst n) ++  show (snd n) ++ " ") notes) 
>                                     ++ ") " ++ show duration ++ ")"

> instance MeiElement Note where
>   --                                  :: Int -> Note -> String
>   rend n (MkNote name octave duration) = indent 8 ++ "<note "
>                                       ++ renderNoteName name  
>                                       ++ "oct=\"" ++ show octave   ++ "\" "
>                                       ++ "dur=\"" ++ show duration ++ "\" />"
>   rend n (MkRest duration)             = indent 8 ++ "<rest "
>                                       ++ "dur=\"" ++ show duration ++ "\" />"
>   rend n (MkChord notes duration)      = indent 8 ++ "<chord dur=\"" ++ show duration ++ "\">"
>                                       ++ concat (map (\ nte -> indent 9 
>                                                             ++ "<note " ++ renderNoteName (fst nte) 
>                                                             ++ "oct=\"" ++ show (snd nte) ++ "\" />") notes)
>
>                                       ++ indent 8 ++ "</chord>"
>
>   --                                  :: (Int -> Int) -> Note -> Note
>   harmonize sft (MkNote nt o d)        = notesToChord [MkNote nt o d, transpose sft (MkNote nt o d)]
>   harmonize _   n                      = n -- Catches the cases where harmonization is note possible Ex: Chord, Rest
>
>   --                                  :: (Int -> Int) -> Note -> Note
>   transpose sft (MkChord nts d)        = MkChord (map (\ (n, o) -> (toEnum ((sft (fromEnum n) ) `mod` 11) ::NoteName, 
>                                                                    (oct ((octa o) + ((sft (fromEnum n)) `div` 11))))) nts) d 
>   transpose sft (MkNote nt o d)        = MkNote  (toEnum ((sft ( fromEnum nt) ) `mod` 11) :: NoteName) 
>                                                  (oct ((octa o) + ((sft (fromEnum nt)) `div` 11))) d
>   transpose _ n                        = n -- Catches the cases where shiftig does nothing Ex: Rest

Takes a list of indivdual notes and makes a chord out of them
TODO: Allow Notes to be added to a Cords
  Ex: [Chord [A, B] 4, Note C 4] -> Chord [A, B, C] 4
TODO: Allow Chords to be added to Chords
  Ex: [Chord [A, B] 4, Chord [C, D] 4] -> Chord [A, B, C, D] 4

> notesToChord :: [Note] -> Note
> notesToChord ((MkRest duration):notes)      = error "Rests cannot be part of a Chord"
> notesToChord ((MkChord nns duration):notes) = error "Connot combine Chords to make Chords"
> notesToChord notes                          = MkChord (map (\n -> (name n, octave n) ) notes) (duration (head notes)) 


Layer:
------
A Layer is comprised of one or more Notes.
Layers are the primary building blocks for Staffs.

> data Layer = MkLayer { notes :: [Note] }
>   deriving Show

> instance MeiElement Layer where
>   --             :: Int -> Layer -> String
>   rend n layer    = indent 7 ++ "<layer n=\"" ++ show n ++ "\">"
>                  ++ render (notes layer)
>                  ++ indent 7 ++ "</layer>"
>
>   --             :: (Int -> Int) -> Layer -> Layer
>   harmonize sft l = MkLayer (map (harmonize sft) (notes l))
>
>   --             :: (Int -> Int) -> Layer -> Layer
>   transpose sft n = MkLayer (map (transpose sft) (notes n))

Combine the notes in two layers into a Layer of Chords
TODO: Implement a method to check that the notes between the two layers are the same duration
      If they are then make chords out of them
      If they are not then leave them as two seporate layers

> layersToChord                        :: Layer -> Layer -> Layer
> layersToChord layerA layerB = MkLayer $ map notesToChord [[a, b] | (a, b) <- zip (notes layerA) (notes layerB)]


Staff:
------
A Staff is comprised of one or more Layers.
Staffs are the primary building blocs for Measures.
Multiple Staffs can be used to specify multiple clefs or instruments that are
being played at the same time.
Currently, with out ScoreDef being implementd, only a single staff is supported.
TODO: Implement ScoreDef to enable the use of multiple Staffs

> data Staff = MkStaff { layers :: [Layer] }
>   deriving Show

> instance MeiElement Staff where
>   --             :: Int -> Staff -> String
>   rend n staff    = indent 6 ++ "<staff n=\"" ++ show n ++ "\">"
>                  ++ render (layers staff)
>                  ++ indent 6 ++ "</staff>"
>
>   --             :: (Int -> Int) -> Staff -> Staff
>   harmonize sft s = MkStaff (map (harmonize sft) (layers s))
>
>   --             :: (Int -> Int) -> Staff -> Staff
>   transpose sft s = MkStaff (map (transpose sft) (layers s))


Measure:
--------
A Measure is comprised of one or more Staffs.
Measures are the primary building blocks for Sections.

> data Measure = MkMeasure { staffs :: [Staff] }
>   deriving Show

> instance MeiElement Measure where
>   --             :: Int -> Measure -> String
>   rend n measure  = indent 5 ++ "<measure n=\"" ++ show n ++ "\">"
>                  ++ render (staffs measure)
>                  ++ indent 5 ++ "</measure>"
>
>   --             :: (Int -> Int) -> Measure -> Measure
>   harmonize sft m = MkMeasure (map (harmonize sft) (staffs m))
>   
>   --             :: (Int -> Int) -> Measure -> Measure
>   transpose sft m = MkMeasure (map (transpose sft) (staffs m))


Section:
--------
A Section is comprised of one or more Measures.
Sections are the primary building blocks for Scores.

> data Section = MkSection { measures :: [Measure] } 
>   deriving Show

> instance MeiElement Section where
>   --             :: Int -> Section -> String
>   rend n section  = indent 4 ++ "<section>"
>                  ++ render (measures section)
>                  ++ indent 4 ++ "</section>"
>
>   --             :: (Int -> Int) -> Section -> Section
>   harmonize sft s = MkSection (map (harmonize sft) (measures s))
>
>   --             :: (Int -> Int) -> Section -> Section
>   transpose sft s = MkSection (map (transpose sft) (measures s))


Score:
------
A Score is comprised of one or more Sections.
Currently there is no need to have more than one section.

> data Score   = MkScore { {-- scoreDef :: ScoreDef, --} sections :: [Section] }
>   deriving Show

> instance MeiElement Score where
>   --              :: Int -> Section -> String
>   rend n score     = indent 0 ++ "<music>" ++ indent 1 ++ "<body>" ++ indent 2 ++ "<mdiv>" ++ indent 3 ++ "<score>"
>                   ++ show MkScoreDef
>                   ++ render (sections score)
>                   ++ indent 3 ++ "</score>" ++ indent 2 ++ "</mdiv>" ++ indent 1 ++ "</body>" ++ indent 0 ++ "</music>"
>
>   --             :: (Int -> Int) -> Staff -> Staff
>   harmonize sft s = MkScore (map (harmonize sft) (sections s))
>
>   --             :: (Int -> Int) -> Staff -> Staff
>   transpose sft s = MkScore (map (transpose sft) (sections s))

Wrapper function for rendering the Score.

> renderScore  :: Score -> String
> renderScore s = rend 0 s



ScoreDef:
---------
The Score Def specifies:
  The Key signature
  The Time signature as count/unit == 4/4
  The Clef of each Staff
TODO: Allow the values outlined above to be dynamically set

> data ScoreDef = MkScoreDef

> instance Show ScoreDef where
>   show a = indent 4 ++ "<scoreDef "
>            ++ "key.sig=\"0\" "
>            ++ "meter.count=\"4\" "
>            ++ "meter.unit=\"4\">"
>               ++ indent 5 ++ "<staffGrp>"
>                  ++ indent 6 ++ "<staffDef "
>                     ++ "clef.shape=\"G\" "
>                     ++ "clef.line=\"2\" "
>                     ++ "n=\"1\" "
>                     ++ "lines=\"5\" />"
>               ++ indent 5 ++ "</staffGrp>"
>            ++ indent 4 ++ "</scoreDef>"
>



OPERATORS TO BUILD A MUSICAL SCORES
===================================

TODO: Further Simplify the Opertors to make writing a piece more intuitive
These operators are intended to make the composition of musical pieces easier
for more experienced users that will use them frequently.
New composers my find it easier to explicitly use the Constructors for the different
musical building blocks

The Symbols adhear to the following pattern :
__  :: a -> a -> b -- Combines two sub elements of type a to from the sub elemnts of an element of type b
_|  :: a -> b -> b -- Cons a sub element of type a into the sub elements of an element of type b
_++ :: b -> b -> b -- Concatinates the sub elements an element of type b to create a new element of type b
_!  :: a -> b      -- Creates an element of type b with the single sub element of type a

The Sybols reference the result of the operations:
_ -> Result of Operation
@ -> Layer
~ -> Staff
/ -> Measure
% -> Section
^ -> Score

Operator Laws:                                      Annotated as lists
a _| (b __ c) == (a __ b) _++ ((_!) c)              a : [b, c]       == [a, b] ++ [c]  == [a, b, c]
a _| []       == _! a                               a : []           == [a]
(a __ b) _++ (c __ d) == a _| b _| c _| (_! d)      [a, b] ++ [c, d] == a:b:c:[d]      == [a, b, c, d]


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



TESTING:
========

> testNoteA    = MkNote C (oct 4) (dur 4)
> testNoteB    = MkNote D (oct 4) (dur 4)

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
>     (!/) ( (!~) (
>       (MkNote C (oct 4) (dur 2))
>       @@ (MkRest (dur 2))
>       )))

Example Piece
Mary Had a little lamb Written using Constructors
(lamb == lamb2)

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
>            (MkNote C (oct 4) (dur 2)),
>            (MkRest (dur 2))]]]]]

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
