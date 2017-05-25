> import Mei

> {--
> header :: String
> header = ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
>   + "<mei xmlns=\"http://www.music-encoding.org/ns/mei\" meiversion=\"3.0.0\">"
>   + "<meiHEad>"
>   + "  <fileDesc>"
>   + "    <titleStmt>"
>   + "      <title>Test</title>"
>   + "    </titleStmt>"
>   + "    <pubStmt/>"
>   + "    <seriesStmt>"
>   + "    <title>Test</title>"
>   + "  </fileDesc>"
>   + "</meiHead>"
>   )
> --}

> renderScore             :: Score -> String
> renderScore (MkScore a) = ("<music><body><mdiv><score>"
>                         ++ (concat (map renderSection a))
>                         ++ "</score></mdiv></body></music>")

> renderSection               :: Section -> String
> renderSection (MkSection a) = ("<section>"
>                             ++ (concat (map renderMeasure a))
>                             ++ "</section>")

> renderMeasure                 :: Measure -> String
> renderMeasure (MkMeasure n b) = ("<measure n=\"" ++ show n ++ "\">"
>                               ++ (concat (map renderStaff b))
>                               ++ "</measure>")

> renderStaff               :: Staff -> String
> renderStaff (MkStaff n b) = ("<staff n=\"" ++ show n ++ "\">"
>                           ++ (concat (map renderLayer b))
>                           ++ "</staff>")

> renderLayer               :: Layer -> String
> renderLayer (MkLayer n b) = ("<layer n=\"" ++ show n ++ "\">"
>                           ++ (concat (map renderNote b))
>                           ++ "</layer>")

> renderNote                  :: Note -> String
> renderNote (MkNote a b c d) = ("<note "
>                             ++ "pname=\"" ++ show a ++ "\" "
>                             ++ "oct=\""   ++ show b ++ "\" "
>                             ++ "dur=\""   ++ show c ++ "\" "
>                             ++ "accid=\"" ++ show d ++ "\">")

> test = renderScore (MkScore [MkSection [MkMeasure 1 [MkStaff 1 [MkLayer 1 [MkNote A (Oct 4) (Dur 4) None]]]]])

