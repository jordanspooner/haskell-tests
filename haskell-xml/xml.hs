module XML where

import Data.Char
import Data.Maybe

type Name = String

type Attributes = [(Name, String)]

data XML = Text String | Element Name Attributes [XML]
         deriving (Eq, Show)

type Stack = [XML]

-----------------------------------------------------------------------
-- Some useful show/print functions

-- The 'show' function for XML objects
showXML :: XML -> String
showXML (Text t)
  = t
showXML (Element n as es)
  = "<" ++ n ++ showAtts as ++ ">" ++ concatMap showXML es ++ "</" ++ n ++ ">"
  where
    showAtts       = concatMap showAtt
    showAtt (n, v) = " " ++ n ++ "=" ++ "\"" ++ v ++ "\""

-- The 'show' function for lists of XML objects
showXMLs :: [XML] -> String
showXMLs
  = concatMap showXML

-- Prints an XML object to the terminal
printXML :: XML -> IO()
printXML
  = putStrLn . showXML

-- Prints a list of XML objects to the terminal (useful for testing the
-- output from expandXML')
printXMLs :: [XML] -> IO()
printXMLs
  = mapM_ printXML

-----------------------------------------------------------------------
-----------------------------------------------------------------------
-- Part I

skipSpace :: String -> String
skipSpace
  = dropWhile isSpace

getAttribute :: String -> XML -> String
getAttribute s (Element _ as _)
  = fromMaybe "" (lookup s as)
getAttribute _ _
  = ""

getChildren :: String -> XML -> [XML]
getChildren s (Element _ _ (x@(Element n _ _) : xml))
  | n == s    = x : getChildren s (Element "" [] xml)
  | otherwise = getChildren s (Element "" [] xml)
getChildren s (Element _ _ (_ : xml))
  = getChildren s (Element "" [] xml)
getChildren _ _
  = []

getChild :: String -> XML -> XML
getChild s xml
  | null cs   = Text ""
  | otherwise = head cs
  where cs = getChildren s xml

addChild :: XML -> XML -> XML
-- Pre: the second argument is an Element
addChild c (Element n as xs)
  = Element n as (xs ++ [c])

getValue :: XML -> XML
getValue x
  = Text (getValue' x)
  where
    getValue' :: XML -> String
    getValue' (Element _ _ xml)
      = concatMap getValue' xml
    getValue' (Text t)
      = t

-------------------------------------------------------------------------
-- Part II

-- Parses an element/attribute name
parseName :: String -> (Name, String)
parseName []
  = error "Error: attempt to read empty name"
parseName s@(c : _)
  | isAlpha c = span isNameChar s
  | otherwise = error ("parseName error: name " ++ show s ++
                      " must begin with a letter")
  where
    isNameChar c = isAlpha c || isDigit c || elem c "-."

sentinel :: XML
sentinel
  = Element "" [] []

addText :: String -> Stack -> Stack
-- Pre: There is at least one Element on the stack
addText s (e : es)
  = addChild (Text s) e : es

popAndAdd :: Stack -> Stack
-- Pre: There are at least two Elements on the stack
popAndAdd (e1 : e2 : es)
  = addChild e1 e2 : es

parseAttributes :: String -> (Attributes, String)
-- Pre: The XML attributes string is well-formed
parseAttributes s
  | c == '>'  = ([], cs)
  | otherwise = ((name, value) : atts, rest'')
  where
    s'@(c : cs)    = skipSpace s
    (name, rest)   = parseName s'
    (value, rest') = break (== '\"') (tail (dropWhile (/= '\"') rest))
    (atts, rest'') = parseAttributes (tail rest')

parse :: String -> XML
-- Pre: The XML string is well-formed
parse s
  = parse' (skipSpace s) [sentinel]

parse' :: String -> Stack -> XML
parse' "" (Element _ _ (c : _) : _)
  = c
parse' ('<' : '/' : s) xs
  = parse' (tail (dropWhile (/= '>') s)) (popAndAdd xs)
parse' ('<' : s) xs
  = parse' s'' (Element n as [] : xs)
  where
    (n, s')   = break (\c -> c == '>' || isSpace c) (skipSpace s)
    (as, s'') = parseAttributes s'
parse' s xs
  = parse' s' (addText t xs)
  where
    (t, s') = break (== '<') s

-------------------------------------------------------------------------
-- Part III

type Context = XML

type XSL = XML

-- Parses XSL and XML source documents and transforms the latter using the
-- former. The output is written to the given file (String).
-- Example use:
--   output "out.html" filmsXSL films
-- To render output.html in a browser, type this at the Linux prompt:
--   firefox output.html &
output :: String -> XML -> XML -> IO()
output file xsl source
  = writeFile file (showXMLs (expandXSL xsl source))

expandXSL :: XSL -> XML -> [XML]
expandXSL xsl source
  = expandXSL' root xsl
  where
    root = Element "/" [] [source]

expandXSL' :: Context -> XSL -> [XML]
expandXSL' context x@(Element "value-of" _ _)
  = [expandVO (getAttribute "select" x) context]
  where
    expandVO :: String -> XML -> XML
    expandVO "" context
      = getValue context
    expandVO xpath context
      | xp == '@' = Text (getAttribute xps context)
      | xp == '.' = expandVO rest' context
      | otherwise = expandVO rest' (getChild current context)
      where
        (current, rest)  = break (== '/') xpath
        (xp : xps)       = current
        rest'            = dropWhile (== '/') rest
expandXSL' context x@(Element "for-each" _ xsl)
  = concat [concat [expandXSL' c x | x <- xsl]
           | c <- expandFE (getAttribute "select" x) [context]]
  where
    expandFE :: String -> [XML] -> [XML]
    expandFE "" contexts
      = contexts
    expandFE xpath contexts
      | xp == '@' = map (Text . getAttribute xps) contexts
      | xp == '.' = expandFE rest' contexts
      | otherwise = expandFE rest' (concatMap (getChildren current) contexts)
      where
        (current, rest)  = break (== '/') xpath
        (xp : xps)       = current
        rest'            = dropWhile (== '/') rest
expandXSL' context (Element n as xs)
  = [Element n as (concatMap (expandXSL' context) xs)]
expandXSL' _ (Text t)
  = [Text t]

-------------------------------------------------------------------------
-- Test data for Parts I and II

-- Simple test cases (no whitespace)
s1, s2, s3 :: String
s1
  = "<a>A</a>"
s2
  = "<a x=\"1\"><b>A</b><b>B</b></a>"
s3
  = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>"

-- Parsed versions of the above
x1, x2, x3 :: XML
x1
  = Element "a" [] [Text "A"]
x2
  = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3
  = Element "a"
            []
            [Element "b"
                     []
                     [Element "c"
                              [("att","att1")]
                              [Text "text1"],
                      Element "c"
                              [("att","att2")]
                              [Text "text2"]],
             Element "b"
                     []
                     [Element "c"
                              [("att","att3")]
                              [Text "text3"],
                      Element "d"
                              []
                              [Text "text4"]]]

casablanca :: String
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML
casablancaParsed
  = Element "film"
            [("title","Casablanca")]
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]

-- Films mark-up of Figure 1
films :: String
films
  = "<filmlist>\n\
    \  <film title = \"Rear Window\">\n\
    \    <director>Alfred Hitchcock</director>\n\
    \    <composer>Franz Waxman</composer>\n\
    \    <year>1954</year>\n\
    \  </film>\n\
    \  <film   title =  \"2001: A Space Odyssey\">\n\
    \    <director>Stanley Kubrick</director>\n\
    \    <composer>Richard Strauss</composer>\n\
    \    <composer>Gyorgy Ligeti</composer>\n\
    \    <composer>Johann Strauss</composer>\n\
    \    <year>1968</year>\n\
    \  </film>\n\
    \  <film title=\"Lawrence of Arabia\"  >\n\
    \    <duration>228</duration>\n\
    \    <director>David Lean</director>\n\
    \    <composer>Maurice Jarre</composer>\n\
    \  </film>\n\
    \</filmlist>\n\n\n"

-- Parsed version of films ('parse films'), suitably formatted
filmsParsed :: XML
filmsParsed
  = Element "filmlist"
            []
            [Text "\n  ",
             Element "film" [("title","Rear Window")]
                            [Text "\n    ",
                             Element "director" [] [Text "Alfred Hitchcock"],
                             Text "\n    ",
                             Element "composer" [] [Text "Franz Waxman"],
                             Text "\n    ",
                             Element "year" [] [Text "1954"],
                             Text "\n  "],
             Text "\n  ",
             Element "film" [("title","2001: A Space Odyssey")]
                            [Text "\n    ",
                             Element "director" [] [Text "Stanley Kubrick"],
                             Text "\n    ",
                             Element "composer" [] [Text "Richard Strauss"],
                             Text "\n    ",
                             Element "composer" [] [Text "Gyorgy Ligeti"],
                             Text "\n    ",
                             Element "composer" [] [Text "Johann Strauss"],
                             Text "\n    ",
                             Element "year" [] [Text "1968"],
                             Text "\n  "],
             Text "\n  ",
             Element "film" [("title","Lawrence of Arabia")]
                            [Text "\n    ",
                             Element "duration" [] [Text "228"],
                             Text "\n    ",
                             Element "director" [] [Text "David Lean"],
                             Text "\n    ",
                             Element "composer" [] [Text "Maurice Jarre"],
                             Text "\n  "],
             Text "\n"]

-------------------------------------------------------------------------
-- XSL tests

-- value-of test cases
xsl1, xsl2, xsl3, xsl4, xsl5, xsl6, xsl7,
  xsl8, xsl9 :: String
xsl1
  = "<value-of select = \"a/b/c\"></value-of>"
xsl2
  = "<value-of select = \"a/b\"></value-of>"
xsl3
  = "<value-of select = \"a/b/d\"></value-of>"
xsl4
  = "<value-of select = \"a/b/c/@att\"></value-of>"
xsl5
  = "<value-of select = \"./a/./b/c/./.\"></value-of>"
xsl6
  = "<t1><t2>Preamble</t2><t3><value-of select = \"a/b/c\"></value-of></t3></t1>"

-- for-each test cases
xsl7
  = "<for-each select=\"a/b/c\"><value-of select=\"./@att\"></value-of>\
    \</for-each>"
xsl8
  = "<for-each select=\"a/b\"><t><value-of select=\"c\"></value-of></t>\
    \</for-each>"
xsl9
  = "<for-each select=\"a/b\"><t1><value-of select=\"absent\"></value-of>\
    \</t1></for-each>"

-- Parsed versions of the above
xsl1Parsed, xsl2Parsed, xsl3Parsed, xsl4Parsed, xsl5Parsed,
  xsl6Parsed, xsl7Parsed, xsl8Parsed, xsl9Parsed :: XML
xsl1Parsed
  = Element "value-of" [("select","a/b/c")] []
xsl2Parsed
  = Element "value-of" [("select","a/b")] []
xsl3Parsed
  = Element "value-of" [("select","a/b/d")] []
xsl4Parsed
  = Element "value-of" [("select","a/b/c/@att")] []
xsl5Parsed
  = Element "value-of" [("select","./a/./b/c/./.")] []
xsl6Parsed
  = Element "t1"
            []
            [Element "t2" [] [Text "Preamble"],
             Element "t3" [] [Element "value-of" [("select","a/b/c")] []]]

xsl7Parsed
  = Element "for-each"
            [("select","a/b/c")]
            [Element "value-of" [("select","./@att")] []]
xsl8Parsed
  = Element "for-each"
            [("select","a/b")]
            [Element "t" [] [Element "value-of" [("select","c")] []]]
xsl9Parsed
  = Element "for-each"
            [("select","a/b")]
            [Element "t1" [] [Element "value-of" [("select","absent")] []]]

-- XSL template for building a films summary (example from spec.)
filmsXSL :: String
filmsXSL
  = "<html>\n\
    \<body>\n\
    \  <h2>Film List</h2>\n\
    \  <table border=\"1\">\n\
    \    <tr>\n\
    \      <th align=\"left\">Title</th>\n\
    \      <th align=\"left\">Director</th>\n\
    \      <th align=\"left\">Principal composer</th>\n\
    \    </tr>\n\
    \    <for-each select=\"filmlist/film\">\n\
    \      <tr>\n\
    \        <td><value-of select=\"@title\"></value-of></td>\n\
    \        <td><value-of select=\"director\"></value-of></td>\n\
    \        <td><value-of select=\"composer\"></value-of></td>\n\
    \      </tr>\n\
    \    </for-each>\n\
    \  </table>\n\
    \</body>\n\
    \</html>"

-- XSL template for building a list of composers (example from spec.)
composersXSL :: String
composersXSL
  = "<for-each select=\"filmlist/film\">\
      \<h2><value-of select=\"@title\"></value-of> composers</h2>\
      \<ul>\
      \<for-each select=\"composer\">\
        \<li><value-of select=\".\"></value-of></li>\
      \</for-each>\
      \</ul>\
    \</for-each>"
