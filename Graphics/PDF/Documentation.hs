{- | Quick documentation for the PDF library.

For detailed examples, download the tar.gz package from Hackage and look at the
test.hs in folder Test.

-}
module Graphics.PDF.Documentation(
	-- * Creating a document
	-- $creating

	-- * Adding pages
	-- $pages

	-- * Creating the page content 
	-- $content

	-- * Text 
	-- $text

	-- ** MonadStyle 
	-- $monadstyle

	-- * Geometry 
	-- $geometry

	-- * X Form 
	-- $xform

	-- * Image 
	-- $image

	-- * Annotations 
	-- $annotations

	-- * Warning 
	-- $warning
	) where 

{- $creating 

When you create a document, you must give some information for the PDF file like the author,
the default size (the pages can use different sizes if specified) and if the document is compressed.

So, a standard way to start a PDF document is with:

@
main :: IO()
main = do
    let rect = 'PDFRect' 0 0 600 400
    'runPdf' \"demo.pdf\" ('standardDocInfo' { author='toPDFString' \"alpheccar\", compressed = False}) rect $ do
        myDocument
@

where myDocument is generating the pages and is a value of the PDF monad.
-}


{- $pages

You can add pages and specify a hierarchical structure for the pages. This hierarchy is optional. Here is an example
of how you could add some pages and specify the table of contents:

@
myDocument :: 'PDF' () 
myDocument = do
    page1 <- 'addPage' Nothing
    'newSection' ('toPDFString' \"Section\") Nothing Nothing $ do
     'newSection' ('toPDFString' \"Subsection\") Nothing Nothing $ do
        createPageContent page1
@

when you use 'addPage' you can specify a different size for the page or use the document's default one.
In 'newSection', the two Maybe options are used to style the entry in the PDF table of contents.

There are other functions to add pages with transitions.
-}

{- $content

To create content for a page, you have to use a page reference with 'drawWithPage'.

'drawWithPage' is using a 'Draw' monad value.

Element of the 'Draw' monad are built with geometry, text and color primitives.

@
createPageContent :: 'PDFReference' 'PDFPage' -> Draw () 
createPageContent page = 'drawWithPage' page $ do 
    'strokeColor' 'red'
    'setWidth' 0.5
    'stroke' $ 'Rectangle' 10 0 200 300
@

-}

{- $text

Text is complex. You can use the low level 'PDFText' to create a text in the 'Draw' monad. For instance:

@
textText :: 'PDFFont' -> 'PDFString' -> 'Draw' ()
textText f t = do
     'drawText' $ do
         'setFont' f
         'textStart' 10 200.0
         'leading' $ 'getHeight' f
         'renderMode' 'FillText'
         'displayText' t
         'startNewLine'
         'displayText' $ 'toPDFString' \"Another little test\"
@

It gives a detailed control on the position of characters and lines but it is too much work.

The library is thus supporting a higher level typesetting system with paragraph styles.

Displaying a formatted text is done with 'displayFormattedText' and using a typesetting monad value:

@
'displayFormattedText' ('Rectangle' (10 :+ 0) (110 :+ 300)) 'NormalPara' 'Normal' $ do 
   'paragraph' $ do
        'txt' $ \"Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor \"
        'txt' $ \"incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud \"
        'txt' $ \"exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute \"
        'txt' $ \"irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla \"
        'txt' $ \"pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia \"
        'txt' $ \"deserunt mollit anim id est laborum.\"
@

The text will be formatted using the NormalPara paragraph style and the Normal style for sentences.

NormalPara is part of an algebraic data type defining some vertical styles (from file test.hs):

@
data MyVertStyles = NormalPara
                  | CirclePara
                  | BluePara !PDFFloat
@

and Normal is part of another algebraic data typec (from file test.hs):

@
data MyParaStyles = Normal
                  | Bold
                  | Crazy
                  | SuperCrazy [Int] [PDFFloat]
                  | DebugStyle
                  | RedRectStyle
                  | BlueStyle
@

The library is coming with standard styles 'StandardParagraphStyle' and 'StandardStyle'.

Custom styles must be instances of some classes. A 'ComparableStyle' to allow the typesetting algorithm to decide when to group
different characters in a span of the same style.

A 'Style' class used for sentence style. And a 'ParagraphStyle' to group together the paragraph style and the sentence
style that can be used in this paragraph.

Why the 'ComparableStyle' is used instead of the class Eq ? A style is containing information 
used for the font (size etc ...) but it can also contain additional information used by styling function (a styling
function may draw a decoration). In that latter case, the additional information is changing the look of the sentence
but not its layout : the font size is not changed. So, from a text point of view, the PDF text is drawn using the same
attributes. But the additional decoration on top of it is changing.

So, 'ComparableStyle' is used to compare the font settings of a style.

The 'ParagraphStyle' is used to change the geometry of the paragraph (the paragraph can be typeset using
a circle as shape for instance). This style is also used to style the bounding box.

The other attributes like distance between two lines etc ... are controlled in the typesetting monad.

@
'setParaStyle' (BluePara 0)
'setFirstPassTolerance' 500
'unstyledGlue' 6 0.33 0
'paragraph' $ do
      'txt' $ \"Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. \"
@

Inside a paragraph, it is possible to change the line style and create new paragraphs:

@
'paragraph' $ do
    'txt' $ \"Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor\"
    'setStyle' Bold
    'txt' $ \" incididunt ut labore et dolore magna aliqua. \"
    'forceNewLine'
@

When charts are created, it is often useful to be able to display captions, labels etc ... The position
of the box containing the text is relative to some specific points in the drawing. To ease with this use-case, an
additional function is provided : 'drawTextBox'
-}

{- $monadstyle 

The typesetting is similar to the TeX one with kern, glues and boxes. So, it means that any drawing
can be used as a letter since any drawing can be contained in a box. The operators to draw boxes, glues are
part of the 'MonadStyle' monad. The 'Draw' value can be transformed into a box with 'mkDrawBox'.

The paragraph and the typesetting monad are instances of this class. So, boxes, glues, kerns can be used in horizontal
mode (paragraph) or vertical mode (typesetting monad).

-}

{- $geometry

Building shapes inside the draw monad is easy. For instance:

@
'strokeColor' red
'stroke' $ 'Rectangle' 0 (200 :+ 100)
'fillColor' 'blue'
'fill' $ 'Ellipse' 100 100 300 200
'fillAndStroke' $ 'RoundRectangle' 32 32 200 200 600 400
@

you can also create paths.

In addition to color, other attributes can be changed:

@
'withNewContext' $ do
    'setWidth' 2
    'setDash' $ 'DashPattern' [3] 0
    'stroke' $ 'Rectangle' 0 (200 :+ 100)
@

'withNewContext' is saving and restoring the settings.

Shapes can be filled with shading patterns:

@
'paintWithShading' ('RadialShading' 0 0 50 0 0 600 ('Rgb' 1 0 0) ('Rgb' 0 0 1)) ('addShape' $ 'Rectangle' 0 (300 :+ 300))
'paintWithShading' ('AxialShading' 300 300 600 400 ('Rgb' 1 0 0) ('Rgb' 0 0 1)) ('addShape' $ 'Ellipse' 300 300 600 400)
@

Note that in above example, 'addShape' is used. You can't use 'stroke' or 'fill'. You are just adding a shape to a path.

More complex patterns can also be used to fill the shapes. In below example we are filling shapes with a complex
drawing defined with a 'Draw' monad value.

@
patternTest :: 'PDFReference' 'PDFPage' -> 'PDF' ()
patternTest page = do
     p <- 'createUncoloredTiling' 0 0 100 50 100 50 'ConstantSpacing' pattern
     cp <- 'createColoredTiling' 0 0 100 50 100 50 'ConstantSpacing' cpattern
     'drawWithPage' page $ do
         'strokeColor' 'green'
         'setUncoloredFillPattern' p ('Rgb' 1 0 0)
         'fillAndStroke' $ 'Ellipse' 0 0 300 300
         'setColoredFillPattern' cp
         'fillAndStroke' $ 'Ellipse' 300 300 600 400
         
 where 
       pattern = do
           'stroke' ('Ellipse' 0 0 100 50)
       'cpattern' = do
           'strokeColor' ('Rgb' 0 0 1)
           'stroke' ('Ellipse' 0 0 100 50) 
@
-}

{- $xform

You can share an object between different pages of a document. It helps reducing the size of the
document is the shared drawing is big. An object can be a 'Draw' monad value. But it can be a JPEG picture too.

@
r <- 'createPDFXForm' 0 0 200 200 lineStyle
'drawWithPage' page6 $ do
     'drawXObject' r
@

in the above example, lineStyle is a @Draw()@ value.

-}

{- $image

It is possible to embed JPEG images in the document.

@
testImage ::  'JpegFile' -> 'PDFReference' 'PDFPage' -> 'PDF' ()
testImage jpgf page =  do
    jpg <- 'createPDFJpeg' jpgf
    'drawWithPage' page $ do
      'withNewContext' $ do
          'setFillAlpha' 0.4
          'drawXObject' jpg
      'withNewContext' $ do
           'applyMatrix' $ 'rotate' (Degree 20)
           'applyMatrix' $ 'translate' (200 :+ 200)
           'applyMatrix' $ 'scale' 2 2
           'drawXObject' jpg
@

The 'JpegFile' value must be created in the 'IO' monad with:

@
Right jpg <- 'readJpegFile' \"logo.jpg\"  
@

Alternatively, jpegs can be compiled into your code. After converting a jpeg to a data URL, a 'JpegFile' can be created with:

@
let Right jpg = readJpegDataURL "data:image/jpeg;base64,........."
@

The haskell code is just extracting the size of the image from the file. The image is not decoded.

-}

{- $annotations 

A pdf page can contain several kind of annotations like links, notes etc ... For instance, to define and
display a link:

@
'newAnnotation' ('URLLink' ('toPDFString' \"Go to my blog\") [0,0,200,100] \"http:\/\/www.alpheccar.org\" True)
@


-}

{- $warning

The PDF format is full of extensions. Depending on the viewer that you use some extensions may not be supported.
It is always a good thing to test on a few viewers if you use complex features.

Mobile viewers (tablets and phones) are generally focusing on a more portable and more restricted set of features. 
So, you may not be able to display you document on a mobile device if you use complex features.

So, I repeat : test.
-}