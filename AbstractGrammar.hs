module AbstractGrammar where

type Strings = String
type Paragraph = String
type Header = String
type Bold = String
type Italic = String
type Link = String
type Img = String


data Slides = Slides [Slide]
    deriving Show

data Slide = Slide TitleSlide BodySlide
    deriving Show

data TitleSlide = TitleSlide Strings
    deriving Show

data Cad = Cad Strings
    deriving Show

data BodySlide = BodySlide [MarkdownBlock]
    deriving Show

data MarkdownBlock = MdParagraph Paragraph
                    | MdH1 Header
                    | MdH2 Header
                    | MdH3 Header
                    | MdH4 Header
                    | MdH5 Header
                    | MdH6 Header
                    | MdBold Bold
                    | MdItalic Italic
                    | MdLink Link
                    | LocalImg Img
                    | URLImg Img
    deriving Show
