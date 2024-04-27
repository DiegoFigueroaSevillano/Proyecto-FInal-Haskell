module HTMLGenerator where

import AbstractGrammar

generateHTML :: Slides -> String
generateHTML (Slides slides) =
  "<!DOCTYPE html>\n\
  \<html>\n\
  \  <head>\n\
  \    <link rel=\"stylesheet\" href=\"styles.css\">\n\
  \  </head>\n\
  \  <body>\n\
  \    <div class=\"slides\">\n"
  ++ concatMap generateSlide slides ++
  "    </div>\n\
  \  </body>\n\
  \</html>"

generateSlide :: Slide -> String
generateSlide (Slide titleSlide bodySlide) =
  "      <div class=\"slide\">\n"
  ++ generateTitleSlide titleSlide
  ++ generateBodySlide bodySlide
  ++ "      </div>\n"

generateTitleSlide :: TitleSlide -> String
generateTitleSlide (TitleSlide title) =
  "        <div class=\"title\">\n"
  ++ "          <h1>" ++ title ++ "</h1>\n"
  ++ "        </div>\n"

generateBodySlide :: BodySlide -> String
generateBodySlide (BodySlide markdownBlocks) =
  "        <div class=\"content\">\n"
  ++ concatMap generateMarkdownBlock markdownBlocks
  ++ "        </div>\n"

generateMarkdownBlock :: MarkdownBlock -> String
generateMarkdownBlock (MdParagraph text) = "<p>" ++ text ++ "</p>\n"
generateMarkdownBlock (MdH1 header) = "<h1>" ++ header ++ "</h1>\n"
generateMarkdownBlock (MdH2 header) = "<h2>" ++ header ++ "</h2>\n"
generateMarkdownBlock (MdH3 header) = "<h3>" ++ header ++ "</h3>\n"
generateMarkdownBlock (MdH4 header) = "<h4>" ++ header ++ "</h4>\n"
generateMarkdownBlock (MdH5 header) = "<h5>" ++ header ++ "</h5>\n"
generateMarkdownBlock (MdH6 header) = "<h6>" ++ header ++ "</h6>\n"
generateMarkdownBlock (MdBold bold) = "<strong>" ++ bold ++ "</strong>\n"
generateMarkdownBlock (MdItalic italic) = "<em>" ++ italic ++ "</em>\n"
generateMarkdownBlock (MdLink link) = "<a href=\"" ++ link ++ "\">" ++ link ++ "</a>\n"
generateMarkdownBlock (LocalImg imagePath) = "<img src=\"" ++ imagePath ++ "\" class=\"centered-image\" />\n"
generateMarkdownBlock (URLImg imageURL) = "<img src=\"" ++ imageURL ++ "\" class=\"centered-image\" />\n"

finishHTML :: String
finishHTML =
  "    </div>\n\
  \  </body>\n\
  \</html>"
