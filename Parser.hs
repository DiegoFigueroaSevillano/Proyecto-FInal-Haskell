module Parser where
 
import UU.Parsing
import Scanner
import AbstractGrammar
 
pSlides = Slides <$> pList pSlide
 
pSlide = Slide <$> pTitleSlide <*> pBodySlide <* pEndSlide "---"
 
pTitleSlide = TitleSlide <$ pKeyword "!" <*> pStrings
 
pBodySlide = BodySlide <$ pOpenBlock "{" <*> pList pMarckdownBlock <* pEndBlock "}"  
 
pMarckdownBlock = MdParagraph <$> pStrings
            <|> pHeaders
            <|> pBold
            <|> pItalic
            <|> pLink
            <|> pLocalImg
            <|> pUrlImg

pHeaders :: Parser Token MarkdownBlock
pHeaders =  MdH5 <$ pKeyword "#####" <*> pStrings
               <|>  MdH4 <$ pKeyword "####" <*> pStrings
               <|>  MdH3 <$ pKeyword "###" <*> pStrings
               <|>  MdH2 <$ pKeyword "##" <*> pStrings
               <|>  MdH1 <$ pKeyword "#" <*> pStrings

pLocalImg :: Parser Token MarkdownBlock
pLocalImg = LocalImg <$ pKeyword "&" <*> pStrings

pUrlImg :: Parser Token MarkdownBlock
pUrlImg = URLImg <$ pKeyword "$" <*> pStrings

pBold :: Parser Token MarkdownBlock
pBold = MdBold <$ pKeyword "^" <*> pStrings <* pKeyword "^"

pItalic :: Parser Token MarkdownBlock
pItalic = MdItalic <$ pKeyword "*" <*> pStrings <* pKeyword "*"

pLink :: Parser Token MarkdownBlock
pLink = MdLink <$ pKeyword "<" <*> pStrings <* pKeyword ">"

instance Symbol Token
 
getValue:: Token -> String
getValue (Token _ v _ _) = v
 
tSym :: Type -> String -> Parser Token String
tSym typ value = getValue <$> pSym (Token typ value 0 0)
 
tStr = getValue <$> pSym (Token String "" 0 0)
 
pKeyword :: String -> Parser Token String
pKeyword = tSym Keyword
 
pOpenBlock :: String -> Parser Token String
pOpenBlock = tSym OpenBlock
 
pEndBlock :: String -> Parser Token String
pEndBlock = tSym EndBlock
 
pEndSlide :: String -> Parser Token String
pEndSlide = tSym EndSlide
 
pStrings :: Parser Token String
pStrings = tStr