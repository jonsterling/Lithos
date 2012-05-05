{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Text.Lithos.Write where
import Text.Lithos.Data
import Text.Hamlet
import Text.Cassius
import Text.Blaze.Renderer.String (renderHtml)
import qualified Text.Pandoc as P
import qualified Text.Highlighting.Kate as K
import qualified Text.Highlighting.Kate.Format.HTML as K

class WriteHtml a where
  writeHtml :: a -> Html
  writeHtmlString :: a -> String
  writeHtmlString = renderHtml . writeHtml

instance WriteHtml Prose where
  writeHtml (Prose ls) = write . read . unlines $ ls
    where write = P.writeHtml P.defaultWriterOptions
          read = P.readMarkdown P.defaultParserState

instance WriteHtml Code where
  writeHtml (Code ls) = format . highlight . unlines $ ls
    where highlight = K.highlightAs "haskell"
          format = K.formatHtmlBlock K.defaultFormatOpts

instance WriteHtml Section where
  writeHtml (Section p c) = 
    [shamlet| 
      <tr>
        <td>^{writeHtml p}
        <td>^{writeHtml c}
    |]

instance WriteHtml Document where
  writeHtml (Document ss) = do
    [shamlet|
      <style>
        #{renderCss $ documentStyle render}
      <table>
        <tbody>
          $forall s <- ss
            ^{writeHtml s}
    |]


-- This dumb bullshit for Cassius
data MyRoute = MyRoute
render _ _ = ""

documentStyle =
  [cassius|
    body
      font-family: 'Palatino Linotype'
      font-size: 15px
      line-height: 22px
      color: #252519
      margin: 0
      padding: 0

    p
      margin: 0 0 15px 0
    
    table td
      border: 0
      outline: 0

    td, th
      max-width: 450px
      min-width: 450px
      min-height: 5px
      padding: 10px 25px 1 px 50px
      overflow-x: hidden
      vertical-align: top
      text-align: left
      
  |]
