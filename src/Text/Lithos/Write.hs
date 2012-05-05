{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Text.Lithos.Write
  ( WriteHtml(..) ) where

import Text.Lithos.Data
import Text.Hamlet
import Text.Cassius
import Text.Julius
import Text.Coffee
import Text.Blaze (ToHtml(..),preEscapedLazyText)
import Text.Blaze.Renderer.String (renderHtml)
import qualified Text.Pandoc as P
import qualified Text.Highlighting.Kate as K
import qualified Text.Highlighting.Kate.Format.HTML as K

class WriteHtml a where
  writeHtml :: a -> Html
  writeHtmlString :: a -> String
  writeHtmlString = renderHtml . writeHtml

writerOptions :: P.WriterOptions
writerOptions =
  P.defaultWriterOptions 
    { P.writerHTMLMathMethod  = P.MathJax "" }

readerOptions :: P.ParserState
readerOptions =
  P.defaultParserState
    { P.stateLiterateHaskell = True }

instance WriteHtml Prose where
  writeHtml (Prose ls) = write . read . unlines $ ls
    where write = P.writeHtml writerOptions
          read = P.readMarkdown readerOptions



instance WriteHtml Code where
  writeHtml (Code ls) = format . highlight . unlines $ ls
    where highlight = K.highlightAs "haskell"
          format = K.formatHtmlBlock K.defaultFormatOpts

instance WriteHtml Section where
  writeHtml (Section p c) = 
    [shamlet| 
      <tr>
        <td .prose>^{writeHtml p}
        <td .code>^{writeHtml c}
    |]

instance WriteHtml Document where
  writeHtml (Document ss) = do
    [shamlet|
      <script src=https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js>
      <script src=http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML>
      <script>
        #{documentScript render}
      <style>
        #{documentStyle render}
      <table>
        <tbody>
          $forall s <- ss
            ^{writeHtml s}
    |]

instance ToHtml Javascript where
  toHtml js = preEscapedLazyText (renderJavascript js)

instance ToHtml Css where
  toHtml css = preEscapedLazyText (renderCss css)

documentStyle =
  [cassius|
    body
      color: #252519
      margin: 0
      padding: 0

    p
      margin: 0 0 15px 0
    
    table
      border-collapse: collapse
    
    td
      border: 0
      outline: 0
      vertical-align: top

    td.prose
      max-width: 450px
      min-width: 450px
      min-height: 5px
      padding: 10px 25px 1 px 50px
      overflow-x: hidden
      text-align: left
      font-family: Palatino
      font-size: 11pt
      line-height: 1.4em


    td.code
      padding: 14px 15px 16px 25px
      width: 100%
      background: #fdf6e3
      border-left: 1px solid #eee8d5

    pre, tt, code
      font-size: 11pt
      line-height: 1.5em
      font-family: Letter Gothic Std
      margin: 0
      padding: 0
      color: #657b83
    
    .kw
      color: #859900
    
    .dt
      color: #cb4b16
    .dv 
      color: #0000FF
    .bn 
      color: #0000FF
    .fl 
      color: #800080
    .ch
      color: #93a1a1
    .st
      color: #93a1a1
    .co
      color: #93a1a1
      font-style: italic
    .ot
      
    .al
      color: green
      font-weight: bold
    .fu
      color: #268bd2
    .re
    
    .er
      color: red
      font-weight: bold
  |]

documentScript = 
  [coffee|
    $ -> 
      $('code span').each ->
        sub = (a,b) => $(this).html $(this).html().replace a,b

        sub '-&gt;',     '&rarr;'
        sub '&lt;-',     '&larr;'
        sub '=&gt;',     '&rArr;'
        sub '==',        '&equiv;'
        sub 'forall',    '&forall;'
        sub '()',        '&empty;'
        sub '&lt;*&gt;', '&#8859;'
        sub '::',        '&#8759;'
  |]

-- This dumb bullshit for Cassius
data MyRoute = MyRoute
render _ _ = ""
