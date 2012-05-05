Text.Lithos.Parse
=================

Lithos provides a parser for Literate Haskell files; currently, only the bird-tracks style of literate programming is supported.

> {-# LANGUAGE NoMonomorphismRestriction #-}
> 
> module Text.Lithos.Parse where
> import Text.Lithos.Data
> 
> import Text.ParserCombinators.Parsec hiding ((<|>), many, optional)
> import Control.Applicative
> import Control.Monad (when)
> import Data.Monoid

> tilEOL = manyTill (noneOf "\n") eol
>   where eol = newline <|> ('\n' <$ eof)

*Code* appears following "bird tracks", like the following:

< > converge :: Eq a => (a -> a) -> a -> a
< > converge f x =
< >   let y = f x in
< >     if x == y then x else converge f y

> code = Code <$> trimMany1 codeLine
>   where codeLine = birdTrack *> tilEOL
>         birdTrack = string "> "

Any other text (including blank lines) is considered *prose*.

> prose = Prose <$> trimMany1 (try textLine <|> blankLine)
>   where textLine = (:) <$> lineHead <*> tilEOL
>         lineHead = do ch <- noneOf "\n"
>                       when (ch == '>') $ notFollowedBy space
>                       return ch
>         blankLine = mempty <$ char '\n'

Now we need to define some new parser combinators. `trimMany1` behaves
just like `many1`, except that it returns a list trimmed for monoidal
zeroes at either end.

> trimMany1 p = trimZero <$> many1 p

We use an auxiliary function `trimZero` to clean up the ends of lists
of monoidal elements:

> trimZero :: (Eq m, Monoid m) => [m] -> [m]
> trimZero = dropEmpty . reverse . dropEmpty . reverse
>   where dropEmpty = dropWhile (== mempty)

There are three options for a section: it may have both prose and
code, it may have only code, and it may have only prose. The latter
two possibilities will likely occur at the edges of documents, whereas
only all the interior sections should have both prose and code.

> section = try (Section <$> prose <*> code) 
>       <|> try (Section <$> optionZero prose <*> code)
>       <|> Section <$> prose <*> optionZero code

We define an auxiliary combinator `optionZero` which either returns
the result of the input parser if satisfied, or the appropriate zero
value.

> optionZero :: Monoid m => Parser m -> Parser m
> optionZero = option mempty

Finally, a document is composed of many sections.

> literateDocument = Document <$> many section
