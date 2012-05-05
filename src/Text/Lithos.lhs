Need comment here

> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}

> module Text.Lithos where

> import Text.ParserCombinators.Parsec hiding ((<|>), many, optional)
> import Text.Parsec.Prim (manyAccum)
> import Control.Applicative
> import Data.Monoid
> import Data.Maybe
> import Control.Monad

First, let's define our model. Each `Section` is a block of prose and
a block of code; these will be displayed in parallel.

> data Section = Section Prose Code deriving Show
> newtype Prose = Prose [String] deriving (Show,Monoid)
> newtype Code = Code [String] deriving (Show,Monoid)

Before we begin parsing, a few primitives must be defined:

> tilEOL = manyTill (noneOf "\n") eol
>   where eol = newline <|> ('\n' <$ eof)

Code appears following "bird tracks":

> code = Code <$> trimMany1 codeLine
>   where codeLine = birdTrack *> tilEOL
>         birdTrack = string "> "

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

> optionZero = option mempty

Finally, a document is composed of many sections.

> literateDocument = many section
