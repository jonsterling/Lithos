Text.Lithos.Data
================

> {-# LANGUAGE GeneralizedNewtypeDeriving #-}

> module Text.Lithos.Data where
> import Data.Monoid

Let's define our model. Each `Section` is a block of prose and
a block of code.

> data Section = Section Prose Code deriving Show
> newtype Prose = Prose [String] deriving (Show,Monoid)
> newtype Code = Code [String] deriving (Show,Monoid)
> newtype Document = Document [Section] deriving Show