module MNML.Base
    ( ModName
    , QualifiedReference
    , ValName
    ) where

import           Data.Text (Text)

type ModName = Text

type ValName = Text

type QualifiedReference = (Text, Text)
