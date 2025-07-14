module MNML.Base
    ( ConstructorName
    , ModName
    , QualifiedConstructorReference
    , QualifiedTypeReference
    , QualifiedValueReference
    , TypeName
    , ValName
    ) where

import           Data.Text (Text)

-- e.g. "foo/bar/baz" represented as ["foo", "bar", "baz"]
type ModName = [Text]
-- e.g. Int
type TypeName = Text
-- e.g. Just, Err
type ConstructorName = Text
-- e.g. "asdf"
type ValName = Text

-- e.g. "foo/bar/baz::Qux" represented as (["foo", "bar", "baz"], "Qux")
-- These are semantically equivalent to QualifiedValueReferences (a constructor
-- is a value), but the syntax is slightly different.
type QualifiedConstructorReference = (ModName, ConstructorName)

-- e.g. "foo/bar/baz::qux" represented as (["foo", "bar", "baz"], "qux")
type QualifiedValueReference = (ModName, ValName)

-- e.g. "math::Complex" represented as (["math"], "Complex")
type QualifiedTypeReference = (ModName, TypeName)
