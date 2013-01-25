
module Util.Annotated where

class Annotation a where
    nullAnnotation :: a
    describe       :: a -> String
    describe _ = "no description"

class Annotated t where
    annotation :: t a -> a
    annotations :: t a -> [a]

relatedNullAnnotation :: (Annotation a, Annotated t) => t a -> a
relatedNullAnnotation _ = nullAnnotation
