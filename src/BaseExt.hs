module BaseExt
    (
        (|>)
        , (?)
        , withDefault
    )
where

import Data.Maybe

(|>) :: a -> (a -> b) -> b
a |> f = f a

withDefault :: a -> Maybe a -> a
withDefault = fromMaybe

(?) :: Bool -> (a, a) -> a 
p ? (t, e) = if p then t else e