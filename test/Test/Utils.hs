module Test.Utils (
    assert,
    maybeToEither,
) where

import Control.Applicative (Alternative (..))

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e m = case m of
    Nothing -> Left e
    Just x -> Right x

assert :: (Alternative f) => Bool -> a -> f a
assert b d = case b of
    False -> empty
    True -> pure d
