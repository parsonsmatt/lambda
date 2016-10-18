{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Lambda.Untyped.Types where

import Prelude hiding (abs)
import Test.QuickCheck
import Control.Monad.Free
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

-- | This representation of the lambda calculus has the base cases of the
-- recursion taken out. This lays bare the structure of the branching.
data LambdaF r
    = App r r
    | Abs Text r
    deriving (Eq, Show)

var :: a -> Free f (Literal a)
var = Pure . Var

str :: Text -> Free f (Literal a)
str = Pure . Str

int :: Integer -> Free f (Literal a)
int = Pure . Int

app :: Free LambdaF a -> Free LambdaF a -> Free LambdaF a
app l r = Free (App l r)

abs :: Text -> Free LambdaF a -> Free LambdaF a
abs x r = Free (Abs x r)

-- | The type of literals is how we'll terminate the recursion. We leave the
-- type of variables polymorphic.
data Literal a
    = Var a
    | Str Text
    | Int Integer
    deriving (Eq, Show)

instance Arbitrary Text where
    arbitrary = T.pack . take 6 <$> listOf1 (choose ('a', 'z'))

instance Arbitrary a => Arbitrary (Literal a) where
    arbitrary = oneof [Var <$> arbitrary, Str <$> arbitrary, Int <$> arbitrary ]


-- | The type of our parsing, then, is taking the free monad of the lambda
-- calculus functor with the leaves of the tree denoted by the literal type.
type Parsed a = Free LambdaF (Literal a)

type Eval a = Free LambdaF (Either Int (Literal a))

instance Arbitrary a => Arbitrary (Free LambdaF a) where
    arbitrary = sized go
      where
        go i
            | i <= 0    = Pure <$> arbitrary
            | otherwise =
                oneof [ abs <$> randChars <*> go (i - 1)
                      , app <$> go (i - 1) <*> go (i - 1)
                      , Pure <$> arbitrary
                      ]
        randChars = T.pack . take 6 <$> listOf1 (choose ('a', 'z'))

    shrink (Free (App a b)) = [a, b] <> (uncurry app <$> shrink (a, b))
    shrink (Free (Abs _ l)) = l : shrink l
    shrink v@(Pure _) = [v]

instance Functor LambdaF where
    fmap k (App f x) = App (k f) (k x)
    fmap k (Abs n e) = Abs n (k e)
