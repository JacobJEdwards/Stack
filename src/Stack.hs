{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DeriveGeneric #-}
module Stack (
  Stack,
  empty,
  push,
  pop,
  top,
  size,
  fromList,
  toList,
  Stack.reverse,
  isEmpty,
  Stack.map,
  Stack.filter,
  Stack.foldl,
  Stack.foldr,
  Stack.all,
  Stack.any,
  Stack.concat
) where

import Data.List(reverse)
import Data.Data (type Typeable)
import GHC.Generics (type Generic)
import Control.DeepSeq (type NFData)

-- | The 'Stack' type represents a stack of elements of type 'a'.
newtype Stack a = Stack [a]
  deriving stock (Show, Eq, Typeable, Generic, Ord, Read)

deriving newtype instance Foldable Stack
deriving newtype instance Functor Stack
deriving newtype instance Semigroup (Stack a)
deriving newtype instance Monoid (Stack a)
deriving newtype instance Applicative Stack
deriving newtype instance Monad Stack
deriving newtype instance NFData a => NFData (Stack a)

-- | The 'empty' function creates an empty stack.
empty :: Stack a
empty = Stack []

-- | The 'isEmpty' function returns 'True' if the stack is empty.
isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty _ = False

-- | The 'push' function pushes an element onto the stack.
push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

-- | The 'pop' function pops the top element off the stack.
pop :: Stack a -> Maybe (a, Stack a)
pop (Stack []) = Nothing
pop (Stack (x:xs)) = Just (x, Stack xs)

-- | The 'top' function returns the top element of the stack.
top :: Stack a -> Maybe a
top (Stack []) = Nothing
top (Stack (x:_)) = Just x

-- | The 'size' function returns the number of elements in the stack.
size :: Stack a -> Int
size (Stack xs) = length xs

-- | The 'fromList' function creates a stack from a list of elements.
fromList :: [a] -> Stack a
fromList = Stack

-- | The 'toList' function converts a stack to a list of elements.
toList :: Stack a -> [a]
toList (Stack xs) = xs

-- | The 'reverse' function reverses the elements of the stack.
reverse :: Stack a -> Stack a
reverse (Stack xs) = Stack (Data.List.reverse xs)

-- | The 'map' function applies a function to each element of the stack.
map :: (a -> b) -> Stack a -> Stack b
map f (Stack xs) = Stack (Prelude.map f xs)

-- | The 'filter' function filters the elements of the stack using a predicate.
filter :: (a -> Bool) -> Stack a -> Stack a
filter f (Stack xs) = Stack (Prelude.filter f xs)

-- | The 'foldl' function folds the elements of the stack from left to right.
foldl :: (b -> a -> b) -> b -> Stack a -> b
foldl f z (Stack xs) = Prelude.foldl f z xs

-- | The 'foldr' function folds the elements of the stack from right to left.
foldr :: (a -> b -> b) -> b -> Stack a -> b
foldr f z (Stack xs) = Prelude.foldr f z xs

-- | The 'all' function returns 'True' if all elements in the stack satisfy the predicate.
all :: (a -> Bool) -> Stack a -> Bool
all f (Stack xs) = Prelude.all f xs

-- | The 'any' function returns 'True' if at least one element in the stack satisfies the predicate.
any :: (a -> Bool) -> Stack a -> Bool
any f (Stack xs) = Prelude.any f xs

-- | The 'concat' function concatenates a stack of stacks into a single stack.
concat :: Stack (Stack a) -> Stack a
concat (Stack xs) = Stack $ concatMap toList xs

