------------------------------------------------------------------------------
Haskell Winter 2015
Homework 1
------------------------------------------------------------------------------

Fill in the requested code in this file, test your mapping functions, and
cnc_submit hw1.lhs for your homework.



> import Prelude hiding (Maybe(..), Either(..))

1.  Use the following Error data type for this problem.

> data Maybe a = Nothing | Just a

  (a) Give the type and Haskell code for an appropriate map function over
      the (Maybe a) type.

> mapMaybe :: (a -> b) -> Maybe a -> Maybe b

> mapMaybe _ (Nothing) = Nothing
> mapMaybe f (Just a) = Just (f a)

  (b) Declare the Maybe type constructor as an instance of the Functor class.

> instance Functor Maybe where
> fmap f (Just x) = Just (f x)
> fmap f Nothing = Nothing

2.  Use the following Error data type for this problem.

> data Error a = Ok a | Error String


  (a) Give the type and Haskell code for an appropriate map function over
      the (Error a) type.

> mapError :: (a -> b) -> Error a -> Error b

> mapError _ (Error a) = Error a
> mapError f (Ok a) = Ok (f a)

  (b) Declare the Error type constructor as an instance of the Functor class.

> instance Functor Error where
> fmap f (Ok a) = Ok (f a)
> fmap f (Error a) = Error a

3. Consider the following binary tree data type.

> data Tree a = Nil | Node a (Tree a) (Tree a)
> 	    deriving (Eq, Read,Show)

  (a) Give the type and function definition for mapT, the map function over the
      binary tree data type above

> mapT :: (a -> b) -> Tree a -> Tree b

> mapT _ Nil = Nil
> mapT f (Node a t1 t2) = Node (f a) (mapT f t1) (mapT f t2)

  (b) Declare the Tree type constructor as an instance of the Functor class.

> instance Functor Tree where
> fmap f (Node a t1 t2) = Node (f a) (fmap f t1) (fmap f t2)
> fmap f Nil = Nil

4. Consider the following General Tree data type.

> data GTree a = GNil | GNode a [GTree a]


  (a) Give a type and a code definition for a mapGT function over GTree. Give
      the most general type.


> mapGT :: (a -> b) -> Gtree a -> Gtree b

> mapGT f GNil = GNil
> mapGT f (GNode a ts) = Gnode (f a) $ map (mapGT f) ts

  (b) Declare the Gtree type constructor as an instance of the Functor class.

> instance Functor Gtree where
> fmap = mapGT

5. Consider the (Either a b) data type.

> data Either a b = Left a | Right b


  (a) Give a type and a code definition for a mapEither function over the
      Either type. This is a bit tricky because the Either type constructor
      takes two parameters and the mapEither will only work on one of them.
      Recall that (Either a b) means ((Either a) b).

> mapEither :: (b -> c) -> Either a b -> Either a c

> mapEither f (Right b) = Right (f b)
> mapEither f (Left a) = Left a


  (b) Declare the (Either a) type constructor as an instance of the Functor
      class.

> instance Functor (Either a) where
> fmap = mapEither


-----------------------------------------------------------------------------
-- Testing
-----------------------------------------------------------------------------
