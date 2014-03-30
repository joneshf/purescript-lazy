module Data.Lazy.ZipList where

  import Data.Array
  import Data.Lazy
  import Data.Lazy.List
  import Data.Monoid

  -- | A version of List with a different Applicative instance.
  --   Generalizes the idea of map.
  data ZipList a = ZipList (List a)

  instance eqZipList :: (Eq a) => Eq (ZipList a) where
    (==) (ZipList xs) (ZipList ys) = xs == ys

    (/=) xs ys = not (xs == ys)

  instance showZipList :: (Show a) => Show (ZipList a) where
    show (ZipList zl) = "ZipList(" ++ joinWith (map show (toArray zl)) "," ++ ")"

  instance monoidZipList :: Monoid (ZipList a) where
    mempty = ZipList mempty

    (<>) (ZipList l1) (ZipList l2) = ZipList (l1 <> l2)

  instance applicativeZipList :: Applicative ZipList where
    pure x = ZipList (repeat x)

    (<*>) (ZipList fs) (ZipList xs) = ZipList $ go fs xs
      where
        go (Cons f fs) (Cons x xs) = Cons (f x) (defer \_ -> go (force fs) (force xs))
        go _           _           = Nil
