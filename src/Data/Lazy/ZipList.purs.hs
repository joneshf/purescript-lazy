module Data.Lazy.ZipList where

  import Data.Lazy
  import Data.Lazy.List

  data ZipList a = ZipList (List a)

  instance eqZipList :: (Eq a) => Eq (ZipList a) where
    (==) (ZipList xs) (ZipList ys) = xs == ys

    (/=) xs ys = not (xs == ys)

  instance showZipList :: (Show a) => Show (ZipList a) where
    show (ZipList zl) = "ZipList(" ++ go zl ++ ")"
      where
        go Nil = ""
        go (Cons x xs) = show x ++ case force xs of
          Nil -> ""
          xs' -> "," ++ go xs'

  instance applicativeZipList :: Applicative ZipList where
    pure x = ZipList (repeat x)

    (<*>) (ZipList fs) (ZipList xs) = ZipList $ go fs xs
      where
        go (Cons f fs) (Cons x xs) = Cons (f x) (defer \_ -> go (force fs) (force xs))
        go _           _           = Nil
