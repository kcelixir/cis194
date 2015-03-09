import Data.Monoid
import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m a b) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty a = a
(+++) a Empty = a
(+++) a b = Append (tag a <> tag b) a b

sizeOf :: (Sized b, Monoid b) => JoinList b a -> Int
sizeOf = getSize . size . tag

-- to test, (indexJ i jl) == (jlToList jl !!? i)
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n jl | n < 0 || n > sizeOf jl = Nothing
indexJ 0 (Single _ a) = Just a
indexJ n (Append m a b)
    | n < sizeOf a = indexJ n a
    | otherwise = indexJ (n - sizeOf a) b

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- to test, jlToList (dropJ n jl) == drop n (jlToList jl)
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl
    | n <= 0 = jl
    | n >= sizeOf jl = Empty
dropJ n (Append m a b)
    | n < sizeOf a = dropJ n a +++ b
    | otherwise = dropJ (n - sizeOf a) b

-- to test, jlToList (takeJ n jl) == take n (jlToList jl)
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n jl
    | n <= 0 = Empty
    | n >= sizeOf jl = jl
takeJ n (Append m a b)
    | n > sizeOf a = (Append m a (takeJ (n - sizeOf a) b))
    | otherwise = takeJ n a

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s
