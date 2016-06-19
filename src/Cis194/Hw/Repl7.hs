:set -i../../
:set prompt "> "
:l JoinList.hs

indexJ (-1) $ Single (Size 1) 1 +++ Single (Size 1) 2 +++ Single (Size 1) 3
indexJ 0 $ Single (Size 1) 1 +++ Single (Size 1) 2 +++ Single (Size 1) 3
indexJ 1 $ Single (Size 1) 1 +++ Single (Size 1) 2 +++ Single (Size 1) 3
indexJ 2 $ Single (Size 1) 1 +++ Single (Size 1) 2 +++ Single (Size 1) 3
indexJ 3 $ Single (Size 1) 1 +++ Single (Size 1) 2 +++ Single (Size 1) 3
indexJ 4 $ Single (Size 1) 1 +++ Single (Size 1) 2 +++ Single (Size 1) 3

dropJ (-1) $ Single (Size 1) 1 +++ Single (Size 1) 2 +++ Single (Size 1) 3
dropJ 0 $ Single (Size 1) 1 +++ Single (Size 1) 2 +++ Single (Size 1) 3
dropJ 1 $ Single (Size 1) 1 +++ Single (Size 1) 2 +++ Single (Size 1) 3
dropJ 2 $ Single (Size 1) 1 +++ Single (Size 1) 2 +++ Single (Size 1) 3
dropJ 3 $ Single (Size 1) 1 +++ Single (Size 1) 2 +++ Single (Size 1) 3
dropJ 4 $ Single (Size 1) 1 +++ Single (Size 1) 2 +++ Single (Size 1) 3

takeJ (-1) $ Single (Size 1) 1 +++ Single (Size 1) 2 +++ Single (Size 1) 3
takeJ 0 $ Single (Size 1) 1 +++ Single (Size 1) 2 +++ Single (Size 1) 3
takeJ 1 $ Single (Size 1) 1 +++ Single (Size 1) 2 +++ Single (Size 1) 3
takeJ 2 $ Single (Size 1) 1 +++ Single (Size 1) 2 +++ Single (Size 1) 3
takeJ 3 $ Single (Size 1) 1 +++ Single (Size 1) 2 +++ Single (Size 1) 3
takeJ 4 $ Single (Size 1) 1 +++ Single (Size 1) 2 +++ Single (Size 1) 3

:l Scrabble.hs

scoreString "yay"
scoreLine "yay " +++ scoreLine "haskell!"

:l JoinListBufEditor.hs
