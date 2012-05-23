
    maximum' :: (Ord a) => [a] -> a
    maximum' [] = error "maximum of empty list"
    maximum' [x] = x
    maximum' (x:xs) = max x (maximum' xs)

没有`for`和`while`, 思维上用递归`recursion`理解...  

    replicate' :: (Num i, Ord i) => i -> a -> [a]
    replicate' n x
      | n <= 0    = []
      | otherwise = x:replicate' (n-1) x

例子比如, 递归生成`n`个`x`的列表:  

    take' :: (Num i, Ord i) => i -> [a] -> [a]
    take' n _
         | n <= 0   = []
    take' _ []     = []
    take' n (x:xs) = x : take' (n-1) xs

用了`Num`和`Ord`两者的原因作者写到`Num`不是`Ord`的子集  
`take`作为例子:  

    reverse' :: [a] -> [a]
    reverse' [] = []
    reverse' (x:xs) = reverse' xs ++ [x]

`reverse`作为例子:  

    repeat' :: a -> [a]
    repeat' x = x:repeat' x

`repeat`作为例子:  

    zip' :: [a] -> [b] -> [(a,b)]
    zip' _ [] = []
    zip' [] _ = []
    zip' (x:xs) (y:ys) = (x,y):zip' xs ys

`zip`作为例子:  

    elem' :: (Eq a) => a -> [a] -> Bool
    elem' a [] = False
    elem' a (x:xs)
      | a == x    = True
      | otherwise = a `elem'` xs

`elem`作为例子:  
`quicksort`被很多人用来展示`Haskell`的优雅,  

    quicksort :: (Ord a) => [a] -> [a]
    quicksort [] = []
    quicksort (x:xs) =
      let
        smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
      in  smallerSorted ++ [x] ++ biggerSorted

运行这段代码发现`ghc`有必要将`smallerSorted`进行对应缩进, 而不是连写在`let`后面  

    iterate' :: (a -> a) -> a -> a
    iterate' f x = x: iterate' f (f x)

递归的方式往往是设定规则和边缘, 对于列表往往上头尾部和空列表  
再来看无穷数列, 注意定义函数时将函数本身用于递归,  
`iterate`作为例子  

    isSquare n = elem n (takeWhile (<=n) squares) where squares=[x^2| x <- [0..]]
    fibs= fibgen 1 1 where fibgen n1 n2 = n1 : fibgen n2 (n1+n2)
    prime= sieve [2..] where sieve (x:xs) = x:sieve (filter (\y->rem y x /=0) xs)

几个重要的例子, 体会下怎样处理递归的:  