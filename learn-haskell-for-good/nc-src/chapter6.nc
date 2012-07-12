  --
高阶函数`higher order function`是`Haskell`体验中不可少的一部分,
多参数函数相当于函数接受参数返回另一函数来接收下一个参数, 称为`curried functions`
`Haskell B. Curry`的名字被使用了, `curried`函数简单说是参数分步代入
  max 4 5 -- 5
和`(max 4) 5`是一致的, 因为`(max 4)`结束返回了一个函数`(->)`
  max :: (Ord a) => a -> a -> a
可理解为每接受一个参数`a`返回一个函数`(->)`, 再接受参数,
  max :: (Ord a) => a -> (a -> a)
这个写法意思一样, 可我不明白那里区别了.
还提到可以将只部分执行的函数作为参数传递的用法, 没有详解
  multThree :: (Num a) => a -> (a -> (a -> a))
像把后面括号部分作为前面已执行部分的参数(?)
  let m x y z = x*y*z
  :t m -- m :: Num a => a -> a -> a -> a
  :t (m 9) -- (m 9) :: Num a => a -> a -> a
  let m'9 = m 9
具体的例子.. 表明返回了函数, 注意参数个数
将一个部分执行的函数结果传递给 m'9
  :t m'9 -- m'9 :: Integer -> Integer -> Integer
类型改变倒是在意料之外, 对浮点数报错了.
  (/10) 200 -- 20.0
因此还有`let divide'10 = (/10)`的写法, 将其写作函数
  divideByTen :: (Floating a) => a -> a
  divideByTen = (/10)
具体写法, 这个类型, 函数接受一个参数返回浮点数, 根据实际来了.
看来, 几乎所有, 中置表达式都可以括号加简写,,
  isUpperAlphanum :: Char -> Bool
  isUpperAlphanum = (`elem` ['A'..'Z'])
  (subtract 4) 3 -- -1
注意参数顺序, `(4) (3) (-1)`
作者说减号`-`例外, 因为负数, 因而只能用`subtrsct`
  -- No instance for (Show (a0 -> a0)) ..
部分执行的函数如`(subtract 4)`直接在 ghci 执行会报错, 
因为返回的`(->)`不是类型`Show`的某个实例, 看来`ghci`的输出都这么搞的

  applyTwice :: (a -> a) -> a -> a
  applyTwice f x = f (f x)
  applyTwice (+3) 10 -- 16
  applyTwice ("HAHA " ++) "HEY" -- "HAHA HAHA HEY"
  applyTwice (++ " HAHA") "HEY" -- "HEY HAHA HAHA"
  applyTwice (3:) [1] -- [3,3,1]
可以用函数作为参数, 注意类型定义的括号是必须的
  zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
  zipWith' _ [] _ = []
  zipWith' _ _ [] = []
  zipWith' f [x:xs] [y:ys] = f x y : zipWith' f xs ys
`zipWith`是高阶函数编程中一个重要函数, 接受一个函数/两个变量作为参数,
`a, b, c`未必要相同的类型, 不确定时先写出内容再`:t`看类型,
边缘的情况, 数列长度不同时用`_`表示:
  zipWith' (+) [4, 2, 5, 6] [2, 6, 2, 3] -- [6,8,7,9]
  zipWith' max [6, 3, 2, 1] [7, 3, 1, 5] -- [7,3,2,5]
  zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
  -- ["foo fighters","bar hoppers","baz aldrin"]
  zipWith' (*) (replicate 5 2) [1..] -- [2,4,6,8,10]
  zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
  -- [[3,4,6],[9,20,30],[10,12,12]]
准确基本的告诫函数可以广泛使用, `zipWith'`函数:
对比命令式编程那种循环加检测来判断是否符合条件的方式,
函数式编程用抽象的方式探测和过滤是否满足条件并完成计算
另一个`flip`函数, 接受一个缺两参数的函数将两参数顺序调换,
  flip' :: (a -> b -> c) -> (b -> a -> c)
  flip' f = g
    where g x y = f y x
这里`x, y`是`f, g`隐含的参数, 看去有些突兀:
  flip' :: (a -> b -> c) -> b -> a ->c
  flip' f x y = f y x
类型定义中后一个括号可有可无, 不影响接收参数, 另外直接明写也是可以的:
  flip' zip [1..5] "hello" -- [('h',1),('e',2),('l',3),('l',4),('o',5)]
  zipWith (flip' div) [2, 2..] [10, 8.. 2] -- [5, 4, 3, 2, 1]
看下用例, `(flip' div)`作为参数传给 zipWith:

  map' :: (a -> b) -> [a] -> [b]
  map' _ [] = []
  map' f (x:xs) => f x : map' f xs
`map`函数接收一个函数和一个列表作为参数,
  map (+3) [1, 5, 3, 1, 6] -- [4,8,6,4,9]
  map (++ "!") ["BIFF", "BANG", "POW"] -- ["BIFF!","BANG!","POW!"]
  map (replicate 3) [3..6] -- [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
  map (map (^2)) [[1,2],[3,4,5,6],[7,8]] -- [[1,4],[9,16,25,36],[49,64]]
  map fst [(1,2),(3,5),(6,3),(2,6),(2,5)] -- [1,3,6,2,2]
返回列表中元素逐个用函数处理的值的列表:
`map`函数属于高阶函数少有的广泛使用, 看例子
  [x+3 | x <- [1,5,3,1,6]] -- [4,8,6,4,9]
`map`函数的功能用列表解析模拟, 比如:
`map`的写法相较更清晰, 特别嵌套使用不会因为括号而糊涂
  filter' :: (a -> Bool) -> [a] -> [a]
  filter' _ [] = []
  filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs
`filter`函数接收一个判断函数`p`加一个列表,
返回经函数`p`判断为真的值所组成的列表
  filter (>3) [1,5,3,2,1,6,4,3,2,1] -- [5,6,4]
  filter (==3) [1,2,3,4,5] -- [3]
  filter even [1..10] -- [2,4,6,8,10]
  let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5]
                                                 ,[2,2],[],[],[]]
  -- [[1,2,3],[3,4,5],[2,2]]
  filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"
  -- "uagameasadifeent"
  filter (`elem` ['A'..'Z']) "i lauGh At You BecAuse u r aLL the Same"
  -- "GAYBALLS"
当`p x`返回`True`时, 和非时分开两个结果:
`filter`和列表解析选取还是考虑可读性, 因为功能相近,
列表解析中可以用`&&`来模拟多层的筛选, 或者多层列表解析
  quicksort :: (Ord a) => [a] -> [a]
  quicksort [] = []
  quicksort (x:xs) =
    let
      smallerSorted = quicksort (filter (<=x) xs)
      biggerSorted = quicksort (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted
`quicksort`算法因此稍微简化一些来写
  largestDivisible :: (Integral a) => a
  largestDivisible = head (filter p [100000, 99999..])
    where p x = x `mod` 3829 == 0
列表解析和高阶函数有时轻松处理命令式编程中大量循环的判断,
而且由于惰性计算, 多余的`filter`和`map`也能避免重复执行(?)
例子中当取出第一个满足的数时不再计算, 得益于惰性计算

  takeWhile' :: (a -> Bool) -> [a] -> [a]
  takeWhile' p (x:xs)
    | p x = x : takeWhile' p xs
    | otherwise = []
`takeWhile`函数接收一个判断和一个列表作为参数,
顺序判断每个元素, 将返回错误前的元素组成列表返回:
  sum (takeWhile (<10000) (filter odd (map (^2) [1..]))) -- 166650
  sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)]) -- 166650
`takeWhile`与`filter`相仿, 但前者在第一次产生`false`时即终止的
列表解析的方式也可以写出该函数, 然而完全用列表解析会成为无限
  chain :: (Integral a) => a -> [a]
  chain n
    | even n = n : chain (n `div` 2)
    | odd n = n : chain (n*3 + 1)
角谷猜想, 即: 取一自然数(不含零)判断`(mod x 2)`,
真则取`x/2`, 否则取`(3*x+1)`, 继续对所取得数进行此步骤, 直到取出`1`
记录此过程步骤, 问`[1..100]`有几个数步长`(>15)`?
注意了不能用`(/2)`代替上面的除以`2`, 似乎是浮点数问题,
  chain 10 -- [10,5,16,8,4,2,1]
  chain 1 -- [1]
另外`1`在程序中特别处理
  numLongChains :: Int
  numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15
然后用`numLongChains`来返回结果, 用`isLong`来判断长短:
使用`Int`的原因是`length`返回值是`Int`类型的, 具体看原文
  [(0*),(1*),(2*),(3*),(4*),(5*)..
另外还能使用`(map (*) [1..])`, 返回元素函数的列表,
  let listOfFuns = map (*) [0..]
  (listOfFuns !! 4) 5 -- 20
然而这不属于`Show`于是不能打印. 用这种方式探测

  --
`Lambdas`基本用来写一次性匿名函数代入高阶函数当中,
书写时先用'\'再写参数再写"->"再写函数体最后包围以括号,
  numLongChains :: Int
  numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))
在上面的例子中直接用`Lambdas`代替`filter`的判断:
像`(+3)`与`(\x -> x+3)`等价, 前者的简洁, 后者没有必要
  zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
  -- [153.0,61.5,31.0,15.75,6.6]
`Lambdas`就像一般函数, 可以带有任意多个参数:
  map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]
  -- [3,8,9,8,7]
`Lambdas`中的模式匹配不能匹配两种模式, 比如`[]`和`[x:xs]`, 慎用
  addThree :: a -> a -> a -> a
  addThree x y z = x+y+z
  addThree = \x -> \y -> \z -> x+y+z
  addThree = \x y z -> x+y+z
不用括号的情况, 因函数本身被`curried`, 后三行等价
  flip' :: (a -> b -> c) -> b -> a -> c
  flip' f = \x y -> f y x
借助`Lambdas`函数`flip`还可以这样写

  --
前文中`(x:xs)`广泛使用, 因而制造`fold`一类函数来做此类事情
  foldl' (a -> b -> a) -> a -> [b] -> a
  foldl' _ x [] = x
  foldl' f y (x:xs) = f y (foldl' x xs)
一个`fold`函数接收一个二元函数一个初值和一个列表为参数
逐个抽取列表元素与初值代入函数中运算, 返回值代入初值直到结束
比如`foldl`从左边开始取列表的元素
  sum' :: (Num a) => [a] -> a
  sum' xs = foldl' (\acc x -> acc + x) 0 xs
再来写`sum`函数累加列表各元素:
  sum' :: (Num a) => [a] -> a
  sum' = foldl (+) 0
考虑到函数是`curried`, 并且`(+)`可以简化, 简写
elem' :: (Eq a) => a  -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x==y then True else acc) False ys
一般出于`curried`特性, `(foo a = bar b a)`简写`(foo = bar b)`
用`foldl`再现`elem`函数, 遍历一次列表
  foldr' :: (a -> b -> b) -> b -> [a] -> b
  foldr' _ x [] = x
  foldr' f x xs = f x (foldr' f (last xs) (init xs))
`foldr`顾名思义是从列表右侧开始遍历, 同时
`Lambdas`中参数顺序需要改为`(\x acc)`, 与`foldl`相反
因为没有现成的`(x:xs)`模式匹配, 我用了`init`/`last`来:
  map' :: (a -> b) -> [a] -> [b]
  map' f xs = foldr (\x acc -> f x : acc) [] xs
然后用来实现`map`函数:
  map' f xs = foldl (\acc x -> acc ++ [f x]) [] xs
上面的`map`也能用`foldl`实现, 因为`(++)`非常灵活:
  sum' = foldl1 (+)
  maximum' :: (Ord a) => [a] -> a
  maximum' = foldl1 (\x y -> if x>y then x else y)
  reverse' :: [a] -> [a]
  reverse' = foldl (\acc x -> x: acc) []
  product' :: (Num a) => [a] -> a
  product' = foldl1 (*)
  filter' :: (a -> Bool) -> [a] -> [a]
  filter' p = foldr (\x acc -> if p x then x:acc else acc) []
  head' :: [a] -> a
  head' = foldl (\x _ -> x)
  last' :: [a] -> a
  last' = foldl (\_ y -> y)
`foldl`和`foldr`区别在于无穷列表处理上, 未给例子
`foldl1`与`foldr1`不用给初值, 因而对`[]`会出错
按最后意思,`fold`将结果作为参数放置后, 这需要注意
其中`reverse`还可以写成`reverse' = foldl (flip (:)) []`
  scanl' :: (a -> b -> a) -> a -> [b] -> [a]
  let scanl' f x xs = reverse $ foldl (\acc y -> f x y : acc) [x] xs
  scanl (+) 0 [3,5,2,1] -- [0,3,8,10,11]
  scanr (+) 0 [3,5,2,1] -- [11,8,3,1,0]
  scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
  -- [3,4,5,5,7,9,9,9]
  scanl (flip (:)) [] [3,2,1] -- [[],[3],[2,3],[1,2,3]]
然后引入`scanl`/`scanr`相近`fold`但打印每一步结果成数列:
`scan`常用在监测那写`fold`方式开展的运算过程,
  sqrtSums :: Int
  sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
  -- 131
题目: 多少个自然数平方根和刚好`(<1000)`?
这里不能用`filter`因其不能处理无穷列表, 而用`takeWhile`
运算过程转化为列表在`Haskell`更容易处理

  ($) :: (a -> b) -> a -> b
  f $ x = f x
`$`称为"function application", 也直接是个函数
  sum (map sqrt [1..100])
  sum $ map sqrt [1..100]
  sqrt (3+4+9)
  sqrt $ 3+4+9
  sum (filter (> 10) (map (*2) [2..10]))
  sum $ filter (> 10) $ map (*2) [2..10]
其优先级最低, 其他操作符左联, `($)`是右联的, 两两等效
  map ($ 3) [(4+), (10*), (^2), sqrt]
  -- [7.0,30.0,9.0,1.7320508075688772]
注意下面用法, 比如`($ 3) (4 +) -- 7`
  (.) :: (b -> c) -> (a -> b) -> a -> c
  f . g =\x -> f (g x)
复合函数:`f (g x)`, 其定义
注意前一个函数接受的与后一个返回的类型应当一致,
  map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
  map (negate . abs) [5,-3,-6,7,-3,2,-19,24]
  -- [-5,-3,-6,-7,-3,-2,-19,-24]
  map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
  map (negate . sum . tail) [[1..5],[3..6],[1..7]]
  -- [-14,-15,-27]
`Lambdas`功能强大, 但很多时候用符合函数更为明了:
  sum (replicate 5 (max 6.7 8.9))
  (sum . replicate 5 . max 6.7) 8.9
  sum . replicate 5 . max 6.7 $ 8.9
  replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
  replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]
  replicate 100 $ product $ map (*3) $ zipWith max [1,2,3,4,5] [4,5,6,7,8]
连续嵌套的带多个参数的表达式将末尾一个用`($)`隔开:
但其实`($)`隔开也是挺相似的, 后三句等价
  sum' :: (Num a) => [a] -> a
  sum' xs = foldl (+) 0 xs
  fn x = ceiling (negate (tan (cos (max 50 x))))
  fn = ceiling . negate . tan . cos . max 50
  oddSquareSum :: Integer
  oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
  oddSquareSum :: Integer
  oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
不明白为甚这叫无点样式, 直接看下成对对比的简写
  oddSqureSum :: Int
  oddSqureSum =
    let
      oddSquares = filter odd $ map (^2) [1..]
      belowLimit = takeWhile (<1000) oddSquares
    in sum belowLimit
作者为了可读性更好建议最后一条用`let`/`in`写, 比较特别:
