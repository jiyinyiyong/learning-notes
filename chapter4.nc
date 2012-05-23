  --
在`Prelude`中定义函数需要使用`let`, 而脚本中不需要
  lucky :: Integral a => a -> String
  lucky 7 = "lucky!"
  lucky x = "sorry.."
注意定义`7`和定义`x`不可以调换顺序, 代码将从上往下匹配
保存为当前目录脚本, `Prelude> :l `加文件名载入函数
  factorial :: Integral a => a -> a
  factorial 0 = 1
  factorial n = n * factorial (n - 1)
`Prelude> :r`重新载入函数, 可以计算
函数调用时会去匹配写好的模式, 注意尽可能将需要的模式涵盖, 否则会出错
  addVectors :: Num a => (a, a) -> (a, a) -> (a, a)
  addVectors a b = (fst a + fst b, snd a + snd b)
  addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
  addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
用操作元组的方法改写`addVectors`函数
  first :: (a, b, c) -> a
  first (x, _, _) = x
  second :: (a, b, c) -> b
  second (_, y, _) = y
  third :: (a, b, c) -> c
  third (_, _, z) = z
这里的`_`表示不管具体是什么类型. 这里定义了操作`triple`的函数
  1:2:3:[] -- [1, 2, 3]
`x:y:z:zs`是列表真实的写法, 从这里可以看列表的模式匹配
  head' :: [a] -> a
  head' [] = error "Can't call head on an empty list, dummy!"
  head' (x:_) = x
自己定义一个`head`, 注意单引号是可以放在末尾的
`error`可以抛出一个异常.
  tell :: Show a => [a] -> String
  tell [] = "empty"
  tell (x:[]) = "one elem: " ++ show x
  tell (x:y:[]) = "two elems: " ++ show x ++ "," ++ show y
  tell (x:y:_) = "ok, have: " ++ show x ++ "," ++ show y
兼顾了不同的列表长度的例子
  length' :: (Num b) => [a] -> b
  length' [] = 0
  length' (_:xs) = 1 + length' xs
用递归方式求数组长度. `_`因为不用区别其类型
  capital :: String -> String
  capital "" = "Empty string.."
  capital str@(x:xs) = "head of " ++ str ++ " is " ++ [x]
形如`xs@(x:y:ys)`以方便使用全体和部分的`xs`
  infixr 3  &&
  (&&)  :: Bool -> Bool -> Bool
  False && x   = False
  True  && x   = x
重载运算符时先确定结合性`(infixl|infixr|infix)`, 然后优先级:

  max' :: (Ord a) => a -> a -> a  max' a b
     | a > b     = a
     | otherwise = b
称为`Guard`的选择语句, 相当`switch/case`, 可以缩进或不缩进, 可以嵌套
  head' :: [a] -> a
  head' xs = case xs of [] -> error "No head for empty lists!"
          (x:_) -> x
`case`意思大概那样, 实际上和`where`和其他的用法可对转, 具体看教程
  f x = x * y where y = y * 4
还可以用`where (a, b) = (1, 3)`的方式简化表达式
`where`关键字用来简化函数当中某个频繁语句的书写, 可以缩进或者不缩进,
`where`绑定的内容是私有的, 仅函数内部可见, 不能在函数间共用
  initials :: String -> String -> String
  initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
      (l:_) = lastname
教程提到了`global`的用法, 实际上等同于定义函数作为关键字
例子有迷惑性, 实际是直接完成了 pattern matching, 疯狂
  calcBmis :: (RealFloat a) => [(a, a)] -> [a]
  calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2
细看`where`还被用来定义函数
  cylinder :: (RealFloat a) => a -> a -> a
  cylinder r h =
    let sideArea = 2 * pi * r * h
         topArea = pi * r ^2
    in  sideArea + 2 * topArea
`let`对比`where`, 不局限于函数; 但不能跨越`guard`使用, 非常`local`
  4 * (let a = 9 in a + 1) + 2 -- 42
  [let square x = x * x in (square 5, square 3, square 2)]
  -- [(25, 9, 4)]
  (let a=100;b=200;c=300 in a*b*c, let foo="Hey ";bar="there!" in foo++bar)
  -- (6000000,"Hey there!")
`where`属于语法构造, 而`let`是独立的表达式, 可以用在各种位置
这一句当中分号不可以省略, 最后一个绑定可以省略
  (let (a,b,c) = (1,2,3) in a+b+c) * 100  -- 600
`let`还可以用在`list comprehension`里边
  calcBmis :: (RealFloat a) => [(a, a)] -> [a]
  calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]
`let`可以用在定义函数当中, 这也就是`ghci`里的常用
  let boot x y z = x * y + z in boot 3 4 2
就因为`let`过于`local`, 所以不能代替`where`用在定义函数