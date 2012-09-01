
要自定义类型, 先看下标准库里 `Bool` 的定义  

    data Bool = False | True

`data` 声明一个新的数据类型. `=` 前部分表示类型, 这里是 `Bool`  
`|` 表示 `or`. `Int` 类型像下面这样定义:  
data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647  
不过我运行出错了, 还在社区发贴, 怀疑是伪代码..  
http://a.haskellcn.org/topic/504163aec19c6ba851042dcf  
定义 `Shape` 的话, 原型可以用 `(43.1, 55.0, 10.4)` 这样 3 个参数  
分别表示坐标, 还有半径. 另外矩形用 4 个参数  

    data Shape = Circle Float Float Float | Rectangle Float Float Float Float

可以用 `:t` 查看定义的类型, 注意是 `=` 后边的名字  

    :t Circle
    -- Circle :: Float -> Float -> Float -> Shape
    :t Rectangle
    -- Rectangle :: Float -> Float -> Float -> Float -> Shape

看来类型需要大写开头, 下面是我尝试的代码, 任一小写都会出错:  

    data Q = A
    :t A
    -- A :: Q

然后对类型可以写一个计算面积的函数可以计算该类型:  
surface :: Shape -> Float  
surface (Circle _ _ r) = pi * r ^ 2  
surface (Ractangle x1 y1 x2 y2) =  

    (abs $ x2 -x1) * (abs $ y2 - y1)

上边不能用 `Circle` 因为它不是个类型, 而是构造器  
注意这里是模式匹配, 英文原文.... 总之运行成功  

    surface $ Circle 10 20 10
    -- 314.15927
    surface $ Rectangle 0 0 100 100
    -- 10000.0

但直接输入 `Circle 10 20 10` 的话就会出现错误  
`show` 想要获取字符串, 要先修改 `Shape` 为 `show` 的类型类  

    data Shape = Circle Float Float Float
      | Rectangle Float Float Float Float
      deriving (Show)

加上 `deriving (Show)` 后 hs 自动将 `Shape` 加入 `Show` 类型类中  

    Circle 10 20 5
    -- Circle 10.0 20.0 5.0

构造器也是函数, 可以用做 `map` 的参数  

    map (Circle 10 20) [4, 5]
    -- [Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0]

可以定义一个中间类型, 这样代码可读性更好  

    data Point = Point Float Float deriving (Show)
    data Shape = Circle Point Float
      | Rectangle Point Point
      deriving (Show)
    surface :: Shape -> Float
    surface (Circle _ r) = pi * r ^ 2
    surface (Rectangle (Point x1 y1) (Point x2 y2)) =
      (abs $ x2 -x1) * (abs $ y2 - y1)

这里 `Point` 用在了构造器和类用了相同的名字, 常有的状况  
注意定义 `Point` 后参数的写法改变了, 之前的代码要调整  

    surface (Circle (Point 0 0) 24)
    -- 1809.5574

再写一个 `nudge` 来反应一个改变了坐标的 `Shape`  

    nudge :: Shape -> Float -> Float -> Shape
    nudge (Circle (Point x y) r) a b
      = Circle (Point (x + a) (y + b)) r
    nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b
      = Rectangle
        (Point (x1+a) (y1+b))
        (Point (x2+a) (y2+b))
    nudge (Circle (Point 1 2) 3) 1 2
    -- Circle (Point 2.0 4.0) 3.0

或者写个辅助函数把顶点在原点的矩形移动到对应位置  

    baseCircle :: Float -> Shape
    baseCircle r = Circle (Point 0 0 ) r

    baseRect :: Float -> Float -> Shape
    baseRect width height
      = Rectangle (Point 0 0) (Point width height)
    nudge (baseRect 40 100) 60 23
    -- Rectangle (Point 60.0 23.0) (Point 100.0 123.0)

文件头部加上代码导出为模块, 注意构造器加上 `(..)`  

    module Shapes
    ( Point(..)
    , Shape(..)
    , surface
    , nudge
    , baseRect
    , baseCircle
    ) where

### Record Syntax  
然后是一个描述个人信息的数据类型  

    data Person
      = Person String String Int Float String String
      deriving (Show)
    guy = Person "Buddy" "Finklestein" 43 184.2
      "526-2928" "Chocolate"
    -- show guy

再从一个 `Person` 中取出个人信息的就容易了  

    firstname :: Person -> String
    firstname (Person firstname _ _ _ _ _) = firstname

    age :: Person -> Int
    age (Person _ _ age _ _ _) = age

    age guy
    -- 43

简洁的写法是这样, 注意 `{` 要放在同一行的:  

    data Person = Person {
      firstname :: String
    , lastname :: String
    , age :: Int
    , height :: Float
    , phoneNumber :: String
    , flavor :: String
    } deriving (Show)
    flavor (Person "d" "d" 2 2 "d" "f")
    -- "f"
    :t flavor
    flavor :: Person -> String

Record Syntax 另外一种写法, 用下面的代码做对比  

    data Car = Car String String Int deriving (Show)
    Car "Ford" "Mustang" 1967

后边的花括号里, 允许不按顺序排列参数:  

    data Car = Car {
      company :: String
    , model :: String
    , year :: Int
    } deriving (Show)
    Car {company="x", year=2, model="dd"}
    Car {company = "x", model = "dd", year = 2}

