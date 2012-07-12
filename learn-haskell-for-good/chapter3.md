
    :t 'a' -> 'a' :: char
    :t "a" -> "a" :: [Char]

`:t 'a'`用来察看类型, 这是字符串  
注意字符和字符串的区别, 后者大概是数组  

    removeNonUppercase :: [Char] -> [Char]
    removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

定义函数, 静态类型编译前检测, 有类型推断因此不用每次声明类型  
不过类型推断对性能影响较大, 还是建议添加类型生声明  

    addThree :: Int -> Int -> Int -> Int
    addThree x y z = x + y + z

关于声明, 后面还会讲. 最后一个`Int`是返回的类型  
如果不确定怎样使用类型, 先`Prelude`写函数, 然后用`:t`查看推断的类型  

    "Int -2147483648 ~ 2147483647
    Interger not bounded
    Float is a real floating point with single precision,
    Double is a real floating point with double the precision!"

`Int`类型的范围, 而`Interger`则是无界的整数  

    "Bool is a boolean type. It can have only two values: True and False.
    Char represents a character. It's denoted by single quotes.
    A list of characters is a string"

`Char`类型单引号. `String`就说了列表了  
`()` 也是类型, 和列表类似, 具体类型还要看所含元素的类型  

    Prelude> :t head
    head :: [a] -> a

这里的`a`不是具体的类型, 叫类型变量, 对应函数叫多态函数, 类似泛型(?)  

    Prelude> :t fst
    fst :: (a, b) -> a

类型变量也可以是不同的. 习惯上字母`a, b, c, d...`来表示  

    Prelude> :t (==)
    (==) :: (Eq a) => a -> a -> Bool

`=>`的意思是类型约束. 意思是, 参数满足类型约束,  a 属于 Eq 这个类  
只有 Eq 这个类里数相互可以比较, `IO`之外的类型都属于`Eq`类型  
`Eq`类型有两个函数`==`和`/=`  

    Prelude> :t (>)
    (>) :: (Ord a) => a -> a -> Bool

`Ord`类型用于比较, 有函数`>`, `<`, `>=`, `<=`  
函数不属于`Ord`类型. `compare`函数接受`Ord`返回`Ordring`类型  

    Prelude> :t compare
    compare :: Ord a => a -> a -> Ordering

`Ordering`类型包含`GT`, `LT`, `EQ`  

    Prelude> :t show
    show :: (Show a) => a -> Strin

`Show`类型才可以用字符串表示出来, 这点和动态语言不一样  

    Prelude> :t read
    read :: (Read a) => String -> a
    read "[1,2,3,4]" ++ [3] -- [1, 2, 3, 4]
    read "[1,2,3,4]" :: [Int] -- [1, 2, 3, 4]

`Read`相反, 读取字符串到相应类型  

    Prelude> read "4"
    Ambiguous type variable `a0' in the constraint:
      (Read a0) arising from a use of `read'
    read "4" :: Int -- 4
    read "4" :: Float -- 4.0

解释器不能确定代码的歧义，有时需要手动提示类型  

    succ 'B' -- 'C'

`Enum`类, 可以被求值. 有`succ`和`pred`两个函数可用, 可用列表生成(?)  
`Bounded`类表示边界. 是多态常量(?).  

    Prelude> :t (*)
    (*) :: Num a => a -> a -> a

`Num`类是所有表示数字的, 又是多态常量...  
`Integral`整数类. 包括`Int`和`Integer`  
`Floating`浮点数, 包括`Float`和`Double`  

    Prelude> :t fromIntegral
    fromIntegral :: (Integral a, Num b) => a -> b
    Prelude> :t (length [2, 3])
    (length [2, 3]) :: Int
    Prelude> :t 3
    3 :: Num a => a
    Prelude> fromIntegral (length [1,2,3,4]) + 3.3
    7.3

用该函数把`Int`转化到更普遍的`Num`类才能计算  