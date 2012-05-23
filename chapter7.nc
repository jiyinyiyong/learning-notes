  --
`module`模块是相关函数/ 类型/ 类型类的组合,
程序一般就是主程序加载模块, 从中获取函数来处理事务,
模块可以重用, 自足的模块可以被别的意图的程序使用,
Haskell 若干个不同功能的模块组成,
前面涉及属于默认自动装载的`Prelude`(前奏?)模块
载入模块的语法为:`import Data`, 在使用函数之前,
  import Data.List
  numUniqeus :: (Eq a) => [a] -> Int
  numUniqeus = length . nub
载入`Data.List`模块处理列表, 找出不重复的元素个数,
`nub`是`Data.List`模块中除去重复元素返回列表的函数:
执行`import`之后, 所有`Data.List`模块的函数可以在全局命名空间使用
`(length . nub)`符合函数等价于`(\xs -> length $ nub xs)`
  Prelude> :m + Data.List Data.Map Data.Set
`GHCI`当中可用`:m + Data.List`来载入模块, 同时载入多个
对应大致有个`:m - `来释放模块, 在脚本中载入模块亦可
  import Data.List (nub, sort)
只想载入某些函数, 比如`nub`和`sort`
  import Data.List hiding (nub)
若自定义了`nub`, 不想从模块加载
  --
像`Data.Map`里`filter`, `null`函数与`Prelude`冲突的话,
冲突时使用, 当尝试指定载入`import Data.list (filter)`
会报错询问选`Prelude.filter`还`Data.List.filter`
  import qualified Data.Map
下面语句照常用`filter`, 而载入另一个到`Data.List.filter`
  import qualified Data.List as M
因为`Data.List.filter`太长, 想简化`M.filter`
  --
下面链接查阅标准库中有哪些模块, 看去真复杂
http://www.haskell.org/ghc/docs/latest/html/libraries/
也可以去 Hoogle 搜索函数名, 模块名, 类型声明:
http://haskell.org/hoogle/

  intersperse '.' "MONKEY" -- "M.O.N.K.E.Y"
来看`Data.List`, 因`Prelude`是从这里取的, `filter`等一般不冲突
`intersperse`接收一个字符一个列表, 用字符散开列表
  intersperse 0 [1,2,3,4,5,6] -- [1,0,2,0,3,0,4,0,5,0,6]
  intercalate " " ["hey","there","guys"] -- "hey there guys"
  intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]
  -- [1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]
`intercalate`接受一字符串和一字符串列表, 以前者间隔后者返回字符串
  transpose [[1,2,3],[4,5,6],[7,8,9]] -- [[1,4,7],[2,5,8],[3,6,9]]
  transpose ["hey","there","guys"] -- ["htg","ehu","yey","rs","e"]
`transpose`将二维列表看作矩阵, 行列互换后返回
  (3x^2 + 5^x + 9)+(10x^3 + 9)+(8x^3 + 5x^2 + x - 1)
  map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]
列多项式相加, 将各多项式系数相加在一起
模块带了`foldl'`和`foldl1'`两个更严格的函数,
两者用于处理大型列表时容易犯错, 详见原文
  concat ["foo","bar","car"] -- "foobarcar"
  concat [[3,4,5],[2,3,4],[2,1,1]] -- [3,4,5,2,3,4,2,1,1]
`concat`将列表的列表扁平化为列表:
只能处理一层列表, 多层需要使用多次
  concatMap (replicate 4) [1..3] -- [1,1,1,1,2,2,2,2,3,3,3,3]
`concatMap`相当于`(concat . map)`, 先执行`map`
  and $ map (>4) [5,6,7,8] -- True
  and $ map (==4) [4,4,4,3,4] -- False
`and`接收布尔值的列表作为参数, 全为`True`时返回`True`
  or $ map (==4) [2,3,4,5,6,1] -- True
  or $ map (>4) [1,2,3] -- False
`or`接收布尔值列表作为参数, 存在`True`时返回`True`
  any (==4) [2,3,5,6,1,4] -- True
  all (>4) [6,9,10] -- True
  all (`elem` ['A'..'Z']) "HEYGUYSwhatsup" -- False
  any (`elem` ['A'..'Z']) "HEYGUYSwhatsup" -- True
`any`接收一判断加一列表, 当列表存在判断真则返回`True`
  take 10 $ iterate (*2) 1 -- [1,2,4,8,16,32,64,128,256,512]
  take 3 $ iterate (++ "haha") "haha" -- ["haha","hahahaha","hahahahahaha"]
`iterate接收一函数加一初值, 重复将初值代入计算, 返回结果列表
  splitAt 3 "heyman" -- ("hey","man")
  splitAt 100 "heyman" -- ("heyman","")
  splitAt (-3) "heyman" -- ("","heyman")
  let (a,b) = splitAt 3 "foobar" in b ++ a -- "barfoo"
`splitAt`接收一数值加一列表, 按数值进行一次截断, 返回元组
  takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1] -- [6,5,4]
  takeWhile (/=' ') "This is a sentence" -- "This"
`takeWhile`接收一判断加一列表, 返回判断出错前部分的列表:
  sum $ takeWhile (<10000) $ map (^3) [1..] -- 53361
比如计算`10000`以内三次方之和:
  dropWhile (/=' ') "This is a sentence" -- " is a sentence"
  dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1] -- [3,4,5,4,3,2,1]
`dropWhile`接收一判断加一列表, 从判断错误开始返回列表, 与上互补
  let stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3)
              ,(1001.4,2008,9,4),(998.3,2008,9,5)]
  head (dropWhile (\(val,y,m,d) -> val < 1000) stock)
  -- (1001.4,2008,9,4)
例子按列表中元组首个元素大于`1000`筛选元组, 给出首个结果
  break (==4) [1,2,3,4,5,6,7]  -- ([1,2,3],[4,5,6,7])
  span (/=4) [1,2,3,4,5,6,7] -- ([1,2,3],[4,5,6,7])
`span`接收一判断加一列表, 开始连续否和其余部分, 用元组中列表返回
`break`接收一判断一列表, 开始连续真和其余部分, 用元组中列表返回
`(break p)`与`(span $ not . p)`等价
  sort [8,5,3,2,1,6,4,2] -- [1,2,2,3,4,5,6,8]
  sort "This will be sorted soon" -- "    Tbdeehiillnooorssstw"
`sort`接受一列表排序后返回一列表, 元素需是 Ord 类型的:
  `group`[1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]  
  -- [[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]
`group`接收一列表, 将相邻相同元素合并为列表, 返回列表嵌列表:
  map (\l@(x:xs) -> (x,length l)) . group . sort $
                    [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
  -- [(1,4),(2,7),(3,2),(5,1),(6,1),(7,1)]
先`sort`, 然后`groups`, 用来统计列表相同元素数量:
  inits "w00t" -- ["","w","w0","w00","w00t"]
  tails "w00t" -- ["w00t","00t","0t","t",""]
  let w = "w00t" in zip (inits w) (tails w)  
  -- [("","w00t"),("w","00t"),("w0","0t"),("w00","t"),("w00t","")]
  search :: (Eq a) => [a] -> [a] -> Bool
  search needle haystack =
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then
                        True else acc) 
                        False (tails haystack)
`tails`可以用在搜索片断中, 编写函数
  "cat" `isInfixOf` "im a cat burglar" -- True
  "Cat" `isInfixOf` "im a cat burglar" -- False
  "cats" `isInfixOf` "im a cat burglar" -- False
`isInfixOf`接收两字符串判断后者是否包含前者, 返回布尔值:
  "hey" `isPrefixOf` "hey there!" -- True
  "hey" `isPrefixOf` "oh hey there!" -- False
  "there!" `isSuffixOf` "oh hey there!" -- True
  "there!" `isSuffixOf` "oh hey there" -- False
`isPrefixOf`和`isSuffixOf`判断是否在头部或尾部
notElem '3' "3.1415" -- False
`elem`和`notElem`检测元素是否在列表当中:
  partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"  
  -- ("BOBMORGAN","sidneyeddy")
  partition (>3) [1,3,5,6,3,2,1,0,3,7]  
  -- ([5,6,7],[1,3,3,2,1,0,3])
`partition`接收一判断一列表, 按条件分成两列表, 以元组返回
  find (>4) [1,2,3,4,5,6] -- Just 5
  find (>9) [1,2,3,4,5,6] -- Nothing
  :t find -- find :: (a -> Bool) -> [a] -> Maybe a
`find`接收一判断一数组, 返回第一个判断为真的元素, 只是
`Maybe`类型将在后面章节解释, 可对有值无值做返回, 较安全
  :t elemIndex -- elemIndex :: (Eq a) => a -> [a] -> Maybe Int
  4 `elemIndex` [1,2,3,4,5,6] -- Just 3
  10 `elemIndex` [1,2,3,4,5,6] -- Nothing
`elemIndex`似`elem`, 但返回索引值, 也用`Maybe`类型
  ' ' `elemIndices` "Where are the spaces?" -- [5,9,13]
`elemIndices`类似上条, 但返回多个索引的列表
  findIndex (==4) [5,3,2,1,6,4] -- Just 5
  findIndex (==7) [5,3,2,1,6,4] -- Nothing
  findIndices (`elem` ['A'..'Z']) "Where Are The Caps?" -- [0,6,10,14]
`findIndex`似`find`, 但返回多个索引的列表或`Nothing`

  zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]
  -- [7,8,9]
  zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]
  -- [(2,2,5,2),(3,2,5,2),(3,2,3,2)]
多个列表的有`[3..8]`的`zip`和`zipWith`, 比如`zip3`
  lines "first line\nsecond line\nthird line"
  -- ["first line","second line","third line"]
`lines`在处理文件或文本时将带`'\n'`的字符串分开返回列表
  unlines ["first line", "second line", "third line"]
  -- "first line\nsecond line\nthird line\n"
`unlines`与`lines`相反, 但注意结尾多出`'\n'`
  words "hey these are the words in this sentence"
  -- ["hey","these","are","the","words","in","this","sentence"]
  words "hey these           are    the words in this\nsentence"
  -- ["hey","these","are","the","words","in","this","sentence"]
`words`和`unwords`转换句子到单词, 以及相反, 通过空格和`'\n'`识别
unwords ["hey","there","mate"] -- "hey there mate"
  'h' . delete 'h' $ "hey there ghang!" -- "ey tere ghang!"
  delete 'h' . delete 'h' . delete 'h' $ "hey there ghang!"
  -- "ey tere gang!"
  delete 'h' "hey there ghang!" -- "ey there ghang!"
`delete`接收一字符一字符串, 删去第一个匹配字符后返回

  [1..10] \\ [2,5,9] -- [1,3,4,6,7,8,10]
  "Im a big baby" \\ "big" -- "Im a  baby"
`(\\)`接受两列表, 从及一个列表减去第二个, 似`delete`只删除一次
  "hey man" `union` "man what's up" -- "hey manwt'sup"
  [1..7] `union` [5..10] -- [1,2,3,4,5,6,7,8,9,10]
`union`接收两列表检查逐个后者在前者不含时追加
  [1..7] `intersect` [5..10] -- [5,6,7]
`intersect`接收两列表返回交集
  insert 4 [3,5,1,2,8,2] -- [3,4,5,1,2,8,2]
  insert 4 [1,3,4,4,1] -- [1,3,4,4,4,1]
  insert 4 [1,2,3,5,6,7] -- [1,2,3,4,5,6,7]
  insert 'g' $ ['a'..'f'] ++ ['h'..'z'] -- "abcdefghijklmnopqrstuvwxyz"
  insert 3 [1,2,4,3,2,1] -- [1,2,3,4,3,2,1]
`insert`接收一字符一列表, 插入到首个不小于自身的元素前
  length, take, drop, splitAt, !!, replicate
  genericLength, genericTake, genericDrop, genericSplitAt
                             , genericIndex, genericReplicate
历史原因上面有些函数返回 Int, 不能用于出除法, 于是另有`Num`类型函数
  nub, delete, union, intersect, group
  nubBy, deleteBy, unionBy, intersectBy, groupBy
同样也是更通用的版本
  let values = [ -4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5
               , 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]
  groupBy (\x y -> (x > 0) == (y > 0)) values
  -- [[-4.3,-2.4,-1.2], [0.4,2.3,5.9,10.5,29.1,5.3]
                      , [-2.4,-14.5],[2.9,2.3]]
前者限定条件(==), 后者接收一个函数作为分组的条件
比如`group`等价`(groupBy (==))`
  on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
  f `on` g = \x y -> f (g x) (g y)
  groupBy ((==) `on` (> 0)) values
  -- [[-4.3,-2.4,-1.2], [0.4,2.3,5.9,10.5,29.1,5.3]
     ,[-2.4,-14.5],     [2.9,2.3]]
使用`Data.Function`模块的`on`函数更简洁
  sortBy :: (a -> a -> Ordering) -> [a] -> [a]
另几函数类似, 比如`sort`和`sortBy`
  let xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]
  sortBy (compare `on` length) xs
  -- [[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]]
`Ordering`可以是`LT`,`EQ`,`GT`. `sort`等价`(sortBy compare)`
  (compare `on` length)
  (\x y -> length x `compare` length y)
两者等价
  
  --
`Data.Char`处理函数字符, 甚至过滤以及映射
`isControl`判断是否是控制字符, 具体看链接:
http://book.realworldhaskell.org/read/characters-strings-and-escaping-rules.html
`isSpace`判断是否`Unicode`空白或换行之类`\t``\n``\r``\f``\v`
`isLower`是否`Unicode`小写
`isUpper`是否`Unicode`大写
`isAlpha`是否 `Unicode` 字母, 英文解释一大串不懂
`isAlphaNum`是否 `Unicode` 字母或数字
`isPrint`是否`Unicode` 可打印, 控制字符不可打印
`isDigit`是否`ASCII`数字, `['0'..'9']`中的数字
`isOctDigit`是否`ASCII`八进制数字`['0'..'7']`中
`isHexDigit`是否`ASCII`十六进制数字`['0'..'9'], ['a'..'f'], ['A'..'F']`
`isLetter`是否`ASCII`字母, 和`isAlpha`等价
`isMark`是否`Unicode`注音字符, 关于法语, 跳过
`isNumber`是否`Unicode`数字, 关系到罗马数字, 等
`isPunctuation`是否`Unicode`标点, 如连接号括号引号
`isSymbol`是否`Unicode`符号, 如数学或货币符号
`isSeparator`是否`Unicode`空格或分隔符
`isAscii`是否`Unicode`前`128`字符, 也对应`ASCII`
`isLatin1`是否`Unicode`前`256`字符, 对应`ISO 8859-1 (Latin-1)`
`isAsciiUpper`是否`ASCII`大写
`isAsciiLower`是否`ASCII`小写
  all isAlphaNum "bobby283" -- True
  all isAlphaNum "eddy the fish!" -- False
类型都是`Char -> Bool`, 对于字符串结合`Data.List.all`处理
  words "hey guys its me" -- ["hey","guys","its","me"]
  groupBy ((==) `on` isSpace) "hey guys its me"
  -- ["hey"," ","guys"," ","its"," ","me"]
  filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "its me"
  -- ["hey","guys","its","me"]
用`isSpace`模拟`Data.List.words`, 注意空格
`Data.Char`输出数据类型属于`Ordering`, 可以给`LT``EQ``GT`
  generalCategory :: Char -> GeneralCategory
  generalCategory ' ' -- Space
  generalCategory 'A' -- UppercaseLetter
  generalCategory 'a' -- LowercaseLetter
  generalCategory '.' -- OtherPunctuation
  generalCategory '9' -- DecimalNumber
  map generalCategory " \t\nA9?|"
  -- [Space,Control,Control,UppercaseLetter,DecimalNumber
           ,OtherPunctuation,MathSymbol]
`GeneralCategory`用来查看类别, 总共`31`种类别
  generalCategory c == Space
`GeneralCategory`类型属于`Eq`类型, 因此可判断
`toUpper`将小写字母转换为大写, 其他符号不发生改变
`converts`转换为小写, 其他不变
`toTitle`基本上等价`toUpper`
  map digitToInt "34538" -- [3,4,5,3,8]
  map digitToInt "FF85AB" -- [15,15,8,5,10,11]
`digitToInt`将`['0'..9]``['a'..'f']``['A'..'F']`转换数字, 其余报错
  intToDigit 15 -- 'f'
  intToDigit 5 -- '5'
`intToDigit`与上相反, 接收数字转化字符`[0..15]`
  ord 'a' -- 97
  chr 97 -- 'a'
  map ord "abcdefgh" -- [97,98,99,100,101,102,103,104]
`ord`将字符转化为`ASCII`编码值, 取决于`Unicode`,`chr`相反
  encode :: Int -> String -> String
  encode shift msg =
    let
      ords = map ord msg
      shifted = map (+ shift) msg
    in map chr shifted
通过改变编码转换字符来模式凯撒编码
  encode 3 "Heeeeey" -- "Khhhhh|"
  encode 4 "Heeeeey" -- "Liiiii}"
  encode 1 "abcd" -- "bcde"
  encode 5 "Marry Christmas! Ho ho ho!" -- "Rfww~%Hmwnxyrfx&%Mt%mt%mt&"
如果喜欢符合函数, 可以写`(map (chr . (+ shift) . ord) msg)`
  decode :: Int -> String -> String
  decode shift msg = encode (negate shift) msg
  encode 3 "Im a little teapot" -- "Lp#d#olwwoh#whdsrw"
  decode 3 "Lp#d#olwwoh#whdsrw" -- "Im a little teapot"
  decode 5 . encode 5 $ "This is a sentence" -- "This is a sentence"
解码时取相反的`shift`参数即可

  phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")]
关联列表也叫字典, 近似散列哈希表, 存放顺序无关的键值对
  findKey :: (Eq k) => k -> [(k,v)] -> v
  findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs
上面是字典的例子, 常用任务是获取给定键对应的
  findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
  findKey key [] = Nothing
  findKey key ((k,v):xs) = if key == k then Just v else findKey key xs
上面函数当不含对应键, 给出空列表时, 会出现运行时错误, 换`Maybe`类型
  findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
  findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
  findKey "penny" phoneBook -- Just "853-2492"
  findKey "betty" phoneBook -- Just "555-2938"
  findKey "wilma" phoneBook -- Nothing
该函数明显递归, 有边界条件, 有递归的调用, 换`fold`实现
  --
`lookup`函数即对应上述`findKey`, 上述函数将遍历一遍列表
`Data.Map`内部用树部署数据, 处理更快, 另有操作工具
因此从此不再用字典称呼, 而称呼其为`map`
  import qualified Data.Map as Map
`lookup`等某些函数从`Data.Map`导入到`Prelude`, 这里载入模块
  Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]
  -- fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]
  Map.fromList [(1,2),(3,4),(3,2),(5,5)] -- fromList [(1,2),(3,2),(5,5)]
`fromList`接收一个字典覆盖相同键的重复值返回一个`map`
猜测前面用`fromList`标明是内部储存用的`map`(?), 不能`(!!)`取出
  Map.fromList :: (Ord k) => [(k, v)] -> Map.Map k v
其类型声明, `k`在存储时需要是`Ord`来排序, `Map.Map`存疑(?)
  Map.empty -- fromList []
`empty`返回一个空的`map`
  Map.insert 3 100 Map.empty -- fromList [(3,100)]
  Map.insert 5 600 (Map.insert 4 200 ( Map.insert 3 100  Map.empty))
  -- fromList [(3,100),(4,200),(5,600)]
  Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty
  -- fromList [(3,100),(4,200),(5,600)]
`insert`接收一键一值一`map`, 返回加入键值后的`map`
  fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
  fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty
用`insert`可以实现`fromList`的功能, 去掉重复返回`map`
  Map.null Map.empty -- True
  Map.null $ Map.fromList [(2,3),(5,5)] -- False
`null`用来判断`map`是否为空
  Map.size Map.empty -- 0
  Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)] -- 5
`size`用来探测长度, 和`length`类似
  Map.singleton 3 9 -- fromList [(3,9)]
  Map.insert 5 9 $ Map.singleton 3 9 -- fromList [(3,9),(5,9)]
`singleton`接受一键一值创建一个`map`
  Map.lookup 2 $ Map.fromList [(2,'4')] -- Just '4'
`lookup`类似`Data.List.lookup`, 但可操作`map`
  Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)] -- True
  Map.member 3 $ Map.fromList [(2,5),(4,5)] -- False
`member`接收一键一`map`, 检查键是否在`map`中
  Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)]
  -- fromList [(1,100),(2,400),(3,900)]
  Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]
  -- fromList [(2,'A'),(4,'B')]
`map`和`filter`与`Data.List`中类似, 独作用于`map`
  Map.toList :: Map.Map k a -> [(k, a)]
  Map.toList . Map.insert 9 2 $ Map.singleton 4 3 -- [(4,3),(9,2)]
`toList`是`fromList`的反演, 特别看类型声明
  phoneBook =   
    [("betty","555-2938")
    ,("betty","342-2492")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("patsy","943-2929")
    ,("patsy","827-9162")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ,("penny","555-2111")]
`keys`和`elems`分别打印出键值为列表`
`keys`等价`(map fst . Map.toList)`, `elems`等价`(map snd . Map.toList)`
`fromListWith`接收一函数一列表返回一`map`, 相当与戴上函数的`fromList`
  phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
  phoneBookToMap xs = Map.fromListWith
                      (\number1 number2 -> number1++", "++number2) xs
  Map.lookup "patsy" $ phoneBookToMap phoneBook
  -- "827-9162, 943-2929, 493-2928"
  Map.lookup "wendy" $ phoneBookToMap phoneBook
  -- "939-8282"
  Map.lookup "betty" $ phoneBookToMap phoneBook
  -- "342-2492, 555-2938"
  phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
  phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs
  Map.lookup "patsy" $ phoneBookToMap phoneBook
  -- ["827-9162","943-2929","493-2928"]
  Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
  -- fromList [(2,100),(3,29),(4,22)]
  Map.fromListWith (+) [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
  -- fromList [(2,108),(3,62),(4,37)]
借助`fromListWith`写函数将重复的值用`,`连接或组成列表, 或最大值/ 总和
  Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]
  -- fromList [(3,104),(5,103),(6,339)]
`insertWith`类似上函数, 出现重复键时调用函数处理
全部函数见下, 可用`import Data.Map (Map)`代替加载
http://www.haskell.org/ghc/docs/latest/html/libraries/containers/Data-Map.html

  --
`Data.Set`模块提供集合的处理, 其中元素唯一
内部为了高效排序, 因此要`Ord`类型, 速度很快
常用操作是插入元素/ 检查成员关系/ 转换为列表
  import qualified Data.Set as Set
  text1 = "I just had an anime dream. Anime... Reality...
           Are they so different?"
  text2 = "The old man left his garbage can out
           and now his trash is all over my lawn!"
  let set1 = Set.fromList text1
  let set2 = Set.fromList text2
  set1 -- fromList " .?AIRadefhijlmnorstuy"
  set2 -- fromList " !Tabcdefghilmnorstuvwy"
`Data.Set`与`Prelude`和`Data.List`命名冲突多
  Set.intersection set1 set2 -- fromList " adefhilmnorstuy"
`intersection`函数可检查两集合的交集
  Set.difference set1 set2 -- fromList ".?AIRj"
  Set.difference set2 set1 -- fromList "!Tbcgvw"
`difference`检测两集合前者有后者没有的部分:
  Set.union set1 set2 -- fromList " !.?AIRTabcdefghijlmnorstuvwy"
`union`返回两集合并集
  Set.null Set.empty -- True
  Set.null $ Set.fromList [3,4,5,5,4,3] -- False
  Set.size $ Set.fromList [3,4,5,3,4,5] -- 3
  Set.singleton 9 -- fromList [9]
  Set.insert 4 $ Set.fromList [9,3,8,1] -- fromList [1,3,4,8,9]
  Set.insert 8 $ Set.fromList [5..10] -- fromList [5,6,7,8,9,10]
  Set.delete 4 $ Set.fromList [3,4,5,4,3,4,5] -- fromList [3,5]
`null``size``member``empty``singleton``insert``delete`可从字面理解
  Set.fromList [2,3,4] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
  -- True
  Set.fromList [1,2,3,4,5] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
  -- True
  Set.fromList [1,2,3,4,5] `Set.isProperSubsetOf` Set.fromList [1,2,3,4,5]
  -- False
  Set.fromList [2,3,4,8] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
  -- False
`isSubsetOf`和`isProperSubsetOf`判断两集合前者是否后者子集和真子集
  Set.filter odd $ Set.fromList [3,4,5,6,7,2,3,4] -- fromList [3,5,7]
  Set.map (+1) $ Set.fromList [3,4,5,6,7,2,3,4] -- fromList [3,4,5,6,7,8]
同样`filter`和`map`的功能
  let setNub xs = Set.toList $ Set.fromList xs
  setNub "HEY WHATS CRACKALACKIN" -- " ACEHIKLNRSTWY"
  nub "HEY WHATS CRACKALACKIN" -- "HEY WATSCRKLIN"
集合常用`fromList`去重再借`toList`返回到列表
`Data.List.nub`也可对列表去重, 但相比速度用集合更快, 代价是
集合需要`Ord`类型限定, 而`nub`仅需要`Eq`类型限定
相较而言 nub 保持了列表原先规则, 而 setNub 不保持(?)

  module Geometry
  ( sphereVolume
  , sphereArea
  , cubeVolume
  , cubeArea
  , cuboidArea
  , cuboidVolume
  ) where
  sphereVolume :: Float -> Float
  sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)
  sphereArea :: Float -> Float
  sphereArea radius = 4 * pi * (radius ^ 2)
  cubeVolume :: Float -> Float
  cubeVolume side = cuboidVolume side side side
  cubeArea :: Float -> Float
  cubeArea side = cuboidArea side side side
  cuboidVolume :: Float -> Float -> Float -> Float
  cuboidVolume a b c = rectangleArea a b * c
  cuboidArea :: Float -> Float -> Float -> Float
  cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2
                                           + rectangleArea c b * 2
  rectangleArea :: Float -> Float -> Float
  rectangleArea a b = a * b
和很多语言一样, Haskell 可以自己写模块重用,
想这个计算体积面积的模块, 先命名为`Geomerty.hs`
  import Geometry
文件末尾`r`开头的函数使用但不输出, 不影响
  Sphere.hs :
      module Geometry.Sphere
      ( volume
      , area
      ) where
      volume :: Float -> Float
      volume radius = (4.0 / 3.0) * pi * (radius ^ 3)
      area :: Float -> Float
      area radius = 4 * pi * (radius ^ 2)
  Cuboid.hs :
      module Geometry.Cuboid
      ( volume
      , area
      ) where
      volume :: Float -> Float -> Float -> Float
      volume a b c = rectangleArea a b * c
      area :: Float -> Float -> Float -> Float
      area a b c = rectangleArea a b * 2 + rectangleArea a c * 2
                                         + rectangleArea c b * 2
      rectangleArea :: Float -> Float -> Float
      rectangleArea a b = a * b
  Cube.hs :
      module Geometry.Cube
      ( volume
      , area
      ) where
      import qualified Geometry.Cuboid as Cuboid
      volume :: Float -> Float
      volume side = Cuboid.volume side side side
      area :: Float -> Float
      area side = Cuboid.area side side side
注意需要在同一个目录, 大写开头, 加载模块似乎`ghci`不行, 脚本里正常
或, 建立 Geometry 目录, 分别创建文件
  import Geometry.Sphere -- 用来导入, 另两类似. 或者
  import qualified Geometry.Sphere as Sphere
  import qualified Geometry.Cuboid as Cuboid
  import qualified Geometry.Cube as Cube
然后以`Sphere.area``Sphere.volume``Cuboid.area`调用