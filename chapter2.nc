  --
本文为 Learn You Haskell 笔记, 摘录代码, 记录理解:
http://learnyouahaskell.com/introduction#about-this-tutorial
  $ sudo aptitude install ghc -y
  $ ghci # 启动 REPL
`Ubuntu`可以直接安装`REPL`工具
在同一个目录建立文件夹内建立文件"#{filename}.hs", 把代码写成脚本,
  Prelude> :quit
  Prelude> :q
用"ghci"命令启动, "Ctrl+d"以及命令退出
  $ echo "a = 3" > demo.hs
  $ ghci demo.hs
  Prelude> :l demo.hs
  Prelude> :r
  Prelude> a
以`.hs`结尾的脚本, 通过`:l`载入, `:l`重载
  Prelude> :?
  Prelude> :{
  Prelude| "多行"
  Prelude| "多行"
  Prelude> :}
可以随时查询可用的命令, 支持多行输入
  Prelude> :! ls
  Prelude> :set prompt #{string_prompt}
执行终端命令, 和更改提示符
  $ vim $HOME/.ghci
  $ vim $HOME/.ghc/ghci.conf`
配置文件可行的位置.
注意可能要设置配置文件权限只给 owner 可写, 出错会提示
  -- 单行注释
  {-
  多行注释
  -}
更多没弄懂, 包括编译, 上面提及内容文档在这里:
http://www.haskell.org/ghc/docs/7.0.4/html/users_guide/index.html

  max 100 101  -- 101
函数和参数不用其他符号, `max`是内置的函数, 结果会返回`101`
  succ 9 + max 5 4 + 1 -- 16
  (succ 9) + (max 5 4) + 1 -- 16
函数的优先级比较高, 但后面一种写法可读性更好
  && , || , not , True , False , == , /=
逻辑运算符要求双方类型相同, 注意不等号
  Prelude> div 20 4
  Prelude> 20 `div` 4
前缀表达式可以用反引号改成中缀表达式
  Prelude> let a = 1
`ghci`当中复制和函数定义需要用`let`, 脚本中不同
后面我就不区分是否是在`Prelude>`里执行了
  doubleMe x = x + x
  doubleMe 2 -- 4
  doubleUs x y = x*2 + y*2
  doubleUs 1 2 -- 6
定义简单的函数, 注意`ghci`中加上`let`
脚本中定义变量及函数不需要严格的先后顺序来保证调用
函数可以没有参数, 输出字符串时需要双引号
函数名不能以大写字母开头, 因为那有特性的含义
单引号可在函数中间或者结尾, 表示严格版本
  if 2>1 then True else False -- True
  doubleSmallNumber x = if x > 100 then x else x*2
或者把`then`和`else`换一行用缩进的格式
  [1, 2] ++ [3,4] -- [1, 2, 3, 4]
  "hello" ++ " " ++ "world" -- "hello world"
  ['w', 'o'] ++ ['o', 't'] -- "woot"
字符串是字符的列表. 可以进行连接, 注意引号区分
  1: [2, 3, 4] -- [1, 2, 3, 4]
  1: 2: 3: [] -- [1, 2, 3]
  [1, 2, 3] !! 1 -- 2
  'A':" Cat " -- "A Cat"
冒号在列表之前加入元素, 同理字符和字符串; 但只能用在开头
`!!`可以取出列表对应序数的值
  [[6,6,6],[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
多维数组, 注意数组内数据类型需要保持一致
  [3, 2, 1] > [2, 3, 4] -- True
数组可按字典序比较大小, > , < , >= , <= , == , /=
  head [1, 2, 3, 4] -- 1
  tail [1, 2, 3, 4] -- [2, 3, 4]
  last [1, 2, 3, 4] -- 4
  init [1, 2, 3, 4] -- [1, 2, 3]
  length [1, 2, 3] -- 3
  null [] -- True
  reverse [1, 2, 3] -- [3, 2, 1]
  take 3 [1, 2, 3, 4] -- [1, 2, 3]
  drop 3 [1, 2, 3, 4] -- [4]
  maximum [1, 2, 3, 4] -- 4
  minimum [1, 2, 3, 4] -- 1
  sum [1, 2, 3, 4] -- 10
  product [1, 2, 3, 4] -- 24
  elem 4 [3, 4] -- True
函数式语言大概对列表有很强的依赖, 列表操作
后面感觉常有列表来存储数据然后设计出神奇算法的
`Lisp`就很紧密, 不过语法太不同
个人比较疑惑`Haskell`为什么不用`Lisp`一类简洁的列表写法
比如说`[1 2 3 4]`就比现在好看很多的吧
作为一种频繁使用的数据类型, 我想越短越好
也许是有别的考虑, 比如`[max 1 2, 3]`这样, 也许

  [1.. 3] -- [1, 2, 3]
  ['1'.. '9']
  ['a'.. 'z']
  ['A'.. 'z']
  [6, 4.. 1] -- [6, 4, 2]
按照范围自动生成数组, 支持简单的跳跃, 但幂次什么的不行
  take 3 [1, 3..] -- [1, 3, 5]
  take 10 (cycle [1,2,3]) -- [1,2,3,1,2,3,1,2,3,1]
  take 10 (repeat 5) -- [5,5,5,5,5,5,5,5,5,5]
  replicate 3 10 -- [10,10,10]
无限列表是常用的, 会结合`take`使用, 无限列表真的无限
  [x*2 | x <- [1.. 3]] -- [2, 4, 6]
  [x*2 | x <- [1.. 10], x*2 >= 12] -- [12, 14, 16, 18, 20]
列表解析的写法, 右边是条件, 也可以写多个条件的
  [x | x <- [1..10], odd x] -- [1, 3, 5, 7, 9]
`odd x`返回值是`True`的时候, 表示满足条件
  [x*y | x <- [2,5,10], y <- [8,10,11]]
返回 `[16,20,22,40,50,55,80,100,110]`
  boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
  boomBangs [7..13] -- ["BOOM!","BOOM!","BANG!","BANG!"]
注意结合的顺序, 并且列表解析支持稍微复杂的表达式
  length' xs = sum [1 | _ <- xs] -- 相当于 length 函数
对应列表每个元素返回一个`1`, 这些`1`加在一起就是长度
  removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
  [[x | x <- xs, even x] | xs <- [[2,3],[2,3]]] -- [[2],[2]]
列表解析可以被写得很长, 可能还需要换行, 可以很复杂
后面会说到其实列表解析的功能可用函数替代
用列表内嵌元组限制列表内的元素格式相同, 否则报错
  zip [1,2,3,4,5] [5,5,5,5,5] -- [(1,5),(2,5),(3,5),(4,5),(5,5)]
  [(a,b,c)| c<-[1..10], b<-[1..c], a<-[1..b], a^2+b^2 ==c^2, a+b+c ==24]
返回 `[(6, 8, 10)]`
  [a + b | (a, b) <- xs]
列表解析对于元组也是实用的,
  Prelude> :t ('d', 1)
  ('d', 1) :: Num t => (Char, t)
元组这个数据类型允许不同的元素, 但因此有各自特定的类型
  fst (1 2) -- 1
  snd (1 2) -- 2
当元组长度为`2`时, 有两个函数可以取出元素