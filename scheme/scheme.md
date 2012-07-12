
打算学`Scheme`, 搜了不少尝试去理解, 中文资源不如`JS`多  
`IBM`社区`5+`篇文章, 下面两篇介绍语法方面比较清晰  
http://www.ibm.com/developerworks/cn/linux/l-schm/index1.html  
http://www.ibm.com/developerworks/cn/linux/l-schm/index2.html  
关于历史掌故可以看下面这篇了解下, 比较乱, 我没有细看  
http://blog.chinaunix.net/space.php?uid=20106293&do=blog&id=142113  
`scm`的规范简介有力, 真的很短, 入门后去看下  
直接`Google`就能找到 "算法语言`Scheme`修订`5`报告"  
教程英文的不少, 中文有本`SICP`的翻译, 算清晰, 爱问搜索有  
我搜到`3`份英文教程, 打算只看最简短的第一份了  
`Teach Yourself Scheme in Fixnum Days`  
http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-1.html  
`How to Design Programs: DrScheme Companion` http://www.htdp.org/  
`The Scheme Programming Language` http://www.scheme.com/tspl3/  
我参照的这份文档只为学会用 scm 解决问题, 大不算深入  
然后我很想用上`Scheme`的缩进语法, 希望入门后去看  
http://srfi.schemers.org/srfi-49/srfi-49.html  

记得例子不少用`guile`来运行`scm`的, 在脚本开头加两行并可以空行  

    #! /usr/bin/env guile
    !#

系统没有 guile 可以在 Ubuntu 安装, 我装的是 1.8 版本  
然后脚本我不重复了, 开头缩进是笔记格式, 代码参考原文  

    ;The first program
    (begin
      (display "Hello")
      (newline))

分号开头进行注释, `begin`表示后边多个模式  
`display`是向`console`输出, `newline`是输出新的换行  
教程说的`mzscheme`不清楚, `Ubuntu`里面用的`guile`  
实际上我用的命令是`$ rlwrap guile`  
然后输入`load "hi.scm"`(点钱目录文件名)运行该脚本  
`guile`中的`prompt`是`guile>`, 这里直接输入代码  
在`prompt`中输入`"hi"`会直接输出内容  
两种方式有区别, 向`console`输出对于函数是种副作用  
而`"hi"`则是计算得到结果的  
文章约定`=>`表示模式运算后给出结果  
可以用`(exit)`退出`guile`命令行, `Linux`常快捷键`C^d`  
运行脚本可以用`guile -s hi.scm`  

@`scm`有布尔, 数值, 字符, 符号几个数据类型  
真: `#t`, 假: `#f`, 判断是否布尔类型: `boolean? #t`  
否定: `(not #t) ;=> #f`  
`scm`中数值类型有整数, 分数, 实数, 复数  
各自有`number? complex? real? rational? integer?` 判断  
整数未必十进制, 前缀`#b #o #x`分别表示二, 八, 十六进制  
比如`#b100`是二进制的`100`, 十进制的`4`  
判断大小是否相等用`(eqv? 2 #b10) => #t``(eqv? 2 2.0) => #f`  
这个广义的判别函数对于不同类型不会报错`(eqv? 2 #f) => #f`  
另外有个针对数值的判别符`(= 42 42.0) => #t`  
而这个判别符对于数值外内容会报错, 比如`(= 2 #f)`  
对于数字的大小判断还有`> < >= <=`可用  
运算符号有`+ - * /`, 都支持一个或多个参数  
其中除法结果是分数  
`(expt 2 3)`表示乘方, 只有两个参数  
`min max abs exp atan sqrt`等都可以推测  
`scm`的字符以`#\`开头比如`#\c`是字符`c`  
一般是在后面跟一个字符, 但也有用多个字母描述的比如  
`#\newline #\tab #\space`, 也有`#\ `表示空格  
字符的判别: `char? #\c ;=> #t`, 字符还有大小的判别  
`char<? char<=? char=? char>=? char>?`  
为忽略大小写用`(char-ci=? #\A #\a) => #t`, 以此类推  
字母大小写转换用`char-downcase char-upcase`  
前面这些比如`#t 42 #\c`自求值的内容  
符号被输入到解释器里, 给出运算结果一般就是本身  
符号类型不同, 因为同样的内容常被用作变量标识符  
意味着那会被计算, 并返回计算结果的内容  
但符号依然是基本的数据类型, 可以和其他类型交换  
在`scm`里用上`(quote xyz) => xyz`标记符号  
符号非常常用, 于是有了简写`'E`相当于`(quote E)`  
符号的表示以不被混淆为准, `<=>`和'$!#*'都行  
但像这些`#t "aString" -i`就不行了  
可以用`(symbol? 'xyz)`判别  
注意`scm`里对大小写不敏感, 这里不例外  
接着可以定义符号为变量`(define xyz 9)`  
在解释器里输入就会直接返回内容了`xyz ;=> 9`  
或者用`(set! xyz #\c)`这样  
复合数据类型由数据类型俺结构组合而成  
字符串是自求值的`"Hello" => "Hello"`  
该程序由一系列字符组成`(string #\H #\e #\l #\l #\o)`  
用`(string-ref "abcd" 1)`取出序号`1` 的元素  
用`(string-append "a" "b" "c")`来组合新的字符串  
可新建指定长的空字符串`(make-string 3) ;=> "\x00\x00\x00")`  
如果`(define s (make-string 3))`  
那么再给`s`赋值就注意不能越界  
`(string-set! s 0 #\s)`用来修改指定序号的字符  
向量可以容纳各种类型, 包括向量自身  
定义向量: `(vector 1 2 3) ;=> #(1 2 3)`  
也可以直接使用`#(1 2 3)`产生向量  
类似有`(make-vector 5)`来生成限定长度的向量  
类似有`vector-ref vector-set vector?`  
@点对用来组合任意两个值, 前者称`car`, 后者`cdr`  
组合两者的程序是`(cons 1 #\t) ;=> (1 . #\t)`  
简洁的定义的方式`'(1 . #\t)`  

    (define x '(1 . #\t))

取出内容通过`(car x)`或者`(cdr x)`  
设置: `(set-car! x)`和`(set-cdr! x)`  

    (define y (cons (cons 1 2) 3)) ;=> ((1 . 1.0) . 2))
    (define y (cons 1 (cons 2 3))) ;=> (1 2 . 3)

`(cdr (car y))`可以简化成`(cdar y)`最多四层  

    (cons 1 (cons 2 (cons 3 (cons 4 5)))) ;=> (1 2 3 4 . 5)

有个空的点对`'() ;=> ()`  

    '(1 . (2 . (3 . (4 . ())))) ;=> (1 2 3 4)
    (cons 1 (cons 2 (cons 3 (cons 4 '())))) ;=> (1 2 3 4)

还有个程序: `(list 1 2 3 4) ;=> (1 2 3 4)`  
还有: `'(1 2 3 4) ;=> (1 2 3 4)`  

    (define y (list 1 2 3 4))
    (list-ref y 0) ;=> 1
    (list-ref y 3) ;=> 3
    (list-tail y 1) ;=> (2 3 4)
    (list-tail y 3) ;=> (4)
    (pair? '(1 . 2)) ;=> #t
    (pair? '(1 2)) ;=> #t
    (pair? '()) ;=> #f
    (list? '()) ;=> #t
    (null? '()) ;=> #t
    (list? '(1 2)) ;=> #t
    (list? '(1 . 2)) ;=> #f

字符串和数值间通过`ASIIC`码互转, 其他较明显  

    (char->integer #\d) ;=> 100
    (integer->char 50) ;=> #\2
    (string->list "hello") ;=> (#\h #\e #\l #\l #\o)
    (number->string 16) ;=> "16"
    (string->number "16") ;=> 16
    (string->number "hi") ;=> #f
    (symbol->string 'symbol) ;=> "symbol"
    (string->symbol "string") ;=> string

基于基数的转化, 比如下面基于八进制  

    `(string->number "16" 8)`

`scm`还有个过程类型`procedure`, 目前看到都是基本过程  
基本过程在环境被支持, 也有途径创建自己的过程  
另有种数据类型`port`端口, 关联文件和终端的输入输出  
比如`display`还有个隐含的参数, 表示输出的端口  
`(display "Hello, World!" (current-output-port)`  

`scm`解释器会探测每一个形式`(form)`首个符号  
如果那是过程, 就会将其余作为参数执行这个形式  
像`begin define set!`是一些特殊形式, 特殊行为  
用户可以通过`lambda`表达式创建过程  

    (lambda (x) (+ x 2))
    ((lambda (x) (+ x 2)) 5) ;=> 7
    (define add2 (lambda (x) (+ x 2)))
    (add2 4) ;=> 6
    (define area
      (lambda (length breadth)
        (* length breadth)))
    (define area *) ;=> 同上

`apply`可以运行一个过程, 并载入指定参数  
可以载入多个参数, 但必须要列表作为参数结尾  

    (define x '(1 2 3))
    (apply + x) ;=> 2
    (apply + 1 2 3 x) ;=> 12

`begin`用来俺顺序执行参数中的子形式  
而`lambda`内部也是顺序执行的:  

    (define display3
      (lambda (arg1 arg2)
        (display arg1)
        (newline)
        (display arg2))

条件语句`if`, `else`是隐含的  

    (if #t
      (display "true"))
    (if (> 1 0)
      (display "true")
      (display "false"))

`when`用来判断, 当为真, 按顺序执行子形式  
`unelss`把`when`的条件取反, 然后顺序执行  
不过两者在`guile`里用不出来, 不考虑了  
`cond`用于判别, 每个参数都有判别是和结果  
有可选的`else`, 相似有`case`, 看例子的括号  

    (define c #\c)
    (cond ((char<? c #\c) -1)
      ((char=? c #\c) 0)
      (else 1)) ;=> 0
    (case c
      ((#\a) 1)
      ((#\c) 2)
      (else 3))

`and or not`对应: 且, 或, 非  
不过在`guile`对于多个值或其他类型参数就要出错  

    (and #\t #\f) ;=> #\f
    (or #\t #\f) ;=> #\t

@`scm`的变量作用域是词法域, 静态作用域  
全局变量不受局部变量的影响, 看例子  

    (define x 9)
    (define add 2 (lambda (x) (+ x 2)))
    x ;=> 9
    (add2 3) ;=> 5
    x ;=> 9

而`(set! x 20)`能对全局变量进行修改  
`scm`会选取词法上(?)最近的变量进行使用  

    (define counter 0)
    (define bump-counter
      (lambda ()
        (set! counter (+ counter 1))
        counter))
    (bump-counter) ;=> 1
    (bump-counter) ;=> 2

`let`可以新建局部变量, 遮盖全局变量, 注意写法  

    (define x 20)
    (let (
      (x 1)
      (y 2))
      (list x y)) ;=> (1 2)
    (let (
      (x 1)
      (y x))
      (list x y)) ;=> (1 20)

考虑到`let*`还有下面的写法  

    (let* ((x 1)
      (y x))
      (+ x y)) ;=> 2
    (let ((x 1))
      (let ((x y))
        (+ x y))) ;=> 2
    (let ((cons (lambda (x y) (+ x y))))
      (cons 1 2)) ;=> 3

`fluid-let`会临时修改全局变量值, 过后复原  
但是这个在`guile`里头也是不对劲的(?)  
而`let`本身面对这样的过程处理不同  

    (define x 0)
    (define x+
      (lambda ()
        (set! x (+ x 1))
        (display x)))
    (let ((x 20))
      (x+)) ;=> 1

@递归, 调用自身, 转化条件, 设定边界, 看例子  

    (define fractorial
      lambda (n)
        (if (= n) 1
          (* n (fractorial (- n 1)))))
    (define is-even?
      (lambda (n)
        (if (= n 0) #t
          (is-odd? (- n 1)))))
    (define is-odd?
      (lambda (n)
        (if (= n 0) #f
          (is-even? (- n 1)))))

其实`scm`已经内置`even? odd?`两个过程  
注意在局部变量实现`is-even? is-odd?`会有问题  
用`let`时`if`内部的`is-even? is-odd?`不能正确指向  
用`let*`时`if`内部的`is-odd`不能正确指向  
于是引入了新的过程`letrec`进行  

    (letrec ((local-even? (lambda (n)
      (if (= n 0) #t
        (local-odd? (- n 1)))))
      (local-odd? (lambda (n)
        (if (= n 0) #f
          (local-even? (- n 1))))))
      (list (local-even? 23) (local-odd? 23)))

`letrec`是专门为定义递归和相互递归的局部过程设计的  
借助`letrec`实现的循环过程  

    (letrec ((countdown (lambda (i)
      (if (= i 0) 'liftoff
        (begin
          (display i)
          (newline)
          (countdown (- i 1)))))))
      (countdown 10))

这一段可以用宏用`let`写更紧凑的结构  

    (let countdown ((i 10))
      (if (= i 0) 'liftoff
        (begin
          (display i)
          (newline)
          (countdown (- i 1)))))

`scm`中不存在递归以外其他循环和迭代的构造  
递归会被关照以免开销过大(?)  
前面的递归是尾部递归, 尾递归可以被优化, 因而安全  
例子, 尾递归实现, 从列表`l`取元素`o`位置, 否则返回`#f`  
例子, 递归倒转列表内容的顺序  

    (define reverse!
      (lambda (s)
        (let loop ((s s) (r '()))
          (if (null? s) r
            (let ((d (cdr s)))
              (set-cdr! s r)
              (display d)
              (display ", ")
              (display s)
              (newline)
              (loop d s))))))

几个小时才看明白, 还好环境里一般会提供这个函数的  
有一类迭代多次重复, 就是遍历列表每个元素  
于是有`map for-each`两种操作, 前者熟悉的  

    (define add2 (lambda (x)
      (+ x 2)))
    (map add2 '(1 2 3)) ;=> (3 4 5)
    (map cons '(1 2 3) '(10 20 30)) ;=> ((1 .10) (2 . 20) (3 30))
    (map + '(1 2 3) '(10 20 30)) ;=> ( 11 22 33)

而后者没有返回值, 是副作用  

    (for-each display '(1 2 3 4))

`scm`的读取端口的参数是可选的, 默认是`console`  
读取内容以字符, 行, `S 表达式`为单位  
以某种`EOF`结束, 才能用`eof-object?`判断结束  
函数有`read-char read-line read`, 后者读取`S 表达式`  
写入的端口也是可选的, 默认`console`  
写入的单位可以是字符, 或者基于`S 表达式`  
`write-char`写出的时候不会有`#\`前置, 保留机器方便的格式  
`display`的功能类似, 但会更方便人们阅读  
`open-input-file`过程接收文件名, 返回输出端口  

    (define i (open-input-file "hello.txt"))
    (read-char i) ;=> #\h
    (define j (read i))
    j ;=> ello

如果原文件不存在, `close-output-file`会建立新文件  
运行代码发现`output input file port`易混淆, 注意  

    (define o (open-output-file "g.txt"))
    (display "hello" o)
    (write-char #\space o)
    (display 'world o)
    (newline o)
    (close0output-port o)

`call-with-input-file`和`call-with-output-file`自动管理关闭  

    (call-with-input-file "hello.txt"
      (lambda (i)
        (let* ((a (read-char i))
          (b (read-char i))
          (c (read-char i)))
        (list a b c)))) ;=> (#\h #\e #\l)

未来操作字符串方便有`open-input-string open-output-string`  

    (define i (open-input-string "hello world"))
    (read-char i) ;=> #\h
    (read i) ;=> ello
    (read i) ;=> world
    (define o (open-output-string))
    (write 'hello o)
    (write-char #\, o)
    (display " " o)
    (display "world" o)
    (get-output-string o) ;=> "hello, world"

还有`load load-ralative`两个形式, 解说比较复杂(?)  

宏按说明应该像重用的代码块的缩写之类, 像链接用  

    (defint-macro when
      (lambda (test . branch)
        (list 'if test
          (cons 'begin branch)))))

用`define-macro`定义, 结果是  

    (when (< 1 2)
      (display "true 1\n")
      (display "true 2\n"))

相当于代入其中转化成为  

    (if (< 1 2)
      (begin
        (display "true 1\n")
        (display "true 2\n")))

同样有`unless`的使用, 甚至在其中使用已经定义的`when`  

    (define-macro unless
      (lambda (test . branch)
        (list 'if
          (list 'not test)
          (cons 'begin branch))))
    (define-macro unless
      (lambda (test . branch)
        (cons 'when
          (cons (list 'not test) branch))))

用反引号还可以简化语法, 大概`guile`对大小写敏感  
其中`,`表示插入相应的值, `,@`表示插入相应`S 表达式`  

    (defin-macro when
      (lambda (test . branch)
        `(if ,test
          (begin ,@branch))))

当用下面这种方式实现一个`or`形式  

    (define-macro my-or
      (lambda (x y)
        `(if ,x ,x ,y)))
    (my-or 1 2) ;=> 1
    (my-or #f 2) ;=> 2
    (my-or
      (begin
        (display "twice")
        (newline)
        #t)
      2)

上面因为求值过程, 会出现两次`display`, 改写  

    (define-macro my-or
      (lambda (x y)
        `(let ((temp ,x))
          (if temp temp ,y))))

再改写用来形成私有的调用, 甚至用`gensym`产生私有的符号  

    (define-macro my-or
      (lambda (x y)
        `(let ((+temp ,x))
          (if +temp +temp ,y))))
    (define-macro my-or
      (lambda (x y)
        (lat ((temp (gensym)))
          `(let ((,temp ,x))
            (if ,temp ,temp ,y)))))

`? 8.3 的 fluid-let 在 guile 没有, 正好跳过`  

后边`Mzscheme`和`guile`差别较大, 加上难度, 放弃了  
