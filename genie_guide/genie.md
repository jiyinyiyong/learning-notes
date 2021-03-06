
不知道学 Genie 什么用场, 也许学后 C 阴影可以小些  
教程地址 https://live.gnome.org/Genie  
粗略的翻译可以看这里:  
http://www.minghao.hk/bbs/read.php?tid=433  
编写 Gtk 图形界面的教程这里:  
https://live.gnome.org/GtkGuiTutorial  
Genie 和 Vala 近亲, 更高级的编程形式, 内容基本类似  
两者可编译到 C, 因而性能与 C 相近  
使用了 GObject, 这是基于 C 的面向对象框架  
http://www.ibm.com/developerworks/cn/linux/l-gobject/  

Genie 的 (Hello world) 代码:  

    init
      print "Hello World"

程序仅两行, init 相当于 C 的 main 函数  
默认用缩进来分块, 或者可以用空格, 需要指定  

    init
      print "Hello World"

print 后面的参数都是双引号的字符串  
声明两个空格表示缩进, 默认 [indent=0]  
注意! 从网页上复制代码可能使缩进变空格  
注意! 源文件结尾最好有个空行, 以免可能的语法错误  
发现 (Sublime Text 2) 缩进容易出问题, 改回 Vim  
安装了 valac 足够版本可以用 vala 的编译器编译  
$ valac hello.gs # 文件名 hello.gs 得到可执行文件  
$ ./hello $ 用来执行程序  
$ valac -C hello.gs # 编译到 C 源代码  
看编译后的代码中类型有 gint, gboolean  
注意后缀 gs, 还有和 vala 共用相同编译器  
如果参数为多个文件, 编译器会自动处理依赖关系  
Genie 继承了 C 大量 warning, 只好看 error  

Genie 对大小写敏感  
没有类型标识符, 用类型推断替代, var 声明局部变量  
单行/ 多行的 var 均适用  

    [indent=2]
    init
      var i = 1
      var
        a = "happy"
        b = "sad"
        c = "anbivalent"
      for var I = 1 to 10
        print "looping"

也可以用类型赋值, 特别定义函数用到  

      def addon (x : int) : int
        x += 1
        return x
      init
        z : int = 3
        y : int = addon (z)
        print "%d", y

对象的实例化使用 new 关键字  

init  

    var o = new Object()

前置修饰符 (@) 可以借用保留字比如 @for , 后面再看  
注释与 C 相同, (//) 单行, (/*...*/) 多行  

支持的运算有:  
=, +, -, *, /, %  

++, --, 看编译结果前置后置都是可以  
+=, -=, *=, /=, %=  

逻辑运算符, 原文写的 "not (b)":  

>, <, >=, <=, (is not), is, and, or, not  

    init
      if (a > 2) and (b < 2)
        print "true"

位运算符沿用 C 的语法:  
|, ^, &, ~, |=, &=, ^=  
比特位移:  
<<=, >>=  
类型相关的:  
obj isa Class  
条件控制语句可以加上 do 使用单行, 同样适用于 while  

    init
      var a = 1
      if a>0
        print "greater than 0"
      else if a is 0
        print "is 0"
      else
        print "less than 0"
      if a > 0 do print "true"
      else do print "false"
      while a < 10 do a++
      for var i = 1 to 10 do print "i:%d", i

也有 (for s in args do print args) 的语法, 后面再看  
break 和 continue 的语法照常  
(case...when) 一旦遇到匹配就不再执行, 注意逗号可用  

    init
      var a = 3
      case a
        when 1,2
          print "1 or 2"
        when 3
          print "3"
        default
          print "else"

而且字符串作为判断条件也是可行的  

Genie 的数据类型分为值类型和引用类型  
值类型每次赋值时都对值进行复制(?)  
引用类型不复制, 新的标识符仅引用该对象  
值类型包括简单的数据类型, 还有复合的结构体  
简单数据类型比如以下:  

    char, uchar
    int, uint
    float, double
    bool (boolean)
    unichar (Unicode character)
    string

引用类型以类作为声明, 不必继承自 Glib 对象  
系统会一直追踪索引的数量来管理内存(?, 不懂)  
列表是引用类型, 就像字符串类型是 UTF-8 编码的字符串(?)  

Genie 要保证引用都指向真实对象, 因此不能随便用  null  
修饰符问号 (?) 放在类型之后, 运行 null 的出现, 避免错误  

    def fn_no_null (param : string) : string
      return param
    def fn_allow_nulls (param : string?) : string
      return null
    init
      var a = null
      // fn_no_null (a) // ... assertion `a != NULL' failed
      fn_allow_nulls (a)

这些检查在运行时执行, 调试完成可以个关掉, 但要看 valac 文档(?)  

通常 Genie 创建对象返回一个引用  
内存及对象会进行引用计数, 在无用时自动删除  
弱引用就是不计入到其中, 用关键字 weak 完成  

    init
      class Test
        o : Object
        def get_weak_ref () : weak Object
          o = new Object ()
          return o

相应调用该方法赋值时需要指定弱引用类型  
o : weak Object = get_weak_ref ()  
例子 o 的复杂来源于所有权的概念  
如果 o 不属于这个类, 那么方法返回的弱引用就不能被调用了  
假如返回值不是弱引用, 将传给调用代码, 但弱类型不能传递所有权  
弱类型像指针, 更容易结合使用, 但不需要常用  

(#) 作为修饰符用来转移所有权, 避免非引用的拷贝(?)  

    init
      var s = "world"
      t : string = # s
      print t
      print s

最终 s 变成了 null, 而 t 继承了 s 原先的所有权  

Genie 自动为创建实例返回索引, 结束时删除无用的索引  
通过指针可以人工创建和销毁实例, 更好地控制内存  
通常系统管理内存足够高效, 除了两种情况:  

    特意优化代码某个部分
    不支持引用计数和内存管理的外部库, 比如不基于 GObject

通过在类型声明加 (*) 后缀来创建实例获得索引  
!! 调试出错, 不行, 中止  


Genie 面向对象. 对象是能从其他类继承的类  
类的域(?)/ 属性/ 方法/ 事件一般公开. 或用下划线或修饰符声明私有  

    class Foo
      prop p1 : int
      def bar (name : string)
        print "hello %s p1 is %d", name, p1
      def bar2 () : string
        return "bar2 method was called"
    init
      var foo = new Object ()
      print foo.bar2 ()

上面的 foo 是 Object 的子类.  
不过 Genie 不支持多继承, 但不妨碍创建接口, 后面说  
类的定义里有几个可选的方法块  

    init 块会在每次实例被创建时执行, 无参数
    final 块在实例被销毁是执行, 无参数
    construct 快创建带参数的实例

一个类可以有多个创建方法, 从参数以及命名区别  

    init
      var foobar = new Foo (10)
      var foobar2 = new Foo.with_bar (10)
    class Foo : Object
      prop a : int
      init
        print "foo is initialized"
      final
        print "foo is being destroyed"
      construct (b : int)
        a = b
      construct (bar : int)
        a = bar

方法的定义用 def 开头, 后面跟可选的修饰符, 跟方法名称..  
还有参数以及参数的类型, 可能还跟上返回值的类型  

    init
      var foo = new Foo ()
      print foo.bar ("world")
    class Foo : Object
      def static bar (name : string) : string
        rueturn "hello " + name

方法的修饰符还可以用于属性及事件(?)  

    private 外界不可访问, 可用下划线 (_) 开头表示
    abstract 表示类中没有定义,而 在子类中定义, 有时很有用
    extern 方法不是在 Genie 中定义, 而在比如 C, (?)
    inline 让编译器在编译时使用内联强化性能
    virtual 表示可能在子类中将会进行覆盖
    override 覆盖超类对应位置, 超类没有对应时报错
    static 方法不随类的实例化而改变状态(?)

方法被调用时值和索引的参数变化  

    值, 被拷贝到新的局部变量
    索引, 只传递索引

默认行为可以通过 out 和 ref 修改  
out  

    调用者, 传递未实例化的参数
    被调用者, 接收未实例化的参数并实例化

ref  

    调用者, 传递实例化的参数
    被调用着, 接收实例化的参数

    init
      var
        a = 1
        b = 2
        c = 3
      var foo = new Foo ()
      foo.bar (a, out b, ref c)
      print "a=%d, b=%d, c=%d", a, b, c
    class Foo : Object
      def static bar (a : int, out b : int, ref c : int)
        a = 10
        b = 20
        c = 30

输出结果 "a=1, b=20, c=30"  
a 的实例化只是改变了拷贝后的局部变量  
b, c 的实例化传递了指向数据的索引, 改变了数据  
b, c 的区别在于实时函数的意图不同(?)  

Genie 有和 GObject 一致的属性, 多种方式定义  

    class Foo : Object
      prop name : string
      prop readonly count : int
      prop output : string
        get
          return "output"
        set
          _name = value

第一种方式自动创建了名为 _name 的私有属性  
第二种表示只读, 用 readonly 声明  
第三种用到 GObject (??)  
也可以假象这样一个类简单的使用方式  

    class Foo : Object
      prop name : string
    init
      var foo = new Foo ()
      foo.name = "world"
      print foo.name

事件, 亦信号是 GObject 内置功能, 所有源于 Object 的对象都具备  
事件被触发是代码块被触发执行, 但下面的代码有问题(?)  

    init
      var f = new Foo ()
      f.my_event += def (t, a)
        print "detected %d", a
      f.my_event (5)
    class Foo : Object
      event my_event (a: int)

代码中是闭包做事件处理器, 闭包是内联到代码中的代码块  
替代方案是为事件处理器编写回调函数  
两个参数, 第一个传给事件处理器, 第二个给函数输出  
像调用方法一样触发函数, 注意, Genie 目前支持不良(?)  

Genie 中一个类可以实现任意个接口(Interface)  
接口很像类, 但不能实例化, 因此定义类的实例可以是接口的实例  
类只有定义了却不实现所有方法, 它才是实用的(?)  

    interface ITest
      prop abstract data : int
      def abstract fn ()

上面的接口不会实现 data 属性, 而是要求其实例将其实现  
同样, fn 方法也被要求其实例将其实现  

    init
      var f = new Foo ()
      f.fn ()
    interface ITest
      prop abstract data : int
      def abstract fn ()
    class Foo : Object implements ITest
      prop data : int
      def fn ()
        print "fn"

接口也可以从接口继承, 定义时就像类的继承  
interface List : Collection  
但这样不能在不实现 Collection 的情况下在类这实现 List (?)  
因此用下面的写法强制实现 List 这个类(?)  
class ListCLass : Object implements Collection, List // 报错的  

isa 用来判断类型(似乎版本上有问题, 出错)(?)  

    init
      var o = new list of string
      if o isa list of Object
        print "a list of string"

http://stackoverflow.com/questions/3412533/valac-says-undef-ref-but-libgee-exists  

Gnome 官网看到 1/4 跳跃太多, 尝试看 Putty 作者的教程  
http://bkhome.org/genie/  
教程最后更新于 09 年 5 月, 作者用 Bacon 去了  
Vala 出现于 mid-2006, Genie 出现于 mid-2008  
Vala 构思来源于 Jürg Billeter, Genie 没查到  
据说是 Gnome 设计的, 为 C 带去高级语言的特性  
两者相同的编译器和特性, 语法迥异, Genie 的文档稀少  
