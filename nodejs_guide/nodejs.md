Node.Js 入门以及中文翻译:  
http://nodebeginner.org/index.html  
http://nodebeginner.org/index-zh-cn.html  
CoffeeScript, 对应 "$ node s.js" 是 "$ coffee s.coffee"  
"$ npm install js2coffee", "$ js2coffee << EOF" 用来即时编译,  
https://github.com/rstacruz/js2coffee  
例子中的输出, 比如 coffee 测试部分, 用 # --> 引出输出内容.  

整个使用 listen 方法时不能在 createServer 后空格, 否则报错没用 listen method,  
为了习惯空格, 我把第一个左圆括号放到开头, createServer 返回的对象就有 listen 了,  
无参数执行函数 f() 语法不好, 我用(f 0)或(f '')等代替, 不注明.  

首先有个被 Node.js 执行的主文件, 保持各种功能的代码的模块分离和干净,  
这里只创建一个主文件 server.coffee , 用"$ coffee server.coffee"执行,  
运行之后访问 http://localhost:8888/  
内容如下:  

    http = require "http"
    (http.createServer (request, response) ->
        response.writeHead 200, "Content-Type": "text/plain"
        response.write "hello world!"
        response.end ''
    ).listen 8888

require... 一行请求 node 自带的 http 模块, 赋值给 http ,  
调用 http 模块的 createServer 方法返回一个对象, 含 listen 方法,  
之后用参数指定 8888 为监听端口, 这个服务器只打开端口, 不能处理请求.  

javascript 中像函数式编程一样, 可以将函数作为参数传递.  
上面的例子用到将括号及缩进部分作为参数传给 createServer.  
coffee 中使用匿名函数作为参数, 执行匿名函数:  

    excute1 = (f, v) -> f v
    excute1 ((word) -> console.log word),
      'hello' # --> hello
    excute2 = (v1, v2) -> console.log v1, v2
    excute2 'hello', 'world' # --> hello world
    ((w) -> console.log w) 'word' # --> word

下面将作为参数传递的函数取出写, 效果相同,  

    http = require "http"
    onRequest = (requset, response) ->
        response.writeHead 200, "Context-Type": "text/plain"
        response.write "hello world!"
        response.end ''
    (http.createServer onRequest).listen 8888

这样写因为 node 是"事件驱动"的, 当接收到请求, 才执行参数传入的函数.  
我们创建服务器, 并传递一个函数, 服务器收到一个请求, 函数就会被调用, 称为回调.  
下面一段代码进行测试, 实际中有时为了 favicon.ico 会多出现一次请求,  

    http = require "http"
    onRequest = (request, response) ->
        console.log "Request received."
        response.writeHead 200, "Content-Type": "text/plain"
        response.write "hello world"
        response.end ''
    (http.createServer onRequest).listen 8888
    console.log "Server has started"

运行之后先打印"started", 浏览器访问时打印"recieved".  
再看回调函数 onRequest 的参数, 他们包含处理 http 请求的细节,  
其中的 request 和 response 是 object, 可以用 cosole.log 打印查看.  

require 'http' 表示 node 自带了'http'模块被我们调用了,  
变量名与模块名相同是一个惯例, 但使用其他名字也是可以的.  
将自己写的模块导出不用多修改, 主要是结尾添加 exports.funcName = funcName .  
比如创建 index.coffee 使其调用 server.coffee 作为模块:  
# server.coffee  

    http = require "http"
    start = ->
        onRequest = (requset, response) ->
            response.writeHead 200, "Content-Type": "text/plain"
            response.write "hi"
            response.end ''
        (http.createServer onRequest).listen 8888
        console.log "started"
    exports.start = start

# index.coffee  

    server = require "./server"
    server.start ''

执行"$ coffee index.coffee", 服务器运行, 加载成功.  
通过这种办法将不同函数放不同文件, 通过模块组合到一起.  

为了针对不同 URL 返回不同的结果, 来处理"路由"模块.  
对于这样一个 URL,  http://localhost:8888/start?foo=bar&hello=world  

    url.parse(string).pathname == "start"
    url.parse(string).query == "foo=bar&hello=world"

借助 url 模块, pathname = (url.parse request.url).pathname .  
使用 route 来将 url 执行处理的程序, 三个文件如下:  
# router.coffee  

    route = (pathname) ->
        console.log "About to route a request for " + pathname
    exports.route = route

# server.coffee  

    http = require "http"
    url = require "url"
    start = (route) ->
        onRequest = (request, response) ->
            pathname = (url.parse request.url).pathname
            console.log "Request for #{pathname} received."
            route pathname
            response.writeHead 200, "Content-Type": "text/plain"
            response.write "hi"
            response.end ''
        (http.createServer onRequest).listen 8888
        console.log "Server has started"
    exports.start = start

# index.coffee  

    server = require "./server"
    router = require "./router"
    server.start router.route

接下来是写 request handler 根据不同的 url 调用不同函数,  
# requestHandlers.coffee  

    start = ->
        console.log "Request Handler 'start' was called."
    upload = ->
        console.log "Request Handoer 'upload' was called."
    exports.start = start
    exports.upload = upload

# router.coffee  

    route = (handle, pathname) ->
      console.log 'about to handle' + pathname
      if typeof handle[pathname] == 'function'
        handle[pathname] ''
      else
        console.log 'no function for ' + pathname
    exports.route = route

# server.coffee  

    url = require 'url'
    http = require 'http'
    start = (route, handle) ->
      (http.createServer (req, res) ->
        pathname = (url.parse req.url).pathname
        console.log "request for #{pathname} receives."
        route handle, pathname
        res.writeHead 200, 'Conten-Type': 'text/plain'
        res.write 'hello world'
        res.end ''
      ).listen 8888
      console.log 'server started'
    exports.start = start

# index.coffee  

    server = require "./server"
    router = require "./router"
    requestHandlers = require "./requestHandlers"
    handle =
        "/": requestHandlers.start
        "/start": requestHandlers.start
        "/upload": requestHandlers.upload
    server.start router.route, handle

接下来改变 res.write 的内容, 让信息在浏览器中现实区别,  
# requestHandlers  

    start = ->
      console.log 'requset for start'
      "Hello Start"
    upload = ->
      console.log 'request for upload'
      'Hello Upload'
    exports.start = start
    exports.upload = upload

# router.coffee  

    route = (handle, pathname) ->
      console.log 'about to handle ' + pathname
      if typeof handle[pathname] == 'function'
        handle[pathname] ''
      else
        console.log 'no function for ' + pathname
        '404 not found'
    exports.route = route

# server.coffee  

    url = require 'url'
    http = require 'http'
    start = (route, handle) ->
      (http.createServer (req, res) ->
        pathname = (url.parse req.url).pathname
        console.log "request for #{pathname} received."
        res.writeHead 200, 'Content-Type': 'text/plain'
        content = route handle, pathname
        res.write content
        res.end ''
      ).listen 8888
      console.log 'started'
    exports.start = start

# index.coffee (未改动)  

    server = require './server'
    router = require './router'
    requestHandlers = require './requestHandlers'
    handle = {}
    handle['/'] = requestHandlers.start
    handle['/start'] = requestHandlers.start
    handle['/upload'] = requestHandlers.upload
    server.start router.route, handle

修改 requestHandlers.coffee 文件, 添加 sleep() ,  
while 一行下的分号为了不执行任何动作,  

    start = ->
      console.log 'requset for start'
      sleep = (ms) ->
        startTime = new Date().getTime()
        while new Date().getTime() < startTime + ms
          ;
      sleep 10000
      console.log 'end sleep'
      "Hello Start"
    upload = ->
      console.log 'request for upload'
      'Hello Upload'
    exports.start = start
    exports.upload = upload

打开浏览器访问 /start , 会开始延迟, 期间访问 /upload 也延迟,  
Node.js 是单线程的, start 函数包含阻塞操作 sleep, 发生阻塞.  
Node 只有代码是顺序进行的, 通过事件轮询来进行并行操作,  
要执行非阻塞操作, 将参数传给其他函数进行回调.  
继续修改 requestHandlers.coffee 文件, 尝试非阻塞,  
先用 chile_proces 模块的 exec 执行一个 Shell 命令,  

    exec = (require 'child_process').exec
    start = ->
      console.log 'requset for start'
      content = 'empty'
      exec 'ls -lah', (error, stdout, stderr) ->
        content = stdout
        console.log 'end, ' + content
      content
    upload = ->
      console.log 'request for upload'
      'Hello Upload'
    exports.start = start
    exports.upload = upload

访问 /start 返回 empty, 说明程序并没有在执行 exec 时等待.  
