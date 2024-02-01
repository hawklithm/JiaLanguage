# JiaLanguage
使用中文进行编程的语言，暂时就把它叫做“甲语言”吧。

它是一款解释执行的脚本语言，进行这个项目的原因是由于本来想设计一个DSL，用来在系统运行时动态下发脚本任务的效果，这些动态任务需要调度底层的模块，在各个模块之间进行数据的传输实现一套动态工作流。结果设计着设计着觉得要不把功能稍微扩展一点，做成一个语言吧！当然，如果从一种语言的角度来讲，甲语言还只是一个Toy，但是从DSL的角度来讲，用来实现对底层模块的调度是绰绰有余了，主打的就是一个胶水语言的效果。

当然肯定有人问，为啥不用groovy呀，为啥不用lua啊之类的问题，rust使用这些语言的执行引擎还是比较重的，而且本来胶水语言只是想达到一个组件之间的粘连效果，不需要那么完善的语法和那么重的执行引擎。

当然还会有人问，为啥用中文啊？因为我乐意啊，当然中文会存在一些utf-8或者gbk之类的编码问题，当然为了应用在不同的特定场景中，后续还是会计划支持等效的一个英文版语言。

语法的解析使用[pest.rs](https://github.com/pest-parser/pest)实现，执行引擎基于生成的AST语法树来完成，目前支持的功能：
 - [x] 赋值语句
 - [x] 循环语句
     - [x] foreach 
     - [x] for loop
     - [x] while
     - [x] 对数据的foreach遍历
 - [x] 条件判断语句
 - [x] 函数的声明和调用
 - [ ] 对native方法的调用
      - [x] 控制台输出
      - [ ] sleep
      - [ ] native方法注册检索
 - [x] 四则运算
 - [ ] 求余
 - [ ] 与、或、异或

支持的数据格式:
 - [x] 整形(64位)
 - [x] 布尔型
 - [x] 字符串
 - [x] 数组
 - [ ] 浮点数

暂时先按以上版本来进行开发吧，后续如果需要更多的功能再扩展。