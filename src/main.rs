extern crate pest_derive;
use std::{collections::HashMap, fmt::Display, rc::Rc};

use pest::{
    error::{Error, ErrorVariant},
    Parser,
};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "cpl.pest"]
struct MyParser;

fn main() {
    let test_data = "令 乾 为 10;
重复(乾 大于 0)
始
乾 为 乾-1;
终;
令 乾 为 真;
若 乾 则
始
甲 为 60;
乙 为 60;
终
否则
乙 为 40;
函数 天晴()
始
输出(\"太阳升起来了\");
终;
函数 下雨()
始
输出(\"下雨了\");
终;
天晴();
下雨();
输出(甲+\"难于上青天\");
令 甲 为 甲+\"难于上青天\";
若 乾 则 甲 为 60;/*1243124124*/
令 坤 为 20;
若 乙 大于 坤 则 甲 为 4;
令 甲 为 \"乾坤\";
令 乙 为 \"李四\";
令 乾 为 10000;
令 坤 为 乙;
若 乾 大于 1000 则
始
甲 为 60;
乙 为 60;
终
否则
始
乙 为 40;
甲 为 20;
终;

重复(次数 3)
始
abc 为 1;
终;


由 甲 至 乙 各 item
始
a 为 2;
终;

令 乾 为 [\"张三\",\"李四\"];
函数 唱(变量)
始
  输出(变量+\",难于上青天\\n\");
终;

[\"张三\",\"李四\"] 各 人员 唱(\"能不能给我一首歌的时间\");

输出(\"\\n\");";
    match parse_input(test_data) {
        Ok(ast) => {
            println!("{:#?}", ast);
            let mut w = Workspace::new();
            if let Err(e) = w.execute(&ASTNode::Root(ast)) {
                println!("error happen, message: {}", e.msg);
            }
        }
        Err(e) => eprintln!("{}", e),
    }
}

fn build_single_normal_expr(pair: pest::iterators::Pair<Rule>) -> Result<ASTNode, Error<Rule>> {
    Ok(match pair.as_rule() {
        Rule::create_variable_expr => build_create_variable_expr(pair),
        Rule::assignment_expr => build_assignment_expr(pair),
        Rule::if_statement => build_if_statement(pair),
        Rule::loop_statement => build_loop_statement(pair),
        Rule::for_statement => build_for_statement(pair),
        Rule::array_loop => build_array_loop(pair),
        Rule::function_expr => build_function_expr(pair),
        Rule::function_declare_statement => build_function_declare_statement(pair),
        Rule::EOI => Ok(ASTNode::NoOp),
        Rule::COMMENT => Ok(ASTNode::NoOp),
        Rule::WHITESPACE => Ok(ASTNode::NoOp),
        r => Err(Error::new_from_span(
            ErrorVariant::CustomError {
                message: format!("unsupport syntax, {:?} is unexpect", r).to_string(),
            },
            pair.as_span(),
        )),
    }?)
}

fn parse_input(input: &str) -> Result<Vec<ASTNode>, Error<Rule>> {
    let mut asts = Vec::new();

    let pairs = MyParser::parse(Rule::chinese, input)?;

    for pair in pairs {
        println!("run for = {:?}", pair);
        let r = build_single_normal_expr(pair)?;
        if ASTNode::NoOp.eq(&r) {
            continue;
        }
        println!("result = {:?}", r);
        asts.push(r);
    }
    Ok(asts)
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Operator {
    Plus,
    Minus,
    Multi,
    Divide,
    And,
    Or,
}

impl Operator {
    pub fn operate(&self, first: &Object, second: &Object) -> Result<Object, ExecuteError> {
        if Object::VoidObject.eq(&first) || Object::VoidObject.eq(&second) {
            return Err(ExecuteError {
                msg: "no operator for Void data".to_string(),
            });
        }
        if Object::NullObject.eq(&first) || Object::NullObject.eq(&second) {
            return Err(ExecuteError {
                msg: "null point exception".to_string(),
            });
        }
        if first != second
            && (matches!(first, Object::BooleanObject(_))
                || matches!(second, Object::BooleanObject(_)))
        {
            return Err(ExecuteError {
                msg: format!("no operator for {:?} and {:?}", first, second),
            });
        }
        match self {
            Operator::Plus => {
                if matches!(first, Object::BooleanObject(_))
                    || matches!(second, Object::BooleanObject(_))
                {
                    return Err(ExecuteError {
                        msg: "boolean can not be plused".to_string(),
                    });
                }
                if matches!(first, Object::StringObject(_))
                    || matches!(second, Object::StringObject(_))
                {
                    return Ok(Object::StringObject(
                        first.to_string() + &second.to_string(),
                    ));
                }
                if let (Object::IntegerObject(f), Object::IntegerObject(s)) = (first, second) {
                    return Ok(Object::IntegerObject(f + s));
                }
                return Err(ExecuteError {
                    msg: format!(
                        "no PLUS operator can be used for {:?} and {:?}",
                        first, second
                    ),
                });
            }
            Operator::Minus => {
                if let (Object::IntegerObject(f), Object::IntegerObject(s)) = (first, second) {
                    return Ok(Object::IntegerObject(f - s));
                }
                return Err(ExecuteError {
                    msg: format!(
                        "no MINUS operator can be used for {:?} and {:?}",
                        first, second
                    ),
                });
            }
            Operator::Multi => {
                if let (Object::IntegerObject(f), Object::IntegerObject(s)) = (first, second) {
                    return Ok(Object::IntegerObject(f * s));
                }
                return Err(ExecuteError {
                    msg: format!(
                        "no MULTI operator can be used for {:?} and {:?}",
                        first, second
                    ),
                });
            }
            Operator::Divide => {
                if let (Object::IntegerObject(f), Object::IntegerObject(s)) = (first, second) {
                    return Ok(Object::IntegerObject(f / s));
                }
                return Err(ExecuteError {
                    msg: format!(
                        "no DIVIDE operator can be used for {:?} and {:?}",
                        first, second
                    ),
                });
            }
            Operator::And => {
                if let (Object::BooleanObject(f), Object::BooleanObject(s)) = (first, second) {
                    return Ok(Object::BooleanObject(*f && *s));
                }
                return Err(ExecuteError {
                    msg: format!(
                        "no AND operator can be used for {:?} and {:?}",
                        first, second
                    ),
                });
            }
            Operator::Or => {
                if let (Object::BooleanObject(f), Object::BooleanObject(s)) = (first, second) {
                    return Ok(Object::BooleanObject(*f || *s));
                }
                return Err(ExecuteError {
                    msg: format!(
                        "no OR operator can be used for {:?} and {:?}",
                        first, second
                    ),
                });
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum CompOperator {
    Bigger,
    Less,
    Equal,
}

impl CompOperator {
    fn compare(&self, l: &Object, r: &Object) -> Result<bool, ExecuteError> {
        Ok(
            if let (Object::IntegerObject(left), Object::IntegerObject(right)) = (l, r) {
                match self {
                    CompOperator::Bigger => left > right,
                    CompOperator::Less => left < right,
                    CompOperator::Equal => left == right,
                }
            } else {
                match self {
                    CompOperator::Equal => match (l, r) {
                        (Object::StringObject(s1), Object::StringObject(s2)) => s1.eq(s2),
                        (Object::BooleanObject(b1), Object::BooleanObject(b2)) => b1 == b2,
                        (Object::ArrayObject(a1), Object::ArrayObject(a2)) => {
                            let l1 = a1.len();
                            let l2 = a2.len();
                            let mut answer = true;
                            if l1 == l2 {
                                for idx in 0..l1 {
                                    if let Err(_e) = CompOperator::Equal.compare(&a1[idx], &a2[idx])
                                    {
                                        answer = false;
                                        break;
                                    }
                                }
                            } else {
                                answer = false;
                            }
                            answer
                        }
                        (Object::NullObject, Object::NullObject) => true,
                        (_, Object::NullObject) => false,
                        (Object::NullObject, _) => false,
                        _ => false,
                    },
                    _ => Err(ExecuteError {
                        msg: format!("compare operator is illegal! {:?} {:?} {:?}", l, self, r),
                    })?,
                }
            },
        )
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
enum Object {
    StringObject(String),
    IntegerObject(i64),
    BooleanObject(bool),
    ArrayObject(Vec<Object>),
    NullObject,
    VoidObject,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.to_string())
    }
}

impl Object {
    pub fn to_string(&self) -> String {
        match self {
            Object::StringObject(str_data) => str_data.to_string(),
            Object::IntegerObject(int_data) => int_data.to_string(),
            Object::BooleanObject(bool_data) => {
                if *bool_data { "true" } else { "false" }.to_string()
            }
            Object::ArrayObject(array_data) => {
                let mut builder = String::from("[");
                let mut first = true;
                for item in array_data {
                    if first {
                        builder = builder + item.to_string().as_str();
                        first = false;
                    } else {
                        builder = builder + "," + item.to_string().as_str();
                    }
                }
                builder = builder + "]";
                builder
            }
            Object::NullObject => "null".to_string(),
            Object::VoidObject => "void".to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum ASTNode {
    // Define your AST nodes here based on the grammar
    Variable(String),
    AssingmentExpr {
        left: Rc<ASTNode>,
        right: Rc<ASTNode>,
    },
    StringConst(String),
    Number(i64),
    Boolean(bool),
    Op(Operator),
    Other,
    ComputeExpr {
        first: Rc<ASTNode>,
        op: Rc<ASTNode>,
        second: Rc<ASTNode>,
    },
    FunctionCall {
        name: String,
        params: Vec<Rc<ASTNode>>,
    },
    /**
     * 解析的时候不会生成，用于对funtionDeclare的执行逻辑的存储
     * 便于与FunctionDeclare区分
     */
    FunctionExecute {
        name: String,
        params: Vec<String>,
        body: Vec<Rc<ASTNode>>,
    },
    FunctionDeclare {
        name: String,
        params: Vec<String>,
        body: Vec<Rc<ASTNode>>,
    },
    Not(Rc<ASTNode>),
    Array(Vec<Rc<ASTNode>>),
    AssignmentExpr {
        left: Rc<ASTNode>,
        right: Rc<ASTNode>,
    },
    IfStatement {
        condition: Rc<ASTNode>,
        process: Vec<Rc<ASTNode>>,
        else_process: Option<Vec<Rc<ASTNode>>>,
    },
    LoopStatement {
        condition: Rc<ASTNode>,
        process: Vec<Rc<ASTNode>>,
    },
    ForStatement {
        range_left: Rc<ASTNode>,
        range_right: Rc<ASTNode>,
        iterator_variable: Rc<ASTNode>,
        process: Vec<Rc<ASTNode>>,
    },
    ArrayLoop {
        array: Rc<ASTNode>,
        variable: Rc<ASTNode>,
        process: Vec<Rc<ASTNode>>,
    },
    CompOp(CompOperator),
    BinaryCompare {
        first: Rc<ASTNode>,
        op: Rc<ASTNode>,
        second: Rc<ASTNode>,
    },
    LoopTimeCheck(Rc<ASTNode>),
    NoOp,
    Root(Vec<ASTNode>),
    /**
     * 用于执行需要超出脚本外的逻辑，比如输出打印之类的
     * 这个枚举中的字符串为底层实际执行逻辑的编码
     */
    Native(String, Vec<String>),
}

struct RuntimeContext {
    operation_params: HashMap<String, Object>,
    global_context: Rc<HashMap<String, Object>>,
    global_method: Rc<HashMap<String, ASTNode>>,
    method_stack: HashMap<String, ASTNode>,
}

impl RuntimeContext {
    // pub fn new() -> RuntimeContext {
    //     RuntimeContext {
    //         operation_params: HashMap::new(),
    //         global_context: Rc::new(HashMap::new()),
    //         method_stack: HashMap::new(),
    //         global_method: Rc::new(HashMap::new()),
    //     }
    // }
    pub fn with_global_context(
        global_context: Rc<HashMap<String, Object>>,
        global_method: Rc<HashMap<String, ASTNode>>,
    ) -> RuntimeContext {
        RuntimeContext {
            operation_params: HashMap::new(),
            global_context,
            method_stack: HashMap::new(),
            global_method,
        }
    }
}

struct ExecuteError {
    msg: String,
}
struct Workspace {
    runtime_context: RuntimeContext,
}

impl ASTNode {
    pub fn get_value(&self, context: &RuntimeContext) -> Option<Object> {
        match self {
            ASTNode::Variable(variable_name) => context
                .operation_params
                .get(variable_name)
                .or(context.global_context.get(variable_name))
                .cloned(),
            ASTNode::StringConst(s) => Some(if s.len() > 2 {
                Object::StringObject(s.get(1..s.len() - 1)?.to_string())
            } else {
                Object::StringObject(String::new())
            }),
            ASTNode::Number(n) => Some(Object::IntegerObject(*n)),
            ASTNode::Boolean(b) => Some(Object::BooleanObject(*b)),
            ASTNode::Array(array) => {
                let mut r = Vec::new();

                for item in array {
                    r.push(item.get_value(context)?);
                }
                Some(Object::ArrayObject(r))
            }
            _ => None,
        }
    }
}
impl Workspace {
    pub fn new() -> Workspace {
        let mut global_method = HashMap::new();
        global_method.insert(
            String::from("输出"),
            ASTNode::FunctionExecute {
                name: String::from("输出"),
                params: vec![String::from("data")],
                body: vec![Rc::new(ASTNode::Native(
                    String::from("print"),
                    vec![String::from("data")],
                ))],
            },
        );
        Workspace {
            runtime_context: RuntimeContext::with_global_context(
                Rc::new(HashMap::new()),
                Rc::new(global_method),
            ),
        }
    }
    // pub fn with_context(runtime_context: &RuntimeContext) -> Workspace {
    //     Workspace {
    //         runtime_context: RuntimeContext::with_global_context(
    //             runtime_context.global_context.clone(),
    //             runtime_context.global_method.clone(),
    //         ),
    //     }
    // }

    pub fn with_new_stack(
        runtime_context: &RuntimeContext,
        operation_param_stack: HashMap<String, Object>,
    ) -> Workspace {
        Workspace {
            runtime_context: RuntimeContext {
                operation_params: operation_param_stack,
                global_context: runtime_context.global_context.clone(),
                method_stack: HashMap::new(),
                global_method: runtime_context.global_method.clone(),
            },
        }
    }

    pub fn execute(&mut self, node: &ASTNode) -> Result<Object, ExecuteError> {
        println!("current = {:?}", node);
        println!("param map = {:?}", self.runtime_context.operation_params);
        match node {
            ASTNode::AssingmentExpr { left, right } => {
                let right_value = self.execute(right)?;
                let variable = match &**left {
                    ASTNode::Variable(variable_name) => Ok(variable_name),
                    _ => Err(ExecuteError {
                        msg: "variable is missing".to_string(),
                    }),
                }?;
                self.runtime_context
                    .operation_params
                    .insert(variable.clone(), right_value);
                Ok(Object::VoidObject)
            }
            ASTNode::ComputeExpr { first, op, second } => Ok(match **op {
                ASTNode::Op(op) => op.operate(&self.execute(first)?, &self.execute(&second)?)?,
                _ => Err(ExecuteError {
                    msg: "operator is missing".to_string(),
                })?,
            }),
            ASTNode::FunctionCall { name, params } => {
                let mut param_objs = Vec::new();
                for param in params {
                    let obj = self.execute(&**param)?;
                    param_objs.push(obj);
                }
                let method = self
                    .runtime_context
                    .method_stack
                    .get(name.as_str())
                    .or(self.runtime_context.global_method.get(name.as_str()))
                    .ok_or(ExecuteError {
                        msg: format!("no method named {} found", name),
                    })?;
                let next_param_context = match method {
                    ASTNode::FunctionExecute {
                        name,
                        params,
                        body: _,
                    } => {
                        let mut param_mapping = HashMap::new();
                        let param_len = params.len();
                        for i in (0..param_len).rev() {
                            if let (Some(param_name), Some(param_data)) =
                                (params.get(i), param_objs.get(i))
                            {
                                param_mapping.insert(param_name.clone(), param_data.clone());
                            } else {
                                Err(ExecuteError {
                                    msg: format!("function {} need {} parameters, but {} parameters provided",name,param_len,param_objs.len())
                                })?;
                            }
                        }
                        param_mapping
                    }
                    _ => Err(ExecuteError {
                        msg: "function has illegal format".to_string(),
                    })?,
                };
                let mut w = Workspace::with_new_stack(&self.runtime_context, next_param_context);
                return w.execute(method);
            }
            ASTNode::FunctionDeclare { name, params, body } => {
                self.runtime_context.method_stack.insert(
                    name.to_string(),
                    ASTNode::FunctionExecute {
                        name: name.clone(),
                        params: params.clone(),
                        body: body.clone(),
                    },
                );
                Ok(Object::VoidObject)
            }
            ASTNode::FunctionExecute {
                name: _,
                params: _,
                body,
            } => {
                //由于在调用FuntionCall的时候已经将入参压栈，所以这里不需要对params进行处理
                let len = body.len();
                for i in 0..len - 1 {
                    self.execute(&*body[i])?;
                }
                let ret = self.execute(&*body[len - 1])?;
                Ok(ret)
            }
            ASTNode::Not(value) => {
                let inner = value.get_value(&self.runtime_context).ok_or(ExecuteError {
                    msg: format!(
                        "NOT operation's content value calculate error, value = {:?} ",
                        value
                    ),
                })?;
                match inner {
                    Object::BooleanObject(b) => Ok(Object::BooleanObject(!b)),
                    _ => Err(ExecuteError {
                        msg: format!(
                            "inner value can not be used for NOT operation, value = {:?} ",
                            value
                        ),
                    }),
                }
            }
            ASTNode::AssignmentExpr { left, right } => {
                //赋值语句本质上是将变量放到参数栈中
                let variable_name = match &**left {
                    ASTNode::Variable(variable_name) => variable_name.clone(),
                    _ => Err(ExecuteError {
                        msg: format!(
                            "variable declarement or assignment has syntax error, value = {:?} ",
                            left
                        ),
                    })?,
                };
                let value_obj = self.execute(right)?;
                self.runtime_context
                    .operation_params
                    .insert(variable_name, value_obj);
                Ok(Object::VoidObject)
            }
            ASTNode::IfStatement {
                condition,
                process,
                else_process,
            } => {
                let condition_obj = self.execute(&condition)?;
                let condition_value = match condition_obj {
                    Object::BooleanObject(b) => b,
                    _ => Err(ExecuteError {
                        msg: format!(
                            "condition of IfStatement has syntax error, value = {:?} ",
                            condition_obj
                        ),
                    })?,
                };
                if condition_value {
                    let len = process.len();
                    for i in 0..len - 1 {
                        self.execute(&*process[i])?;
                    }
                    let ret = self.execute(&*process[len - 1])?;
                    Ok(ret)
                } else if let Some(else_process) = else_process {
                    let len = else_process.len();
                    for i in 0..len - 1 {
                        self.execute(&*else_process[i])?;
                    }
                    let ret = self.execute(&*else_process[len - 1])?;
                    Ok(ret)
                } else {
                    Ok(Object::VoidObject)
                }
            }
            ASTNode::LoopStatement { condition, process } => match &**condition {
                ASTNode::LoopTimeCheck(check_time) => {
                    let time_count = self.execute(&check_time)?;
                    let time_value = match time_count {
                        Object::IntegerObject(value) => value,
                        _ => Err(ExecuteError {
                            msg: format!(
                                "loop time checker must be integer value, value = {:?} ",
                                time_count
                            ),
                        })?,
                    };
                    for _ in 0..time_value {
                        for item in process {
                            self.execute(item)?;
                        }
                    }
                    Ok(Object::VoidObject)
                }
                other => {
                    loop {
                        let condition = self.execute(other)?;
                        let condition_value = match condition {
                            Object::BooleanObject(b) => b,
                            _ => Err(ExecuteError {
                                msg: format!(
                                    "condition must be boolean value, value = {:?} ",
                                    condition
                                ),
                            })?,
                        };
                        if !condition_value {
                            break;
                        }
                        for item in process {
                            self.execute(item)?;
                        }
                    }
                    Ok(Object::VoidObject)
                }
            },
            ASTNode::ForStatement {
                range_left,
                range_right,
                iterator_variable,
                process,
            } => {
                let left = self.execute(&range_left)?;
                let right = self.execute(&range_right)?;
                let iterator_variable_name = self.execute(&iterator_variable)?;
                if let (
                    Object::IntegerObject(left_i),
                    Object::IntegerObject(right_i),
                    Object::StringObject(variable),
                ) = (left, right, iterator_variable_name)
                {
                    for i in left_i..right_i {
                        self.runtime_context
                            .operation_params
                            .insert(variable.clone(), Object::IntegerObject(i));
                        for sub in process {
                            self.execute(&sub)?;
                        }
                    }
                }
                Ok(Object::VoidObject)
            }
            ASTNode::ArrayLoop {
                array,
                variable,
                process,
            } => {
                let array_data = self.execute(&array)?;
                let array = match array_data {
                    Object::ArrayObject(data) => data,
                    _ => Err(ExecuteError {
                        msg: format!("should be array, value = {:?} ", array_data),
                    })?,
                };
                let variable_name = match &**variable {
                    ASTNode::Variable(name) => name,
                    _ => Err(ExecuteError {
                        msg: format!("should be variable name, value = {:?} ", variable),
                    })?,
                };
                for item in array {
                    self.runtime_context
                        .operation_params
                        .insert(variable_name.clone(), item.clone());
                    for sub_process in process {
                        self.execute(&sub_process)?;
                    }
                }
                Ok(Object::VoidObject)
            }
            ASTNode::BinaryCompare { first, op, second } => {
                let first_obj = self.execute(&first)?;
                let second_obj = self.execute(&second)?;
                let op = match &**op {
                    ASTNode::CompOp(op) => op,
                    _ => Err(ExecuteError {
                        msg: format!("compare operator syntax error! value = {:?} ", op),
                    })?,
                };
                Ok(Object::BooleanObject(op.compare(&first_obj, &second_obj)?))
            }
            ASTNode::Root(asts) => {
                for ast in asts {
                    self.execute(ast)?;
                }
                Ok(Object::VoidObject)
            }
            ASTNode::Variable(_)
            | ASTNode::StringConst(_)
            | ASTNode::Number(_)
            | ASTNode::Boolean(_)
            | ASTNode::Array(_) => {
                if let Some(t) = node.get_value(&self.runtime_context) {
                    Ok(t)
                } else {
                    Ok(Object::NullObject)
                }
            }
            ASTNode::Native(code, params) => {
                match code.as_str() {
                    "print" => {
                        for item in params {
                            print!(
                                "{}",
                                self.runtime_context
                                    .operation_params
                                    .get(item)
                                    .ok_or(ExecuteError {
                                        msg: format!(
                                            "parameter for native method [{}] is missing [{}] ",
                                            code, item
                                        ),
                                    })?
                                    .to_string()
                                    .replace("\\n", "\n")
                            );
                        }
                    }
                    other => Err(ExecuteError {
                        msg: format!("native method is missing {} ", other),
                    })?,
                }
                Ok(Object::VoidObject)
            }
            other => Err(ExecuteError {
                msg: format!("can not execute for {:?} ", other),
            })?,
        }
    }
}

fn build_bin_compare_expr(pair: pest::iterators::Pair<Rule>) -> Result<ASTNode, Error<Rule>> {
    let span = pair.as_span();
    let mut items = pair.into_inner();
    if let (Some(first), Some(op), Some(second)) = (items.next(), items.next(), items.next()) {
        let first_node = build_right_element(first)?;
        let second_node = build_right_element(second)?;
        let op_node = match op.as_str() {
            "大于" => ASTNode::CompOp(CompOperator::Bigger),
            "小于" => ASTNode::CompOp(CompOperator::Less),
            "等于" => ASTNode::CompOp(CompOperator::Equal),
            _ => ASTNode::Other,
        };
        if ASTNode::Other.eq(&op_node) {
            return Err(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: "binary compare expr has syntax error".to_string(),
                },
                span,
            ));
        }
        Ok(ASTNode::BinaryCompare {
            first: Rc::new(first_node),
            op: Rc::new(op_node),
            second: Rc::new(second_node),
        })
    } else {
        return Err(Error::new_from_span(
            ErrorVariant::CustomError {
                message: "binary compare expr has syntax error".to_string(),
            },
            span,
        ));
    }
}

fn build_compare_element(pair: pest::iterators::Pair<Rule>) -> Result<ASTNode, Error<Rule>> {
    let span = pair.as_span();
    let t = match pair.as_rule() {
        Rule::bin_compare_expr => build_bin_compare_expr(pair)?,
        Rule::function_expr => build_function_expr(pair)?,
        Rule::variable => build_variable(pair)?,
        Rule::boolean_expr => build_boolean_expr(pair)?,
        _ => ASTNode::Other,
    };
    if ASTNode::Other.eq(&t) {
        return Err(Error::new_from_span(
            ErrorVariant::CustomError {
                message: "compare element has syntax error".to_string(),
            },
            span,
        ));
    }
    Ok(t)
}

fn build_loop_condtion(pair: pest::iterators::Pair<Rule>) -> Result<ASTNode, Error<Rule>> {
    println!("loop condition parse {:?}", pair);
    let span = pair.as_span();
    let t = match pair.as_rule() {
        Rule::compute_expr => build_compute_expr(pair)?,
        Rule::function_expr => build_function_expr(pair)?,
        Rule::variable => build_variable(pair)?,
        Rule::number => {
            if let Ok(tmp_int) = pair.as_str().parse::<i64>() {
                ASTNode::Number(tmp_int)
            } else {
                ASTNode::Other
            }
        }
        _ => ASTNode::Other,
    };
    if ASTNode::Other.eq(&t) {
        return Err(Error::new_from_span(
            ErrorVariant::CustomError {
                message: "loop condition has syntax error".to_string(),
            },
            span,
        ));
    }
    Ok(t)
}

fn build_time_check_expr(pair: pest::iterators::Pair<Rule>) -> Result<ASTNode, Error<Rule>> {
    println!("time check expr = {:?}", pair);
    let span = pair.as_span();
    let mut item = pair.into_inner();
    if let Some(tmp) = item.next() {
        return Ok(ASTNode::LoopTimeCheck(Rc::new(build_loop_condtion(tmp)?)));
    }
    return Err(Error::new_from_span(
        ErrorVariant::CustomError {
            message: "time check expr has syntax error".to_string(),
        },
        span,
    ));
}

fn build_multi_expr(pair: pest::iterators::Pair<Rule>) -> Result<Vec<Rc<ASTNode>>, Error<Rule>> {
    Ok(match pair.as_rule() {
        Rule::batch_expr => {
            let mut multi: Vec<Rc<ASTNode>> = Vec::new();
            for single in pair.into_inner() {
                multi.push(Rc::new(build_single_normal_expr(single)?));
            }
            multi
        }
        _ => vec![Rc::new(build_single_normal_expr(pair)?)],
    })
}

fn build_array_loop(pair: pest::iterators::Pair<Rule>) -> Result<ASTNode, Error<Rule>> {
    let span = pair.as_span();
    let mut items = pair.into_inner();
    if let (Some(array), Some(item), Some(expr)) = (items.next(), items.next(), items.next()) {
        let array_node = build_array_expr(array)?;
        let item_node = match item.as_rule() {
            Rule::variable => build_variable(item),
            _ => Err(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: "for statement has syntax error".to_string(),
                },
                span,
            )),
        }?;

        let process_node = build_multi_expr(expr)?;
        return Ok(ASTNode::ArrayLoop {
            array: Rc::new(array_node),
            variable: Rc::new(item_node),
            process: process_node,
        });
    };
    return Err(Error::new_from_span(
        ErrorVariant::CustomError {
            message: "array loop has syntax error".to_string(),
        },
        span,
    ));
}
fn build_for_statement(pair: pest::iterators::Pair<Rule>) -> Result<ASTNode, Error<Rule>> {
    let span = pair.as_span();
    let mut items = pair.into_inner();
    if let (Some(range_left), Some(range_right), Some(variable)) =
        (items.next(), items.next(), items.next())
    {
        let left_node = build_loop_condtion(range_left)?;
        let right_node = build_loop_condtion(range_right)?;
        let temp_variable = {
            let span = variable.as_span();
            match variable.as_rule() {
                Rule::variable => build_variable(variable)?,
                _ => Err(Error::new_from_span(
                    ErrorVariant::CustomError {
                        message: "for statement has syntax error".to_string(),
                    },
                    span,
                ))?,
            }
        };

        if let Some(expr) = items.next() {
            let process_node = build_multi_expr(expr)?;
            return Ok(ASTNode::ForStatement {
                range_left: Rc::new(left_node),
                range_right: Rc::new(right_node),
                iterator_variable: Rc::new(temp_variable),
                process: process_node,
            });
        }
    };
    return Err(Error::new_from_span(
        ErrorVariant::CustomError {
            message: "for statement has syntax error".to_string(),
        },
        span,
    ));
}

fn build_loop_statement(pair: pest::iterators::Pair<Rule>) -> Result<ASTNode, Error<Rule>> {
    let span = pair.as_span();
    let mut items = pair.into_inner();
    if let Some(condition) = items.next() {
        let condition_node = match condition.as_rule() {
            Rule::time_check_expr => build_time_check_expr(condition)?,
            Rule::bin_compare_expr => build_compare_element(condition)?,
            _ => ASTNode::Other,
        };
        if ASTNode::Other.eq(&condition_node) {
            return Err(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: "loop statement has syntax error".to_string(),
                },
                span,
            ));
        }
        if let Some(expr) = items.next() {
            return Ok(ASTNode::LoopStatement {
                condition: Rc::new(condition_node),
                process: build_multi_expr(expr)?,
            });
        }
    }
    return Err(Error::new_from_span(
        ErrorVariant::CustomError {
            message: "loop statement has syntax error".to_string(),
        },
        span,
    ));
}

fn build_if_statement(pair: pest::iterators::Pair<Rule>) -> Result<ASTNode, Error<Rule>> {
    let span = pair.as_span();
    let mut items = pair.into_inner();
    if let (Some(condition), Some(body)) = (items.next(), items.next()) {
        let condition_node = build_compare_element(condition)?;
        if let Some(else_node) = items.next() {
            Ok(ASTNode::IfStatement {
                condition: Rc::new(condition_node),
                process: build_multi_expr(body)?,
                else_process: Some(build_multi_expr(else_node)?),
            })
        } else {
            Ok(ASTNode::IfStatement {
                condition: Rc::new(condition_node),
                process: build_multi_expr(body)?,
                else_process: None,
            })
        }
    } else {
        return Err(Error::new_from_span(
            ErrorVariant::CustomError {
                message: "if statement has syntax error".to_string(),
            },
            span,
        ));
    }
}

fn build_right_element(right_params: pest::iterators::Pair<Rule>) -> Result<ASTNode, Error<Rule>> {
    Ok(match right_params.as_rule() {
        Rule::compute_expr => build_compute_expr(right_params),
        Rule::function_expr => build_function_expr(right_params),
        Rule::string => Ok(ASTNode::StringConst(String::from(right_params.as_str()))),
        Rule::number => Ok(ASTNode::Number(right_params.as_str().parse::<i64>().or(
            Err(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: "illegal number".to_string(),
                },
                right_params.as_span(),
            )),
        )?)),
        Rule::variable => build_variable(right_params),
        Rule::boolean_expr => build_boolean_expr(right_params),
        Rule::array_expr => build_array_expr(right_params),
        _ => Err(Error::new_from_span(
            ErrorVariant::CustomError {
                message: "unsupport syntax".to_string(),
            },
            right_params.as_span(),
        )),
    }?)
}
fn build_assignment_expr(pair: pest::iterators::Pair<Rule>) -> Result<ASTNode, Error<Rule>> {
    let span = pair.as_span();
    let mut items = pair.into_inner();
    if let (Some(left), Some(right)) = (items.next(), items.next()) {
        let left_node = match left.as_rule() {
            Rule::variable => build_variable(left)?,
            _ => ASTNode::Other,
        };
        if ASTNode::Other.eq(&left_node) {
            return Err(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: "left value has syntax error".to_string(),
                },
                span,
            ));
        }
        let right_node = build_right_element(right)?;
        Ok(ASTNode::AssignmentExpr {
            left: Rc::new(left_node),
            right: Rc::new(right_node),
        })
    } else {
        Err(Error::new_from_span(
            ErrorVariant::CustomError {
                message: "left value is missing".to_string(),
            },
            span,
        ))
    }
}

fn build_create_variable_expr(pair: pest::iterators::Pair<Rule>) -> Result<ASTNode, Error<Rule>> {
    let span = pair.as_span();
    let mut items = pair.into_inner();
    let left_params = items.next().ok_or(Error::new_from_span(
        ErrorVariant::CustomError {
            message: "left value is missing".to_string(),
        },
        span,
    ))?;
    let right_params = items.next().ok_or(Error::new_from_span(
        ErrorVariant::CustomError {
            message: "right value is missing".to_string(),
        },
        span,
    ))?;

    let left_node = match left_params.as_rule() {
        Rule::variable => build_variable(left_params),
        _ => Err(Error::new_from_span(
            ErrorVariant::CustomError {
                message: "unsupport syntax".to_string(),
            },
            left_params.as_span(),
        )),
    }?;
    let right_node = build_right_element(right_params)?;
    Ok(ASTNode::AssingmentExpr {
        left: Rc::new(left_node),
        right: Rc::new(right_node),
    })
}

fn build_compute_expr(pair: pest::iterators::Pair<Rule>) -> Result<ASTNode, Error<Rule>> {
    let span = pair.as_span();
    let mut items = pair.into_inner();
    if let (Some(first), Some(op), Some(second)) = (items.next(), items.next(), items.next()) {
        let op_node = match op.as_rule() {
            Rule::bin_op => match op.as_str() {
                "-" => ASTNode::Op(Operator::Minus),
                "+" => ASTNode::Op(Operator::Plus),
                "*" => ASTNode::Op(Operator::Multi),
                "/" => ASTNode::Op(Operator::Divide),
                _ => ASTNode::Other,
            },
            _ => ASTNode::Other,
        };

        if ASTNode::Other.eq(&op_node) {
            return Err(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: "compute element is missing".to_string(),
                },
                op.as_span(),
            ));
        }

        let first_node = build_right_element(first)?;
        let second_node = build_right_element(second)?;
        return Ok(ASTNode::ComputeExpr {
            first: Rc::new(first_node),
            op: Rc::new(op_node),
            second: Rc::new(second_node),
        });
    } else {
        return Err(Error::new_from_span(
            ErrorVariant::CustomError {
                message: "compute element is missing".to_string(),
            },
            span,
        ));
    }
}

fn build_variable(pair: pest::iterators::Pair<Rule>) -> Result<ASTNode, Error<Rule>> {
    let span = pair.as_span();
    Ok(match pair.as_rule() {
        Rule::variable => {
            let name = String::from(pair.as_str());
            //判断是否是保留字
            if "真" == name || "假" == name {
                ASTNode::Boolean("真" == name)
            } else {
                ASTNode::Variable(name)
            }
        }
        _ => Err(Error::new_from_span(
            ErrorVariant::CustomError {
                message: "variable declare has syntax error".to_string(),
            },
            span,
        ))?,
    })
}

fn build_function_declare_statement(
    pair: pest::iterators::Pair<Rule>,
) -> Result<ASTNode, Error<Rule>> {
    let span = pair.as_span();
    let mut items = pair.into_inner();
    if let (Some(func_expr), Some(body)) = (items.next(), items.next()) {
        if Rule::function_expr.eq(&func_expr.as_rule()) && Rule::batch_expr.eq(&body.as_rule()) {
            let head_node = build_function_expr(func_expr)?;
            let body_node = build_multi_expr(body)?;
            if let ASTNode::FunctionCall { name, params } = head_node {
                let mut param_names = Vec::new();
                for param in params {
                    match &(*param) {
                        ASTNode::Variable(name) => param_names.push(name.clone()),
                        _ => Err(Error::new_from_span(
                            ErrorVariant::CustomError {
                                message: "function declare has syntax error".to_string(),
                            },
                            span,
                        ))?,
                    }
                }
                return Ok(ASTNode::FunctionDeclare {
                    name,
                    params: param_names,
                    body: body_node,
                });
            }
        }
    }
    return Err(Error::new_from_span(
        ErrorVariant::CustomError {
            message: "function declare has syntax error".to_string(),
        },
        span,
    ));
}
fn build_function_expr(pair: pest::iterators::Pair<Rule>) -> Result<ASTNode, Error<Rule>> {
    let span = pair.as_span();
    let mut items = pair.into_inner();
    if let Some(func_name) = items.next() {
        let func_name_str;
        if Rule::variable.eq(&func_name.as_rule()) {
            func_name_str = String::from(func_name.as_str());
        } else {
            return Err(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: "function expression element is missing".to_string(),
                },
                func_name.as_span(),
            ));
        }
        let mut params = Vec::new();
        while let Some(param) = items.next() {
            params.push(Rc::new(build_right_element(param)?));
        }
        Ok(ASTNode::FunctionCall {
            name: func_name_str,
            params,
        })
    } else {
        return Err(Error::new_from_span(
            ErrorVariant::CustomError {
                message: "function express has syntax error".to_string(),
            },
            span,
        ));
    }
}
fn build_boolean_expr(pair: pest::iterators::Pair<Rule>) -> Result<ASTNode, Error<Rule>> {
    let span = pair.as_span();
    let mut items = pair.into_inner();
    if let Some(first) = items.next() {
        let t = match first.as_rule() {
            Rule::boolean_value => ASTNode::Boolean(match first.as_str() {
                "真" => true,
                _ => false,
            }),
            Rule::boolean_not_op => {
                if let Some(second) = items.next() {
                    ASTNode::Not(Rc::new(build_compare_element(second)?))
                } else {
                    ASTNode::Other
                }
            }
            Rule::variable | Rule::function_expr => {
                if let (Some(op), Some(second)) = (items.next(), items.next()) {
                    ASTNode::ComputeExpr {
                        first: Rc::new(if Rule::variable.eq(&first.as_rule()) {
                            build_variable(first)?
                        } else {
                            build_function_expr(first)?
                        }),
                        op: Rc::new(ASTNode::Op(if "与".eq(op.as_str()) {
                            Operator::And
                        } else {
                            Operator::Or
                        })),
                        second: Rc::new(build_compare_element(second)?),
                    }
                } else {
                    ASTNode::Other
                }
            }
            _ => build_compare_element(first)?,
        };
        if ASTNode::Other.eq(&t) {
            return Err(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: "boolean express has syntax error".to_string(),
                },
                span,
            ));
        }
        Ok(t)
    } else {
        return Err(Error::new_from_span(
            ErrorVariant::CustomError {
                message: "boolean express has syntax error".to_string(),
            },
            span,
        ));
    }
}
fn build_array_expr(pair: pest::iterators::Pair<Rule>) -> Result<ASTNode, Error<Rule>> {
    let items = pair.into_inner();
    let mut asts = Vec::new();
    for item in items {
        asts.push(Rc::new(build_right_element(item)?));
    }
    Ok(ASTNode::Array(asts))
}
