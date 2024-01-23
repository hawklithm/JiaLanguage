extern crate pest_derive;
use std::{any::Any, borrow::BorrowMut, collections::HashMap, fmt::Display, hash::Hash, rc::Rc};

use pest::{
    error::{Error, ErrorVariant},
    Parser,
};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "cpl.pest"]
struct MyParser;

fn main() {
    let test_data = "重复(乾 大于 坤)
abc 为 1;
若 乾 则
始
甲 为 60;
乙 为 60;
终
否则
乙 为 40;
若 天晴() 或 下雨() 则 甲 为 1;
输出(变量+\"难于上青天\");
令 甲 为 变量+\"难于上青天\";
若 乾 则 甲 为 60;/*1243124124*/
若 乾 大于 坤 则 甲 为 60;
令 甲 为 \"乾坤\";
令 乙 为 \"李四\";
令 乾 为 10000;
令 坤 为 乙;
若 乾 则
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


由 甲 至 乙 
始
a 为 2;
终;

令 乾 为 [\"张三\",\"李四\"];

[\"张三\",\"李四\"] 各 人员 唱(\"能不能给我一首歌的时间\");

函数 曰(变量)
始
  输出(变量+\"难于上青天\");
终;
输出();";
    match parse_input(test_data) {
        Ok(ast) => println!("{:#?}", ast),
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
                    return Ok(Object::StringObject(Rc::new(
                        first.to_string() + &second.to_string(),
                    )));
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

#[derive(Clone, PartialEq, Eq, Debug)]
enum Object {
    StringObject(Rc<String>),
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
    NoOp,
}

struct RuntimeContext {
    operation_param_stack: Vec<HashMap<String, Object>>,
    global_context: Rc<HashMap<String, Object>>,
    method_stack: HashMap<String, ASTNode>,
}

impl RuntimeContext {
    pub fn new() -> RuntimeContext {
        RuntimeContext {
            operation_param_stack: Vec::new(),
            global_context: Rc::new(HashMap::new()),
            method_stack: HashMap::new(),
        }
    }
    pub fn with_global_context(global_context: Rc<HashMap<String, Object>>) -> RuntimeContext {
        RuntimeContext {
            operation_param_stack: Vec::new(),
            global_context,
            method_stack: HashMap::new(),
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
    pub fn getValue(&self, context: &HashMap<&str, Object>) -> Option<Object> {
        match self {
            ASTNode::Variable(variable_name) => context.get(variable_name.as_str()).cloned(),
            ASTNode::StringConst(s) => Some(Object::StringObject(Rc::new(s.clone()))),
            ASTNode::Number(n) => Some(Object::IntegerObject(*n)),
            ASTNode::Boolean(b) => Some(Object::BooleanObject(*b)),
            ASTNode::Array(array) => {
                let mut r = Vec::new();

                for item in array {
                    r.push(item.getValue(context)?);
                }
                Some(Object::ArrayObject(r))
            }
            _ => None,
        }
    }
}
impl Workspace {
    pub fn new() -> Workspace {
        Workspace {
            runtime_context: RuntimeContext::new(),
        }
    }
    pub fn with_context(runtime_context: &RuntimeContext) -> Workspace {
        Workspace {
            runtime_context: RuntimeContext::with_global_context(
                runtime_context.global_context.clone(),
            ),
        }
    }

    pub fn with_new_stack(
        runtime_context: &RuntimeContext,
        operation_param_stack: HashMap<String, Object>,
    ) -> Workspace {
        Workspace {
            runtime_context: RuntimeContext {
                operation_param_stack: vec![operation_param_stack],
                global_context: runtime_context.global_context.clone(),
                method_stack: HashMap::new(),
            },
        }
    }

    pub fn execute(&mut self, node: &ASTNode) -> Result<Object, ExecuteError> {
        match node {
            ASTNode::AssingmentExpr { left, right } => {
                let right_value = self.execute(right)?;
                let variable = match &**left {
                    ASTNode::Variable(variable_name) => Ok(variable_name),
                    _ => Err(ExecuteError {
                        msg: "variable is missing".to_string(),
                    }),
                }?;
                if let Some(operation_param) = self.runtime_context.operation_param_stack.last_mut()
                {
                    operation_param.insert(variable.clone(), right_value);
                }
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
                let method =
                    self.runtime_context
                        .method_stack
                        .get(name.as_str())
                        .ok_or(ExecuteError {
                            msg: format!("no method named {} found", name),
                        })?;
                let next_param_context = match method {
                    ASTNode::FunctionDeclare {
                        name,
                        params,
                        body: _,
                    } => {
                        let mut param_mapping = HashMap::new();
                        let param_len = params.len();
                        for i in (0..param_len).rev() {
                            if let (Some(param_name), Some(param_data)) =
                                (params.get(i), param_objs.pop())
                            {
                                param_mapping.insert(param_name.clone(), param_data);
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
            ASTNode::FunctionDeclare { name, params, body } => todo!(),
            ASTNode::Not(_) => todo!(),
            ASTNode::AssignmentExpr { left, right } => todo!(),
            ASTNode::IfStatement {
                condition,
                process,
                else_process,
            } => todo!(),
            ASTNode::LoopStatement { condition, process } => todo!(),
            ASTNode::ForStatement {
                range_left,
                range_right,
                process,
            } => todo!(),
            ASTNode::ArrayLoop {
                array,
                variable,
                process,
            } => todo!(),
            ASTNode::CompOp(_) => todo!(),
            ASTNode::BinaryCompare { first, op, second } => todo!(),
            _ => todo!(),
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
        Rule::variable => ASTNode::Variable(String::from(pair.as_str())),
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
        Rule::variable => ASTNode::Variable(String::from(pair.as_str())),
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
        return Ok(build_loop_condtion(tmp)?);
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
            Rule::variable => Ok(ASTNode::Variable(String::from(item.as_str()))),
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
    if let (Some(range_left), Some(range_right)) = (items.next(), items.next()) {
        let left_node = build_loop_condtion(range_left)?;
        let right_node = build_loop_condtion(range_right)?;

        if let Some(expr) = items.next() {
            let process_node = build_multi_expr(expr)?;
            return Ok(ASTNode::ForStatement {
                range_left: Rc::new(left_node),
                range_right: Rc::new(right_node),
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
        Rule::variable => Ok(ASTNode::Variable(String::from(right_params.as_str()))),
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
            Rule::variable => ASTNode::Variable(String::from(left.as_str())),
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
        Rule::variable => Ok(ASTNode::Variable(String::from(left_params.as_str()))),
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
                            ASTNode::Variable(String::from(first.as_str()))
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
