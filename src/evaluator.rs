use crate::{ast::*, Error, Object, Result};

fn error(msg: String) -> Result<Object> {
    Err(Error::RuntimeError(msg))
}

pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {}
    }

    fn eval_literal_expr(&mut self, literal: Literal) -> Result<Object> {
        match literal {
            Literal::Int(val) => Ok(Object::Int(val)),
            Literal::String(val) => Ok(Object::String(val)),
            Literal::Bool(val) => Ok(Object::Bool(val)),
        }
    }

    fn eval_prefix_expr(&mut self, prefix: Prefix, expr: Expr) -> Result<Object> {
        let object = self.eval_expr(expr)?;
        match prefix {
            Prefix::Plus => Ok(object),
            Prefix::Minus => {
                if let Object::Int(val) = object {
                    Ok(Object::Int(-val))
                } else {
                    error(format!(
                        "no operator `-` found for `{}`",
                        object.get_type_name()
                    ))
                }
            }
            Prefix::Not => {
                if let Object::Bool(val) = object {
                    Ok(Object::Bool(!val))
                } else {
                    error(format!(
                        "no operator `!` found for `{}`",
                        object.get_type_name()
                    ))
                }
            }
        }
    }

    fn eval_infix_expr(&mut self, infix: Infix, left: Expr, right: Expr) -> Result<Object> {
        let left = self.eval_expr(left)?;
        let right = self.eval_expr(right)?;
        match (infix, left, right) {
            (Infix::Plus, Object::Int(l_val), Object::Int(r_val)) => Ok(Object::Int(l_val + r_val)),
            (Infix::Minus, Object::Int(l_val), Object::Int(r_val)) => {
                Ok(Object::Int(l_val - r_val))
            }
            (Infix::Divide, Object::Int(l_val), Object::Int(r_val)) => {
                Ok(Object::Int(l_val / r_val))
            }
            (Infix::Multiply, Object::Int(l_val), Object::Int(r_val)) => {
                Ok(Object::Int(l_val * r_val))
            }
            (Infix::Equal, left, right) => match (left, right) {
                (Object::Int(l_val), Object::Int(r_val)) => Ok(Object::Bool(l_val == r_val)),
                (Object::String(l_val), Object::String(r_val)) => Ok(Object::Bool(l_val == r_val)),
                (Object::Bool(l_val), Object::Bool(r_val)) => Ok(Object::Bool(l_val == r_val)),
                (Object::Unit, Object::Unit) => Ok(Object::Bool(true)),
                (left, right) => error(format!(
                    "no implementation for `{} == {}`",
                    left.get_type_name(),
                    right.get_type_name(),
                )),
            },
            (Infix::NotEqual, left, right) => match (left, right) {
                (Object::Int(l_val), Object::Int(r_val)) => Ok(Object::Bool(l_val != r_val)),
                (Object::String(l_val), Object::String(r_val)) => Ok(Object::Bool(l_val != r_val)),
                (Object::Bool(l_val), Object::Bool(r_val)) => Ok(Object::Bool(l_val != r_val)),
                (Object::Unit, Object::Unit) => Ok(Object::Bool(false)),
                (left, right) => error(format!(
                    "no implementation for `{} != {}`",
                    left.get_type_name(),
                    right.get_type_name(),
                )),
            },
            (Infix::GreaterThanEqual, Object::Int(l_val), Object::Int(r_val)) => {
                Ok(Object::Bool(l_val >= r_val))
            }
            (Infix::GreaterThan, Object::Int(l_val), Object::Int(r_val)) => {
                Ok(Object::Bool(l_val > r_val))
            }
            (Infix::LessThanEqual, Object::Int(l_val), Object::Int(r_val)) => {
                Ok(Object::Bool(l_val <= r_val))
            }
            (Infix::LessThan, Object::Int(l_val), Object::Int(r_val)) => {
                Ok(Object::Bool(l_val < r_val))
            }
            (infix, left, right) => error(format!(
                "no implementation for `{} {} {}`",
                left.get_type_name(),
                infix,
                right.get_type_name(),
            )),
        }
    }

    fn eval_if_expr(
        &mut self,
        cond: Expr,
        consequence: BlockStmt,
        alternative: Option<BlockStmt>,
    ) -> Result<Object> {
        let evaluated_cond = self.eval_expr(cond)?;
        match evaluated_cond {
            Object::Bool(val) => {
                if val == true {
                    self.eval_block_stmt(consequence)
                } else if let Some(alt) = alternative {
                    self.eval_block_stmt(alt)
                } else {
                    Ok(Object::Unit)
                }
            }
            _ => error(format!(
                "expected `Bool`, found `{}` variable",
                evaluated_cond.get_type_name(),
            )),
        }
    }

    fn eval_while_expr(&mut self, cond: Expr, body: BlockStmt) -> Result<Object> {
        loop {
            let evaluated_cond = self.eval_expr(cond.clone())?;
            match evaluated_cond {
                Object::Bool(val) => {
                    if val == true {
                        self.eval_block_stmt(body.clone())?;
                    } else {
                        return Ok(Object::Unit);
                    }
                }
                _ => {
                    return error(format!(
                        "expected `Bool`, found `{}` variable",
                        evaluated_cond.get_type_name(),
                    ))
                }
            }
        }
    }

    fn eval_expr(&mut self, expr: Expr) -> Result<Object> {
        match expr {
            Expr::Literal(literal) => self.eval_literal_expr(literal),
            Expr::Prefix(prefix, e) => self.eval_prefix_expr(prefix, *e),
            Expr::Infix(infix, left, right) => self.eval_infix_expr(infix, *left, *right),
            Expr::Block(block_stmt) => self.eval_block_stmt(block_stmt),
            Expr::If(cond, consequence, alternative) => {
                self.eval_if_expr(*cond, consequence, alternative)
            }
            Expr::Lambda(_args, _body) => unimplemented!(),
            Expr::Call(_lambda, _args) => unimplemented!(),
            Expr::While(cond, body) => self.eval_while_expr(*cond, body),
            _ => unimplemented!(),
        }
    }

    fn eval_stmt(&mut self, stmt: Stmt) -> Result<Object> {
        match stmt {
            // Stmt::Let(ident, expr) => self.eval_let_stmt(ident, expr),
            // Stmt::Return => self.eval_return_stmt(stmt),
            Stmt::Expr(expr) => self.eval_expr(expr),
            _ => unimplemented!(),
        }
    }

    fn eval_block_stmt(&mut self, block_stmt: BlockStmt) -> Result<Object> {
        let mut result = Ok(Object::Unit);
        for stmt in block_stmt {
            result = self.eval_stmt(stmt);
        }
        result
    }

    pub fn eval(&mut self, program: Program) -> Result<Object> {
        self.eval_block_stmt(program)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Lexer, Parser};

    fn eval(src: &str) -> Object {
        let mut parser = Parser::new(Lexer::new(src));
        let program = parser.parse();
        let mut evaluator = Evaluator::new();
        match evaluator.eval(program) {
            Ok(object) => object,
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn expr_evaluates_to_object() {
        // 式が評価され適切なオブジェクトが返されるかをテストする
        // 評価順のテストは

        // リテラル
        assert_eq!(eval("0"), Object::Int(0));
        assert_eq!(eval("\"str\""), Object::String("str".to_string()));
        assert_eq!(eval("true"), Object::Bool(true));

        // 基本的な演算
        assert_eq!(eval("1 + 2"), Object::Int(1 + 2));
        assert_eq!(eval("1 + -2"), Object::Int(1 + -2));
        assert_eq!(eval("1 - 2"), Object::Int(1 - 2));
        assert_eq!(eval("1 - -2"), Object::Int(1 - -2));
        assert_eq!(eval("2 * 3"), Object::Int(2 * 3));
        assert_eq!(eval("2 * -3"), Object::Int(2 * -3));
        assert_eq!(eval("-2 * -3"), Object::Int(-2 * -3));
        assert_eq!(eval("2 / 3"), Object::Int(2 / 3));
        assert_eq!(eval("2 / -3"), Object::Int(2 / -3));
        assert_eq!(eval("-2 / -3"), Object::Int(-2 / -3));
        assert_eq!(eval("1 == 1"), Object::Bool(1 == 1));
        assert_eq!(eval("1 == 2"), Object::Bool(1 == 2));
        assert_eq!(eval("true == true"), Object::Bool(true == true));
        assert_eq!(eval("true == false"), Object::Bool(true == false));
        assert_eq!(eval("\"str\" == \"str\""), Object::Bool("str" == "str"));
        assert_eq!(eval("\"str\" == \"\""), Object::Bool("str" == ""));
        assert_eq!(eval("1 != 1"), Object::Bool(1 != 1));
        assert_eq!(eval("1 != 2"), Object::Bool(1 != 2));
        assert_eq!(eval("true != true"), Object::Bool(true != true));
        assert_eq!(eval("true != false"), Object::Bool(true != false));
        assert_eq!(eval("\"str\" != \"str\""), Object::Bool("str" != "str"));
        assert_eq!(eval("\"str\" != \"\""), Object::Bool("str" != ""));
        assert_eq!(eval("1 >= 0"), Object::Bool(1 >= 0));
        assert_eq!(eval("1 >= 1"), Object::Bool(1 >= 1));
        assert_eq!(eval("1 >= 2"), Object::Bool(1 >= 2));
        assert_eq!(eval("1 > 0"), Object::Bool(1 > 0));
        assert_eq!(eval("1 > 1"), Object::Bool(1 > 1));
        assert_eq!(eval("1 > 2"), Object::Bool(1 > 2));
        assert_eq!(eval("0 <= 1"), Object::Bool(0 <= 1));
        assert_eq!(eval("1 <= 1"), Object::Bool(1 <= 1));
        assert_eq!(eval("2 <= 1"), Object::Bool(2 <= 1));
        assert_eq!(eval("0 < 1"), Object::Bool(0 < 1));
        assert_eq!(eval("1 < 1"), Object::Bool(1 < 1));
        assert_eq!(eval("2 < 1"), Object::Bool(2 < 1));

        // 複雑な演算
        assert_eq!(
            eval("1 + 2 * 3 + 4 * (5 - 6)"),
            Object::Int(1 + 2 * 3 + 4 * (5 - 6))
        );
        assert_eq!(
            eval("!!(!(!!!(0 == 0)) == (1 == 2))"),
            Object::Bool(!!(!(!!!(0 == 0)) == (1 == 2)))
        );

        // 制御構文
        assert_eq!(eval("if true { 10 }"), Object::Int(10));
        assert_eq!(eval("if false { 10 }"), Object::Unit);
        assert_eq!(eval("if true { 10 } else { 20 }"), Object::Int(10));
        assert_eq!(eval("if false { 10 } else { 20 }"), Object::Int(20));
        assert_eq!(eval("if true { 10 } else if true { 20 }"), Object::Int(10));
        assert_eq!(eval("if false { 10 } else if true { 20 }"), Object::Int(20));
        assert_eq!(eval("if true { 10 } else if false { 20 }"), Object::Int(10));
        assert_eq!(eval("if false { 10 } else if false { 20 }"), Object::Unit);
        assert_eq!(
            eval("if true { 10 } else if true { 20 } else { 30 }"),
            Object::Int(10)
        );
        assert_eq!(
            eval("if false { 10 } else if true { 20 } else { 30 }"),
            Object::Int(20)
        );
        assert_eq!(
            eval("if true { 10 } else if false { 20 } else { 30 }"),
            Object::Int(10)
        );
        assert_eq!(
            eval("if false { 10 } else if false { 20 } else { 30 }"),
            Object::Int(30)
        );
        assert_eq!(eval("while false { 10 }"), Object::Unit);
    }

    #[test]
    fn evaluation_strategy_is_call_by_value() {
        // TODO: ラムダ式の束縛変数が値渡しされることをテストする
    }

    #[test]
    fn expression_is_evaluated_from_left_to_right() {
        // TODO: 式が左から順に評価されることをテストする
    }

    #[test]
    fn logical_operator_is_short_circuit() {
        // TODO: 論理演算子が短絡評価されることをテストする
    }

    #[test]
    fn if_expression_is_short_circuit() {
        // TODO: if式が短絡評価されることをテストする
    }

    #[test]
    fn while_expression_loop_body() {
        // TODO: while式が `cond` が `false` になるまで
        // `body` を繰り返し評価することをテストする
    }
}
