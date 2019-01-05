use crate::{ast::*, Error, Object, Result};

fn error(msg: String) -> Result<Object> {
    Err(Error::RuntimeError(msg))
}

pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {}
    }

    fn eval_literal(&mut self, literal: Literal) -> Result<Object> {
        match literal {
            Literal::Int(val) => Ok(Object::Int(val)),
            Literal::String(val) => Ok(Object::String(val)),
            Literal::Bool(val) => Ok(Object::Bool(val)),
        }
    }

    fn eval_prefix(&mut self, prefix: Prefix, expr: Expr) -> Result<Object> {
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

    fn eval_infix(&mut self, infix: Infix, left: Expr, right: Expr) -> Result<Object> {
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

    fn eval_expr(&mut self, expr: Expr) -> Result<Object> {
        match expr {
            Expr::Literal(literal) => self.eval_literal(literal),
            Expr::Prefix(prefix, e) => self.eval_prefix(prefix, *e),
            Expr::Infix(infix, left, right) => self.eval_infix(infix, *left, *right),
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
    fn test_expr_stmt() {
        // 優先順位のテストは parser の責務であるため, ここではテストしない

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
    }
}
