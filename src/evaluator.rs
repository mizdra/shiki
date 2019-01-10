/// shiki 言語の評価器.
use crate::env::Env;
use crate::{ast::*, Error, Object, Result};
use std::cell::RefCell;
use std::rc::Rc;

use std::collections::HashMap;

fn error(msg: String) -> Result<Object> {
    Err(Error::RuntimeError(msg))
}

/// 評価器
pub struct Evaluator {
    env: Rc<RefCell<Env>>,
}

impl Evaluator {
    /// 新しい評価器を返します.
    pub fn new() -> Evaluator {
        Evaluator {
            env: Rc::new(RefCell::new(Env::new(None, HashMap::new()))),
        }
    }

    /// 識別子を評価します.
    /// エラーが発生した場合は即座に評価を中断し, そのエラーを返します.
    fn eval_ident_expr(&mut self, ident: Ident) -> Result<Object> {
        match self.env.borrow_mut().get(&ident) {
            Some(object) => Ok(object),
            None => error(format!(
                "cannot find identifier `{}` in this scope",
                ident.get_ident_name(),
            )),
        }
    }

    /// リテラルを評価します.
    /// エラーが発生した場合は即座に評価を中断し, そのエラーを返します.
    fn eval_literal_expr(&mut self, literal: Literal) -> Result<Object> {
        match literal {
            Literal::Int(val) => Ok(Object::Int(val)),
            Literal::String(val) => Ok(Object::String(val)),
            Literal::Bool(val) => Ok(Object::Bool(val)),
            Literal::Unit => Ok(Object::Unit),
        }
    }

    /// 前置演算子を評価します.
    /// エラーが発生した場合は即座に評価を中断し, そのエラーを返します.
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

    /// 中置演算子を評価します.
    /// エラーが発生した場合は即座に評価を中断し, そのエラーを返します.
    fn eval_infix_expr(&mut self, infix: Infix, left: Expr, right: Expr) -> Result<Object> {
        match infix {
            Infix::AndAnd => {
                let left = self.eval_expr(left)?;
                match left {
                    Object::Bool(false) => return Ok(Object::Bool(false)),
                    Object::Bool(true) => {
                        let right = self.eval_expr(right)?;
                        match right {
                            Object::Bool(val) => return Ok(Object::Bool(val)),
                            _ => error(format!(
                                "no implementation for `{} && {}`",
                                left.get_type_name(),
                                right.get_type_name(),
                            )),
                        }
                    }
                    _ => error(format!(
                        "no implementation for `{} && ..`",
                        left.get_type_name(),
                    )),
                }
            }
            Infix::OrOr => {
                let left = self.eval_expr(left)?;
                match left {
                    Object::Bool(true) => return Ok(Object::Bool(true)),
                    Object::Bool(false) => {
                        let right = self.eval_expr(right)?;
                        match right {
                            Object::Bool(val) => return Ok(Object::Bool(val)),
                            _ => error(format!(
                                "no implementation for `{} || {}`",
                                left.get_type_name(),
                                right.get_type_name(),
                            )),
                        }
                    }
                    _ => error(format!(
                        "no implementation for `{} || ..`",
                        left.get_type_name(),
                    )),
                }
            }
            infix => {
                let left = self.eval_expr(left)?;
                let right = self.eval_expr(right)?;
                match (infix, left, right) {
                    (Infix::Plus, left, right) => match (left, right) {
                        (Object::Int(l_val), Object::Int(r_val)) => Ok(Object::Int(l_val + r_val)),
                        (Object::String(l_val), Object::String(r_val)) => {
                            Ok(Object::String(l_val + &r_val))
                        }
                        (left, right) => error(format!(
                            "no implementation for `{} + {}`",
                            left.get_type_name(),
                            right.get_type_name(),
                        )),
                    },
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
                        (Object::Int(l_val), Object::Int(r_val)) => {
                            Ok(Object::Bool(l_val == r_val))
                        }
                        (Object::String(l_val), Object::String(r_val)) => {
                            Ok(Object::Bool(l_val == r_val))
                        }
                        (Object::Bool(l_val), Object::Bool(r_val)) => {
                            Ok(Object::Bool(l_val == r_val))
                        }
                        (Object::Unit, Object::Unit) => Ok(Object::Bool(true)),
                        (left, right) => error(format!(
                            "no implementation for `{} == {}`",
                            left.get_type_name(),
                            right.get_type_name(),
                        )),
                    },
                    (Infix::NotEqual, left, right) => match (left, right) {
                        (Object::Int(l_val), Object::Int(r_val)) => {
                            Ok(Object::Bool(l_val != r_val))
                        }
                        (Object::String(l_val), Object::String(r_val)) => {
                            Ok(Object::Bool(l_val != r_val))
                        }
                        (Object::Bool(l_val), Object::Bool(r_val)) => {
                            Ok(Object::Bool(l_val != r_val))
                        }
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
        }
    }

    /// if式を評価します.
    /// エラーが発生した場合は即座に評価を中断し, そのエラーを返します.
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

    /// ラムダ式を評価します.
    /// エラーが発生した場合は即座に評価を中断し, そのエラーを返します.
    fn eval_lambda_expr(&mut self, params: Vec<Ident>, body: Expr) -> Result<Object> {
        Ok(Object::Lambda(Rc::clone(&self.env), params, body))
    }

    /// 呼び出し式を評価します.
    /// エラーが発生した場合は即座に評価を中断し, そのエラーを返します.
    fn eval_call_expr(&mut self, callee: Expr, args: Vec<Expr>) -> Result<Object> {
        let callee = self.eval_expr(callee)?;

        let (env, params, body) = match callee {
            Object::Lambda(env, params, body) => (env, params, body),
            _ => {
                return error(format!(
                    "expected `Lambda`, found `{}` variable",
                    callee.get_type_name(),
                ))
            }
        };

        if params.len() != args.len() {
            return error(format!(
                "`Lambda` takes {} parameter but {} parameters were supplied",
                params.len(),
                args.len(),
            ));
        }

        let mut evaluated_args = vec![];
        for arg in args {
            evaluated_args.push(self.eval_expr(arg)?);
        }

        let mut scoped_env = Env::with_outer(Rc::clone(&env));
        let list = params.iter().zip(evaluated_args.iter());
        for (ident, object) in list {
            scoped_env.add(ident.clone(), object.clone());
        }

        let current_env = self.env.clone();
        self.env = Rc::new(RefCell::new(scoped_env));

        // 評価した body の値が ReturnObject であれば
        // 中から Object を取り出す
        let object = match self.eval_expr(body) {
            Ok(object) => object,
            Err(Error::ReturnObject(object)) => object,
            err => return err,
        };
        self.env = current_env;

        Ok(object)
    }

    /// while式を評価します.
    /// エラーが発生した場合は即座に評価を中断し, そのエラーを返します.
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

    /// 式を評価します.
    /// エラーが発生した場合は即座に評価を中断し, そのエラーを返します.
    fn eval_expr(&mut self, expr: Expr) -> Result<Object> {
        match expr {
            Expr::Ident(ident) => self.eval_ident_expr(ident),
            Expr::Literal(literal) => self.eval_literal_expr(literal),
            Expr::Prefix(prefix, e) => self.eval_prefix_expr(prefix, *e),
            Expr::Infix(infix, left, right) => self.eval_infix_expr(infix, *left, *right),
            Expr::Block(block_stmt) => self.eval_block_stmt(block_stmt),
            Expr::If(cond, consequence, alternative) => {
                self.eval_if_expr(*cond, consequence, alternative)
            }
            Expr::Lambda(params, body) => self.eval_lambda_expr(params, *body),
            Expr::Call(callee, args) => self.eval_call_expr(*callee, args),
            Expr::While(cond, body) => self.eval_while_expr(*cond, body),
        }
    }

    /// return文を評価します.
    /// エラーが発生した場合は即座に評価を中断し, そのエラーを返します.
    fn eval_return_stmt(&mut self, expr: Expr) -> Result<Object> {
        let object = self.eval_expr(expr)?;
        Err(Error::ReturnObject(object))
    }

    /// 宣言文を評価します.
    /// エラーが発生した場合は即座に評価を中断し, そのエラーを返します.
    fn eval_let_stmt(&mut self, ident: Ident, expr: Expr) -> Result<Object> {
        let expr = self.eval_expr(expr)?;
        self.env.borrow_mut().add(ident, expr);
        Ok(Object::Unit)
    }

    /// 代入文を評価します.
    /// エラーが発生した場合は即座に評価を中断し, そのエラーを返します.
    fn eval_assign_stmt(&mut self, ident: Ident, expr: Expr) -> Result<Object> {
        let new_object = self.eval_expr(expr)?;
        self.env.borrow_mut().update(ident, new_object)?;
        Ok(Object::Unit)
    }

    /// 文を評価します.
    /// エラーが発生した場合は即座に評価を中断し, そのエラーを返します.
    fn eval_stmt(&mut self, stmt: Stmt) -> Result<Object> {
        match stmt {
            Stmt::Let(ident, expr) => self.eval_let_stmt(ident, expr),
            Stmt::Assign(ident, expr) => self.eval_assign_stmt(ident, expr),
            Stmt::Return(expr) => self.eval_return_stmt(expr),
            Stmt::Expr(expr) => self.eval_expr(expr),
        }
    }

    /// 複文を評価します.
    /// エラーが発生した場合は即座に評価を中断し, そのエラーを返します.
    fn eval_block_stmt(&mut self, block_stmt: BlockStmt) -> Result<Object> {
        let mut result = Object::Unit;
        for stmt in block_stmt {
            result = self.eval_stmt(stmt)?;
        }
        Ok(result)
    }

    /// プログラムを評価します.
    /// エラーが発生した場合は即座に評価を中断し, そのエラーを返します.
    pub fn eval(&mut self, program: Program) -> Result<Object> {
        match self.eval_block_stmt(program) {
            Err(Error::ReturnObject(_)) => {
                error(format!("cannnot return from outside of lambda expression"))
            }
            result => result,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Lexer, Parser};

    fn eval(src: &str) -> Object {
        let mut parser = Parser::new(Lexer::new(src));
        let program = parser.parse().unwrap();
        let mut evaluator = Evaluator::new();
        match evaluator.eval(program) {
            Ok(object) => object,
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn expr_evaluates_to_object() {
        // 式が評価され適切なオブジェクトが返されるかをテストする

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
        assert_eq!(eval("false && false"), Object::Bool(false && false));
        assert_eq!(eval("false && true"), Object::Bool(false && true));
        assert_eq!(eval("true && false"), Object::Bool(true && false));
        assert_eq!(eval("true && true"), Object::Bool(true && true));
        assert_eq!(eval("false || false"), Object::Bool(false || false));
        assert_eq!(eval("false || true"), Object::Bool(false || true));
        assert_eq!(eval("true || false"), Object::Bool(true || false));
        assert_eq!(eval("true || true"), Object::Bool(true || true));
        assert_eq!(
            eval("\"Hello \" + \"World!\""),
            Object::String("Hello World!".to_string())
        );
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

        // 変数
        assert_eq!(eval("let x = 1; x"), Object::Int(1));
        assert_eq!(eval("let x = 1; x + 2"), Object::Int(3));
        assert_eq!(eval("let x = 1; let x = x + 2; x"), Object::Int(3));
        assert_eq!(
            eval("let x = \"str\"; x"),
            Object::String("str".to_string())
        );
        assert_eq!(eval("let x = true; x"), Object::Bool(true));
        assert_eq!(eval("let x = (); x"), Object::Unit);
        assert!(if let Object::Lambda(..) = eval("let x = || 0; x") {
            true
        } else {
            false
        });

        // ラムダ式呼び出し
        assert_eq!(eval("(|| 0)()"), Object::Int(0));
        assert_eq!(eval("(|| { 0 })()"), Object::Int(0));
        assert_eq!(eval("(|| { 0; 1 })()"), Object::Int(1));
        assert_eq!(eval("(|| { return 0; 1 })()"), Object::Int(0));
        assert_eq!(eval("(|x| x)(1)"), Object::Int(1));
        assert_eq!(eval("(|x, y| x + y)(1, 2)"), Object::Int(3));
        assert_eq!(eval("let z = 3; (|x, y| x + y + z)(1, 2)"), Object::Int(6));

        // ブロック
        assert_eq!(eval("{}"), Object::Unit);
        assert_eq!(eval("{ 0 }"), Object::Int(0));
        assert_eq!(eval("{ 0; 1 }"), Object::Int(1));
        assert_eq!(eval("{ 0; 1 } + 2"), Object::Int(3));

        // 文
        assert_eq!(eval("let x = 0;"), Object::Unit);
        assert_eq!(eval("let x = 0; x = 1;"), Object::Unit);
        assert_eq!(eval("let x = 0; x = 1; x;"), Object::Int(1));
    }

    #[test]
    fn test_lambda() {
        // shiki は function scope を持つ
        assert_eq!(
            eval(
                r#"
        let val = 1;
        let f = |arg| arg + val;
        val + f(10);
        "#
            ),
            Object::Int(12)
        );
        assert_eq!(
            eval(
                r#"
        let x = 1;
        let f = |y| {
            let g = |z| x * 100 + y * 10 + z;
            g;
        };

        // y はキャプチャされるので captured_g は
        // 他のラムダ式呼び出しの影響を受けない
        let captured_g = f(2);
        f(1000); // これで captured_g の y が 1000 に書き換わることはない
        captured_g(3);
        "#
            ),
            Object::Int(123)
        );
        assert_eq!(
            eval(
                r#"
        let x = 1;
        let y = x; // 代入文は右辺の式を評価してそのオブジェクトをディープコピーする
        if true {
            // shiki は lexical scope ではなく function scope であるため,
            // x は再束縛される. ただし y はディープコピーされた
            // オブジェクトを持っているので再束縛の影響を受けない
            let x = 2;
        }
        x + y;
        "#
            ),
            Object::Int(3)
        );

        // 再帰呼び出しができる
        assert_eq!(
            eval(
                r#"
let pow = |a, b| {
  if b <= 1 {
    a;
  } else {
    a * pow(a, b - 1);
  }
};
pow(3, 3);
        "#
            ),
            Object::Int(27)
        );
    }

    #[test]
    fn evaluation_strategy_is_call_by_value() {
        // ラムダ式の束縛変数が値渡しされることをテストする
        // 仮引数に対する代入は外部の変数を変更しない
        assert_eq!(
            eval(
                r#"
        let outer = 0;
        let f = |inner| {
            inner = 1;
        };
        f(outer);
        outer;
        "#
            ),
            Object::Int(0)
        );
    }

    #[test]
    fn expression_is_evaluated_from_left_to_right() {
        // 式が左から順に評価されることをテストする
        assert_eq!(
            eval(
                r#"
        let str = "";
        let f = |c| { str = str + c; 0 };
        f("a") + f("b") * f("c") + f("d");
        (|a, b, c, d| 0)(f("a"), f("b"), f("c"), f("d"));
        str;
        "#
            ),
            Object::String("abcdabcd".to_string())
        );
    }

    #[test]
    fn logical_operator_is_short_circuit() {
        // 論理演算子が短絡評価されることをテストする
        assert_eq!(
            eval(
                r#"
        let str = "";
        let f = |c, ret_val| {
            str = str + c;
            ret_val
        };
        f("a", false) && f("b", false);
        f("a", false) && f("b", true);
        f("a", true) && f("b", false);
        f("a", true) && f("b", true);
        f("a", false) || f("b", false);
        f("a", false) || f("b", true);
        f("a", true) || f("b", false);
        f("a", true) || f("b", true);
        str;
        "#
            ),
            Object::String("aaababababaa".to_string())
        );
    }

    #[test]
    fn if_expression_is_short_circuit() {
        // if式が短絡評価されることをテストする
        assert_eq!(
            eval(
                r#"
        let str = "";
        let tee = |c| {
            str = str + c;
            c
        };
        let f = |c| {
            if c == tee("a") {
                str = str + "1";
            } else if c == tee("b") {
                str = str + "2";
            } else {
                str = str + "3";
            }
        };
        f("a");
        f("b");
        f("c");
        str;
        "#
            ),
            Object::String("a1ab2ab3".to_string())
        );
    }

    #[test]
    fn while_expression_loop_body() {
        // while式において `cond` が `false` になるまで
        // `body` を繰り返し評価することをテストする
        assert_eq!(
            eval(
                r#"
        let sum = 0;
        while sum < 10 {
            sum = sum + 1;
        }
        sum;
        "#
            ),
            Object::Int(10)
        );
    }
}
