/// shiki 言語の環境.
use crate::ast::Ident;
use crate::object::Object;
use crate::{Error, Result};
use std::cell::RefCell;
use std::rc::Rc;

use std::collections::HashMap;

/// 環境
#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    local: HashMap<Ident, Object>,
}

impl Env {
    /// 新しい環境を返す
    pub fn new(parent: Option<Env>, local: HashMap<Ident, Object>) -> Env {
        Env {
            parent: parent.map(|p| Rc::new(RefCell::new(p))),
            local,
        }
    }

    /// `outer` を親に持つ新しい環境を返す
    pub fn with_outer(outer: Rc<RefCell<Env>>) -> Env {
        Env {
            parent: Some(Rc::clone(&outer)),
            local: HashMap::new(),
        }
    }

    /// 環境に変数を宣言する
    pub fn add(&mut self, ident: Ident, object: Object) {
        self.local.insert(ident, object);
    }

    /// 環境から変数を取得する
    pub fn get(&mut self, ident: &Ident) -> Option<Object> {
        match self.local.get(ident) {
            Some(object) => Some(object.clone()),
            None => {
                if let Some(ref mut parent) = self.parent {
                    parent.borrow_mut().get(ident)
                } else {
                    None
                }
            }
        }
    }

    /// 環境の変数を更新する
    pub fn update(&mut self, ident: Ident, new_object: Object) -> Result<()> {
        if self.local.contains_key(&ident) {
            self.local.insert(ident, new_object);
            return Ok(());
        }
        if let Some(ref mut parent) = self.parent {
            return parent.borrow_mut().update(ident, new_object);
        }
        Err(Error::RuntimeError(format!(
            "cannot find identifier `{}` in this scope",
            ident.get_ident_name(),
        )))
    }
}
