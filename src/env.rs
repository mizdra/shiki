use crate::ast::Ident;
use crate::object::Object;
use crate::{Error, Result};
use std::cell::RefCell;
use std::rc::Rc;

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    local: HashMap<Ident, Object>,
}

impl Env {
    pub fn new(parent: Option<Env>, local: HashMap<Ident, Object>) -> Env {
        Env {
            parent: parent.map(|p| Rc::new(RefCell::new(p))),
            local,
        }
    }

    pub fn with_outer(outer: Rc<RefCell<Env>>) -> Env {
        Env {
            parent: Some(Rc::clone(&outer)),
            local: HashMap::new(),
        }
    }

    pub fn add(&mut self, ident: Ident, object: Object) {
        self.local.insert(ident, object);
    }

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
