use crate::ast::Ident;
use crate::object::Object;
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
}
