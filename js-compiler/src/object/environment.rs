use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use crate::object::Object;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: Rc<RefCell<HashMap<String, Object>>>,
    outer: Option<Rc<Environment>>,
    pub output: Rc<RefCell<Vec<String>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: Rc::new(RefCell::new(HashMap::new())),
            outer: None,
            output: Rc::new(RefCell::new(Vec::new())),
        }
    }

    pub fn new_enclosed(outer: Environment) -> Self {
        let output = outer.output.clone();
        Environment {
            store: Rc::new(RefCell::new(HashMap::new())),
            outer: Some(Rc::new(outer)),
            output,
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.borrow().get(name) {
            Some(obj) => Some(obj.clone()),
            None => match &self.outer {
                Some(outer) => outer.get(name),
                None => None,
            },
        }
    }

    pub fn set(&mut self, name: String, val: Object) -> Object {
        self.store.borrow_mut().insert(name, val.clone());
        val
    }
}
