use std::collections::HashMap;
use std::rc::Rc;

type Method = fn(receiver: &mut Class, args: Vec<Class>) -> Class;

struct Class {
    name: String,
    superclass: Rc<Option<Class>>,
    methods: HashMap<String, Method>
}

impl Class {
    fn send(&mut self, selector: String, args: Vec<Class>) -> Class {
        match self.methods.get(&selector) {
            None => panic!("todo: error message"),
            Some(method) => method(self, args)
        }
    }
}

fn main() {

}
