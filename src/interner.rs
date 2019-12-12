use std::cell::RefCell;
use nom::lib::std::collections::HashSet;
use std::rc::Rc;

#[derive(Default, Debug)]
pub struct StringInterner {
    pool: RefCell<HashSet<Rc<str>>>,
}

impl StringInterner {
    pub fn get_or_intern(&self, s: &str) -> Rc<str> {
        let mut pool = self.pool.borrow_mut();
        if pool.contains(s) {
            let s_ref = pool.get(s).unwrap().clone();
            s_ref
        } else {
            let boxed: Rc<str> = s.into();
            let s_ref = boxed.clone();
            pool.insert(boxed);
            s_ref
        }
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_usage() {
        let interner = StringInterner::default();

        let a1 = interner.get_or_intern("a");
        let a2 = interner.get_or_intern(&"a".to_string());

        assert_eq!(a1, a2);
        assert_eq!(a1.as_ptr(), a2.as_ptr());

        let b1 = interner.get_or_intern("b");
        let s = "b".to_string();
        let b2 = interner.get_or_intern(&s);
        std::mem::drop(s);
        assert_eq!(b1, b2);
        assert_eq!(b1.as_ptr(), b2.as_ptr());

        // make the hashset reallocate
        for i in 0..10000 {
            let _ = interner.get_or_intern(&i.to_string());
        }

        assert_eq!(a1, a2);
        assert_eq!(a1.as_ptr(), a2.as_ptr());

    }
}