use std::cell::RefCell;
use nom::lib::std::collections::HashSet;
use std::rc::Rc;
use std::sync::{Arc};
use parking_lot::RwLock;
use nom::lib::std::fmt::{Formatter, Error};

#[derive(Default)]
pub struct StringInterner {
    pool: RwLock<HashSet<Arc<str>>>,
}

impl StringInterner {
    pub fn get_or_intern(&self, s: &str) -> Arc<str> {
        let pool = self.pool.read();
        if pool.contains(s) {
            let s_ref = pool.get(s).unwrap().clone();
            s_ref
        } else {
            std::mem::drop(pool);
            let mut pool = self.pool.write();
            let boxed: Arc<str> = s.into();
            let s_ref = boxed.clone();
            pool.insert(boxed);
            s_ref
        }
    }
}

impl std::fmt::Debug for StringInterner {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        f.debug_set()
            .entries(self.pool.read().iter().map(|a| {
                format!("{} - {} refs", a, Arc::strong_count(a))
            }))
            .finish()
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