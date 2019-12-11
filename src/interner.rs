use std::cell::RefCell;
use nom::lib::std::collections::HashSet;

#[derive(Default)]
pub struct StringInterner {
    pool: RefCell<HashSet<Box<str>>>,
}

impl StringInterner {
    pub fn get_or_intern<'input, 'pool>(&'pool self, s: &'input str) -> &'pool str {
        let mut pool = self.pool.borrow_mut();
        if pool.contains(s) {
            let internal_ref: &'pool str = unsafe { std::mem::transmute(pool.get(s).unwrap().as_ref()) };
            internal_ref
        } else {
            let boxed = s.to_string().into_boxed_str();
            pool.insert(boxed);
            let ref_s: &'pool str = unsafe { std::mem::transmute(pool.get(s).unwrap().as_ref())};
            ref_s
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