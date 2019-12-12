/// param!(Float(1, 2, 3))
#[macro_export]
macro_rules! param {
    ($param_name:ident, $param_type:ident($($val:expr),+)) => {
        crate::params::Param::new(stringify!($param_name).into(), crate::params::ParamVal::$param_type(vec![$($val.into()),+]))
    };
}

#[macro_export]
macro_rules! tf {
    ($tf_type:ident($($x:expr),+)) => {
        crate::transform::TransformStmt::$tf_type(Box::new([$($x as f32),+]))
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::params::{Param, ParamVal};

    #[test]
    fn test_param() {
        let param = param!(foo, Float(1.0, 2.0, 3.0));
        assert_eq!(param, Param::new("foo", ParamVal::Float(vec![1.0, 2.0, 3.0])));
    }
}

