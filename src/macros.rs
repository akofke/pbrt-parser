/// param!(Float(1, 2, 3))
#[macro_export]
macro_rules! param {
    ($param_name:ident, $param_type:ident($($val:expr),+)) => {
        crate::params::Param::new(stringify!($param_name).to_string(), crate::make_vals(crate::params::ParamVal::$param_type, &[$($val),+]))
    };
}

macro_rules! rgb {
    ($r:expr, $g:expr, $b:expr) => {
        crate::params::SpectrumVal::Rgb([$r as f32, $g as f32, $b as f32])
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
    use crate::make_vals;

    #[test]
    fn test_param() {
        let param = param!(foo, Float(1.0, 2.0, 3.0));
        assert_eq!(param, Param::new("foo".to_string(), make_vals(ParamVal::Float, &[1.0, 2.0, 3.0])))
    }
}

