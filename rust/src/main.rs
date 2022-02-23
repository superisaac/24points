use crate::Binop::{Add, Div, Mul, Sub};
use crate::Formula::{Arith, Num};
use itertools::Itertools;
use num::rational::Ratio;
use std::fmt;

// data structures
#[derive(Clone, Copy)]
enum Binop {
    Add,
    Sub,
    Mul,
    Div,
}

impl fmt::Display for Binop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
        };
        write!(f, "{}", s)
    }
}

fn binop_combinations(n: u32) -> Vec<Vec<Binop>> {
    match n {
        0 => vec![vec![]],
        x => {
            let mut dest_items: Vec<Vec<Binop>> = vec![];
            let last_items = binop_combinations(x - 1);
            for item in last_items {
                for c in [Add, Sub, Mul, Div] {
                    let mut det = vec![c];
                    det.extend_from_slice(&item);
                    dest_items.push(det);
                }
            }
            return dest_items;
        }
    }
}

// Formula
enum Formula {
    Num(i64),
    Arith(Box<Formula>, Binop, Box<Formula>),
}

impl fmt::Display for Formula {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Num(x) => write!(f, "{}", x),
            Arith(left, op, right) => write!(f, "({} {} {})", op, *left, *right),
        }
    }
}

fn calc(f: &Formula) -> Option<Ratio<i64>> {
    match &*f {
        Num(x) => Some(Ratio::from_integer(*x)),
        Arith(left, Add, right) => n_add(calc(&left), calc(&right)),
        Arith(left, Sub, right) => n_sub(calc(&left), calc(&right)),
        Arith(left, Mul, right) => n_mul(calc(&left), calc(&right)),
        Arith(left, Div, right) => n_div(calc(&left), calc(&right)),
    }
}

fn wrap_repr(f: Formula) -> String {
    format!("({})", repr(f))
}

fn sub_right_repr(right: Formula) -> String {
    match right {
        Arith(_, Add, _) => wrap_repr(right),
        Arith(_, Sub, _) => wrap_repr(right),
        _ => repr(right),
    }
}

fn mul_repr(f: Formula) -> String {
    match f {
        Num(_) => repr(f),
        Arith(_, Mul, _) => repr(f),
        _ => wrap_repr(f),
    }
}

fn div_repr(f: Formula) -> String {
    match f {
        Num(_) => repr(f),
        _ => wrap_repr(f),
    }
}

fn repr(f: Formula) -> String {
    match f {
        Num(x) => x.to_string(),
        Arith(left, Add, right) => format!("{} {} {}", repr(*left), Add, repr(*right)),
        Arith(left, Sub, right) => format!("{} {} {}", repr(*left), Sub, sub_right_repr(*right)),
        Arith(left, Mul, right) => format!("{} {} {}", mul_repr(*left), Mul, mul_repr(*right)),
        Arith(left, Div, right) => format!("{} {} {}", div_repr(*left), Div, div_repr(*right)),
    }
}

fn n_add(a: Option<Ratio<i64>>, b: Option<Ratio<i64>>) -> Option<Ratio<i64>> {
    match (a, b) {
        (Some(x), Some(y)) => Some(x + y),
        _ => None,
    }
}

fn n_sub(a: Option<Ratio<i64>>, b: Option<Ratio<i64>>) -> Option<Ratio<i64>> {
    match (a, b) {
        (Some(x), Some(y)) => Some(x - y),
        _ => None,
    }
}

fn n_mul(a: Option<Ratio<i64>>, b: Option<Ratio<i64>>) -> Option<Ratio<i64>> {
    match (a, b) {
        (Some(x), Some(y)) => Some(x * y),
        _ => None,
    }
}

fn n_div(a: Option<Ratio<i64>>, b: Option<Ratio<i64>>) -> Option<Ratio<i64>> {
    match (a, b) {
        (Some(x), Some(y)) => {
            if y == Ratio::from_integer(0) {
                None
            } else {
                Some(x / y)
            }
        }
        _ => None,
    }
}

fn arith_box(left: Box<Formula>, op: Binop, right: Box<Formula>) -> Box<Formula> {
    Box::new(Arith(left, op, right))
}

fn build_formula_types<'a>(numbers: Vec<i64>, ops: Vec<Binop>) -> Vec<Formula> {
    let (op0, op1, op2) = (ops[0], ops[1], ops[2]);
    let num_box = { |n| Box::new(Num(numbers[n])) };

    vec![
        Arith(
            arith_box(arith_box(num_box(0), op0, num_box(1)), op1, num_box(2)),
            op2,
            num_box(3),
        ),
        Arith(
            arith_box(num_box(0), op0, num_box(1)),
            op1,
            arith_box(num_box(2), op2, num_box(3)),
        ),
        Arith(
            num_box(0),
            op0,
            arith_box(num_box(1), op1, arith_box(num_box(2), op2, num_box(3))),
        ),
    ]
}

fn main() {
    // extract the arguments
    let args_iter = std::env::args().skip(1);
    // inputs is an array of four integers
    let inputs = args_iter.map(|arg| arg.parse::<i64>().unwrap());
    // permutations of inputs, a 24x4 matrix
    for numbers in inputs.permutations(4) {
        // all combinations of bin ops, [[+, -, *], [+, -, /], ...]
        // the count is 64
        let bop_comb = binop_combinations(3);
        for ops in bop_comb {
            for f in build_formula_types(numbers.clone(), ops) {
                match calc(&f) {
                    Some(x) => {
                        if x == Ratio::from_integer(24) {
                            println!("{}", repr(f))
                        }
                    }
                    _ => (),
                }
            }
        }
    }
}
