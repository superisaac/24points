use crate::Binop::*;
use crate::Formula::*;
use itertools::Itertools;
use num::rational::Ratio;
use std::fmt;

// util functions and constants

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

fn tripple_terms(left: String, op: Binop, right: String) -> String {
    format!("{} {} {}", left, op, right)
}

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

impl Formula {
    fn calc(&self) -> Option<Ratio<i64>> {
        match self {
            Num(x) => Some(Ratio::from_integer(*x)),
            Arith(left, Add, right) => n_add(left.calc(), right.calc()),
            Arith(left, Sub, right) => n_sub(left.calc(), right.calc()),
            Arith(left, Mul, right) => n_mul(left.calc(), right.calc()),
            Arith(left, Div, right) => n_div(left.calc(), right.calc()),
        }
    }

    fn wrap_repr(&self) -> String {
        format!("({})", self.repr())
    }

    fn sub_right_repr(&self) -> String {
        match self {
            Arith(_, Add, _) => self.wrap_repr(),
            Arith(_, Sub, _) => self.wrap_repr(),
            _ => self.repr(),
        }
    }

    fn mul_repr(&self) -> String {
        match self {
            Num(_) => self.repr(),
            Arith(_, Mul, _) => self.repr(),
            _ => self.wrap_repr(),
        }
    }

    fn div_repr(&self) -> String {
        match self {
            Num(_) => self.repr(),
            _ => self.wrap_repr(),
        }
    }

    fn repr(&self) -> String {
        match self {
            Num(x) => x.to_string(),
            Arith(left, Add, right) => tripple_terms(left.repr(), Add, right.repr()),
            Arith(left, Sub, right) => tripple_terms(left.repr(), Sub, right.sub_right_repr()),
            Arith(left, Mul, right) => tripple_terms(left.mul_repr(), Mul, right.mul_repr()),
            Arith(left, Div, right) => tripple_terms(left.div_repr(), Div, right.div_repr()),
        }
    }

    fn build_formula_types(numbers: &Vec<i64>, ops: &Vec<Binop>) -> Vec<Formula> {
        let (op0, op1, op2) = (ops[0], ops[1], ops[2]);
        let num_box = { |n| Box::new(Num(numbers[n])) };
        let arith_box = { |left, op, right| Box::new(Arith(left, op, right)) };

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
}

fn main() {
    // extract the arguments
    let args_iter = std::env::args().skip(1);
    // inputs is an array of four integers
    let inputs = args_iter.map(|arg| arg.parse::<i64>().unwrap());

    // all combinations of bin ops, [[+, -, *], [+, -, /], ...]
    // the count is 64
    let binop_combs = binop_combinations(3);

    // permutations of inputs, a 24x4 matrix
    for numbers in inputs.permutations(4) {
        for ops in &binop_combs {
            for f in Formula::build_formula_types(&numbers, ops) {
                match f.calc() {
                    Some(x) => {
                        if x == Ratio::from_integer(24) {
                            println!("{}", f.repr())
                        }
                    }
                    _ => (),
                }
            }
        }
    }
}
