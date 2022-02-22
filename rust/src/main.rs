use std::fmt;
//use num::integer::div_rem;
use num::rational::Ratio;
use itertools::Itertools;

// data structures
#[derive(Clone,Copy)]
enum Binop {
    Add,
    Sub,
    Mul,
    Div,
}

impl fmt::Display for Binop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Binop::Add => "+",
            Binop::Sub => "-",
            Binop::Mul => "*",
            Binop::Div => "/"
        };
        write!(f, "{}", s)
    }
}

fn binop_combinations(n:u32) -> Vec<Vec<Binop>> {
    match n {
        0 => vec![vec![], vec![], vec![], vec![]],
        1 =>
            vec![vec![Binop::Add], vec![Binop::Sub], vec![Binop::Mul], vec![Binop::Div]],
        x => {
            let mut dest_items: Vec<Vec<Binop>> = vec![];
            let last_items = binop_combinations(x-1);
            for item in last_items {
                for c in [Binop::Add, Binop::Sub, Binop::Mul, Binop::Div] {
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
            Formula::Num(x) => write!(f, "{}", x),
            Formula::Arith(left, op, right) =>
                write!(f, "({} {} {})", op, *left, *right)
        }
    }
}

fn calc(f:&Formula) -> Option<Ratio<i64>> {
    match &*f {
        Formula::Num(x) => Some(Ratio::from_integer(*x)),
        Formula::Arith(left, Binop::Add, right) => n_add(calc(&left), calc(&right)),
        Formula::Arith(left, Binop::Sub, right) => n_sub(calc(&left), calc(&right)),
        Formula::Arith(left, Binop::Mul, right) => n_mul(calc(&left), calc(&right)),
        Formula::Arith(left, Binop::Div, right) => n_div(calc(&left), calc(&right)),
    }
}

fn n_add(a: Option<Ratio<i64>>, b: Option<Ratio<i64>>) -> Option<Ratio<i64>> {
    match (a, b) {
        (Some(x), Some(y)) => Some(x+y),
        _ => None
    }
}

fn n_sub(a: Option<Ratio<i64>>, b: Option<Ratio<i64>>) -> Option<Ratio<i64>> {
    match (a, b) {
        (Some(x), Some(y)) => Some(x-y),
        _ => None
    }
}

fn n_mul(a: Option<Ratio<i64>>, b: Option<Ratio<i64>>) -> Option<Ratio<i64>> {
    match (a, b) {
        (Some(x), Some(y)) => Some(x*y),
        _ => None
    }
}

fn n_div(a: Option<Ratio<i64>>, b: Option<Ratio<i64>>) -> Option<Ratio<i64>> {
    match (a, b) {
        (Some(x), Some(y)) => {
            if y == Ratio::from_integer(0) {
                None
            } else {
                Some( x / y)
            }
        },
        _ => None
    }
}

fn fabox(left: Box<Formula>, op: Binop, right: Box<Formula>) -> Box<Formula> {
    Box::new(Formula::Arith(left, op, right))
}

fn build_formula_types<'a>(numbers: Vec<i64>, ops: Vec<Binop>) -> Vec<Formula> {
    let (op0, op1, op2) = (ops[0], ops[1], ops[2]);

    let fnbox = {|n| Box::new(Formula::Num(numbers[n]))};


    vec![
        Formula::Arith(
            fabox(
                fabox(
                    fnbox(0),
                    op0,
                    fnbox(1)),
                op1,
                fnbox(2)),
            op2,
            fnbox(3)),

        Formula::Arith (
            fabox(
                fnbox(0),
                op0,
                fnbox(1)),
            op1,
            fabox(
                fnbox(2),
                op2,
                fnbox(3))),

        Formula::Arith(
            fnbox(0),
            op0,
            fabox(
                fnbox(1),
                op1,
                fabox(
                    fnbox(2),
                    op2,
                    fnbox(3)))),

    ]
}

fn main() {
    let inputs = vec![1, 3, 4, 6];
    let perms = inputs.into_iter().permutations(4);
    for numbers in perms {
        let bop_comb = binop_combinations(3);
        for ops in bop_comb {
            for f in build_formula_types(numbers.clone(), ops) {
                match calc(&f) {
                    Some(x) => {
                        if x == Ratio::from_integer(24) {
                            println!("got it {}", f)
                        }
                    },
                    _ => ()
                }

            }
        }
    }

}
