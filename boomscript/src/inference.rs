use crate::parser::{BinaryOperator, UntypedExpression};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Type {
    TyBool,
    TyInt,
    TyFloat,
    TyVariable(String),
    TyBinaryOp(Box<Type>, Box<Type>),
    TyFunction(Box<Type>, Box<Type>),
}

#[derive(Debug)]
pub struct TypedExpression {
    expr: UntypedExpression,
    type_: Type,
}

use std::sync::atomic::{AtomicUsize, Ordering};

static ID: AtomicUsize = AtomicUsize::new(0);

fn newTyVariable() -> Type {
    let id = ID.fetch_add(1, Ordering::Relaxed);
    Type::TyVariable(format!("_a{}", id))
}

fn occursCheck(vcheck: &str, ty: &Type) -> bool {
    match ty {
        Type::TyBool |
        Type::TyInt |
        Type::TyFloat => false,
        Type::TyVariable(v) => v == vcheck,
        Type::TyBinaryOp(left, right) =>
            occursCheck(vcheck, left.as_ref()) || occursCheck(vcheck, right.as_ref()),
        Type::TyFunction(v, expr) =>
            occursCheck(vcheck, v.as_ref()) || occursCheck(vcheck, expr.as_ref())
    }
}

fn substType(subst: &HashMap<String, Type>, ty: Type) -> Type {
    match ty {
        Type::TyBool |
        Type::TyInt |
        Type::TyFloat => ty,
        Type::TyVariable(ref v) => subst.get(v).unwrap_or(&ty).clone(),
        Type::TyBinaryOp(left, right) =>
            Type::TyBinaryOp(Box::new(substType(subst, left.as_ref().clone())), Box::new(substType(subst, right.as_ref().clone()))),
        Type::TyFunction(parameter, e) => {
            Type::TyFunction(Box::new(substType(subst, parameter.as_ref().clone())), Box::new(substType(subst, e.as_ref().clone())))
        }
    }
}

fn substConstrs(subst: &HashMap<String, Type>, constraints: Vec<(Type, Type)>) -> Vec<(Type, Type)> {
    constraints.iter().map(|(t1, t2)| (substType(subst, t1.clone()), substType(subst, t2.clone()))).collect()
}

fn solve(constraints: &[(Type, Type)]) -> Vec<(String, Type)> {
    match constraints.split_first() {
        None => vec![],
        Some(((t1, t2), tail)) => {
            match (t1, t2) {
                (Type::TyBool, Type::TyBool) |
                (Type::TyInt, Type::TyInt) |
                (Type::TyFloat, Type::TyFloat) => solve(tail),
                (Type::TyVariable(v), n) |
                (n, Type::TyVariable(v)) => {
                    if occursCheck(v, n) {
                        panic!("Cannot be solved (occurs check)")
                    }
                    let s = HashMap::from([(v.clone(), n.clone())]);
                    let constraints = substConstrs(&s, constraints.iter().map(|x| x.clone()).collect());
                    let mut subst = solve(&constraints);
                    let z: HashMap<String, Type> = subst.iter().cloned().collect();
                    let n = substType(&z, n.clone());
                    subst.push((v.clone(), n));
                    subst
                }
                (Type::TyBinaryOp(l1, r1), Type::TyBinaryOp(l2, r2)) |
                (Type::TyFunction(l1, r1), Type::TyFunction(l2, r2)) => {
                    let mut new_constraints: Vec<(Type, Type)> = Vec::new();
                    new_constraints.push((l1.as_ref().clone(), l2.as_ref().clone()));
                    new_constraints.push((r1.as_ref().clone(), r2.as_ref().clone()));

                    for c in constraints {
                        new_constraints.push(c.clone())
                    }

                    solve(&new_constraints[..])
                }
                _ => panic!("Could not solve")
            }
        }
    }
}

type TypingContext = HashMap<String, Type>;

fn generate(ctx: &mut TypingContext, e: &UntypedExpression) -> (Type, Vec<(Type, Type)>) {
    match e {
        UntypedExpression::Bool(_) => (Type::TyBool, vec![]),
        UntypedExpression::Int(_) => (Type::TyInt, vec![]),
        UntypedExpression::Float(_) => (Type::TyFloat, vec![]),
        UntypedExpression::Variable(v) => (ctx.get(v).unwrap().clone(), vec![]),
        UntypedExpression::BinaryOperation(left, op, right) => {
            let (t1, mut s1) = generate(ctx, left.as_ref());
            let (t2, mut s2) = generate(ctx, right.as_ref());
            s1.append(&mut s2);

            s1.push((t1, Type::TyInt));
            s1.push((t2, Type::TyInt));

            match op {
                BinaryOperator::Greater |
                BinaryOperator::Less => (Type::TyBool, s1),
                BinaryOperator::Add |
                BinaryOperator::Mul => (Type::TyInt, s1)
            }
        }
        UntypedExpression::Let(v, e) => {
            let targ = newTyVariable();
            ctx.insert(v.clone(), targ);
            let (t1, s1) = generate(ctx, e);
            (t1, s1)
        }
        UntypedExpression::Fn(parameter, e) => {
            let targ = newTyVariable();
            ctx.insert(parameter.clone(), targ);
            let (t1, s1) = generate(ctx, e);
            (t1, s1)
        }
    }
}

pub fn infer(expressions: &Vec<UntypedExpression>) -> Vec<Type> {
    let mut ctx = HashMap::new();
    let mut types = Vec::new();

    for e in expressions {
        let (typ, constraints) = generate(&mut ctx, &e);
        let subst: HashMap<String, Type> = solve(&constraints).into_iter().collect();
        let typ = substType(&subst, typ);
        types.push(typ)
    }

    types
}

