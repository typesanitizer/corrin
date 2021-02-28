use std::collections::HashMap;
use std::sync::Arc;

#[derive(Clone, Debug)]
enum Expr {
    Lit(i64),
    Var(String),
    Let(String, Arc<Expr>, Arc<Expr>),
    Add(Arc<Expr>, Arc<Expr>)
}

#[derive(Clone, Debug)]
struct Decl {
    var: String,
    expr: Expr
}

impl Decl {
    fn elaborate(&self, symbol_table: &mut SymbolTable) -> Result<TypedDecl, String> {
        let expr = self.expr.elaborate(symbol_table)?;
        return Ok(TypedDecl { var: self.var.clone(), expr })
    }
}
#[derive(Clone, Debug)]
struct TypedDecl {
    var: String,
    expr: TypedExpr
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum Type {
    Int64
}

#[derive(Clone, Debug)]
enum TypedExpr {
    Lit(i64),
    Var(String, Type),
    Let(String, Type, Arc<TypedExpr>, Arc<TypedExpr>),
    Add(Type, Arc<TypedExpr>, Arc<TypedExpr>)
}

impl TypedExpr {
    fn type_(&self) -> Type {
        match *self {
            TypedExpr::Lit(_) => Type::Int64,
            TypedExpr::Var(_, ty) => ty,
            TypedExpr::Let(_, ty, _, _) => ty,
            TypedExpr::Add(ty, _, _) => ty
        }
    }
}

type SymbolTable = HashMap<String, Type>;

#[derive(Clone)]
struct Module {
    decls: Vec<Decl>
}

#[derive(Debug)]
struct TypedModule {
    decls: Vec<TypedDecl>,
    symbol_table: SymbolTable
}


fn test_module() -> Module {
    Module {
        decls: vec![
            Decl { var: "x".to_string(), expr: Expr::Lit(3) },
            Decl {
                var: "y".to_string(),
                expr:
                Expr::Let(
                    "x".to_string(),
                    Arc::new(Expr::Add(
                        Arc::new(Expr::Var("x".to_string())),
                        Arc::new(Expr::Lit(10))
                    )),
                    Arc::new(Expr::Add(
                        Arc::new(Expr::Var("x".to_string())),
                        Arc::new(Expr::Lit(3))
                    ))
                )
            }
        ]
    }
}

impl Module {
    fn outline_elaborate(&self) {
    }
    fn elaborate(&self) -> Result<TypedModule, String> {
        let mut typed_decls: Vec<TypedDecl> = vec![];
        let mut symbol_table: SymbolTable = SymbolTable::new();
        for decl in &self.decls {
            let typed_decl = decl.elaborate(&mut symbol_table)?;
            typed_decls.push(typed_decl.clone());
            symbol_table.insert(typed_decl.var, typed_decl.expr.type_());
        }
        return Ok(TypedModule { decls: typed_decls, symbol_table })
    }
}

impl Expr {
    fn elaborate(&self, symbol_table: &mut HashMap<String, Type>) -> Result<TypedExpr, String> {
        match self {
            Expr::Lit(i) => return Ok(TypedExpr::Lit(*i)),
            Expr::Var(s) => match symbol_table.get(&*s) {
                Some(t) => return Ok(TypedExpr::Var(s.clone(), *t)),
                None => return Err("Variable not defined.".to_string())
            }
            Expr::Let(s, rhs, body) => {
                let rhs = rhs.elaborate(symbol_table)?;
                symbol_table.insert(s.clone(), rhs.type_());
                let body = body.elaborate(symbol_table)?;
                symbol_table.remove(&*s);
                return Ok(TypedExpr::Let(s.clone(), rhs.type_(), Arc::new(rhs), Arc::new(body)))
            } 
            Expr::Add(lhs, rhs) => {
                let lhs = lhs.elaborate(symbol_table)?;
                let rhs = rhs.elaborate(symbol_table)?;
                if lhs.type_() != Type::Int64 || rhs.type_() != Type::Int64 {
                    return Err("Both operands need to be Int64".to_string());
                }
                return Ok(TypedExpr::Add(Type::Int64, Arc::new(lhs), Arc::new(rhs)));
            }
        }
    }
}

fn main() {
    println!("Hello, world!");
    println!("{:?}", test_module().elaborate());
}
