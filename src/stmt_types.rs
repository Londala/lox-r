
use crate::tokenizer::Token;

#[derive(Clone, Debug)]
pub struct LiteralExpr {
    pub literal: Token,
}

#[derive(Clone, Debug)]
pub struct VariableExpr {
    pub identifier: Token,
}

#[derive(Clone, Debug)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub arguments: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Literal(LiteralExpr),
    Variable(VariableExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Call(CallExpr),
}


#[derive(Clone, Debug)]
pub struct FunDeclStmt {
    pub fun_name: Token,
    pub params: Vec<Token>,
    pub body: BlockStmt,
}

#[derive(Clone, Debug)]
pub struct VarDeclStmt {
    pub identifier: Token,
    pub init_val: Option<Expr>,
}
#[derive(Clone, Debug)]
pub struct AssignStmt {
    pub identifier: Token,
    pub value: Expr,
}
#[derive(Clone, Debug)]
pub struct PrintStmt {
    pub expr: Expr,
}
#[derive(Clone, Debug)]
pub struct BlockStmt {
    pub statements: Vec<Stmt>,
}
#[derive(Clone, Debug)]
pub struct IfStmt {
    pub condition: Expr,
    pub run_if_true: Box<Stmt>,
    pub run_if_false: Option<Box<Stmt>>,
}
#[derive(Clone, Debug)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

#[derive(Clone, Debug)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    VarDecl(VarDeclStmt),
    Assign(AssignStmt),
    Print(PrintStmt),
    Block(BlockStmt),
    If(IfStmt),
    While(WhileStmt),
    FunDecl(FunDeclStmt),
    ExprStmt(ExprStmt),
}



// #[derive(Clone, Copy, Debug)]
// pub enum Expr {
//     Literal(Token),
//     Variable(Token),
//     Unary {
//         operator: Token,
//         right: Box<Expr>,
//     },
//     Binary {
//         left: Box<Expr>,
//         operator: Token,
//         right: Box<Expr>,
//     },
//     Call {
//         callee: Box<Expr>,
//         arguments: Vec<Expr>,
//     },
// }
//
// #[derive(Clone, Copy, Debug)]
// pub enum Stmt {
//     VarDecl {
//         identifier: Token,
//         init_val: Option<Expr>,
//     },
//     Assign {
//         identifier: Token,
//         value: Expr,
//     },
//     Print {
//         expr: Expr,
//     },
//     Block {
//         statements: Vec<Stmt>,
//     },
//     If {
//         condition: Expr,
//         run_if_true: Box<Stmt>,
//         run_if_false: Option<Box<Stmt>>,
//     },
//     While {
//         condition: Expr,
//         body: Box<Stmt>,
//     },
//     FunDecl {
//         fun_name: Token,
//         params: Vec<Token>,
//         body: Box<Stmt>,
//     },
//     ExprStmt {
//         expr: Expr,
//     },
// }