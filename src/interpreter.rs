
use std::collections::{HashMap, LinkedList};
use std::mem::discriminant;
use crate::stmt_types::{
    Expr, Stmt, NativeType,
    LiteralExpr, VariableExpr, UnaryExpr, BinaryExpr, CallExpr,
    VarDeclStmt, AssignStmt, PrintStmt, BlockStmt,
    IfStmt, WhileStmt, FunDeclStmt, ExprStmt
};
use crate::token_types::TokenType;


fn rem_first_and_last(value: &str) -> &str {
    let mut chars = value.chars();
    chars.next();
    chars.next_back();
    chars.as_str()
}

struct LoxFunction {
    declaration: Stmt,
    closure: LinkedList<HashMap<String, NativeType>>,
}

struct Interpreter {
    environment: LinkedList<HashMap<String, NativeType>>,
}

impl Interpreter {
    fn new_child_env(&mut self) -> &mut HashMap<String, NativeType> {
        self.environment.push_front(HashMap::new());
        self.environment.front_mut().unwrap()
    }

    fn find_env_var(&self, name: &str) -> Option<&NativeType> {
        for env in self.environment.iter() {

            if let Some(val) = env.get(name) {
                return Some(val);
            }
        }
        None
    }

    fn assign_env_var(&mut self, name: &str, value: NativeType) {
        for env in self.environment.iter_mut() {
            if let Some(val) = env.get_mut(name) {
                *val = value.to_owned();
            }
        }
        match self.environment.front_mut() {
            Some(m) => {m.insert(name.to_owned(), value.to_owned()); }
            None => panic!("Environment not found")
        }
        return
    }


    fn init_environment(&mut self) {
        self.environment = LinkedList::new();
        self.environment.push_front(HashMap::new());
    }
    pub fn new() -> Interpreter {
        let mut environment = LinkedList::new();
        environment.push_front(HashMap::new());
        Interpreter { environment }
    }

    // --- Expressions --- //

    fn eval_literal(&mut self, expr: &LiteralExpr) -> NativeType {
        let token_type = expr.literal.token_type.clone();
        match expr.literal.token_type {
            TokenType::NUMBER => {
                let f: f64 = expr.literal.lexeme.parse().unwrap();
                NativeType::Num(f)
            }
            TokenType::STRING => {
                let s = rem_first_and_last(expr.literal.lexeme.as_str());
                NativeType::Str(s.to_string())
            }
            _ => panic!("Invalid literal type {:?}", expr.literal.token_type)
        }
    }

    fn eval_variable_ref(&mut self, expr: &VariableExpr) -> NativeType {
        match self.find_env_var(expr.identifier.lexeme.as_str()) {
            Some(val) => {
                val.to_owned()
            }
            None => {
                panic!(
                    "Reference to unknown variable {} on line {}",
                    expr.identifier.lexeme,
                    expr.identifier.line_number
                )
            }
        }
    }

    fn eval_binary_expr(&mut self, expr: &BinaryExpr) -> NativeType {
        let left = self.eval_expr(*expr.left.to_owned());
        let right = self.eval_expr(*expr.right.to_owned());
        match expr.operator.token_type {
            TokenType::PLUS => {
                match (left, right) {
                    (NativeType::Num(left), NativeType::Num(right)) => {
                        NativeType::Num(left + right)
                    }
                    (NativeType::Str(left), NativeType::Str(right)) => {
                        NativeType::Str(left + &right)
                    }
                    _ => panic!("Operands must be two numbers or two strings for '+' on line {}", expr.operator.line_number)
                }
            }
            TokenType::MINUS => {
                match (left, right) {
                    (NativeType::Num(left), NativeType::Num(right)) => {
                        NativeType::Num(left - right)
                    }
                    _ => panic!("Operands must be two numbers for '-' on line {}", expr.operator.line_number)
                }
            }
            TokenType::STAR => {
                match (left, right) {
                    (NativeType::Num(left), NativeType::Num(right)) => {
                        NativeType::Num(left * right)
                    }
                    _ => panic!("Operands must be two numbers for '*' on line {}", expr.operator.line_number)
                }
            }
            TokenType::SLASH => {
                match (left, right) {
                    (NativeType::Num(left), NativeType::Num(right)) => {
                        NativeType::Num(left / right)
                    }
                    _ => panic!("Operands must be two numbers for '/' on line {}", expr.operator.line_number)
                }
            }
            TokenType::GREATER => {
                match (left, right) {
                    (NativeType::Num(left), NativeType::Num(right)) => {
                        NativeType::Bool(left > right)
                    }
                    _ => panic!("Operands must be two numbers for '>' on line {}", expr.operator.line_number)
                }
            }
            TokenType::GREATER_EQUAL => {
                match (left, right) {
                    (NativeType::Num(left), NativeType::Num(right)) => {
                        NativeType::Bool(left >= right)
                    }
                    _ => panic!("Operands must be two numbers for '>=' on line {}", expr.operator.line_number)
                }
            }
            TokenType::LESS => {
                match (left, right) {
                    (NativeType::Num(left), NativeType::Num(right)) => {
                        NativeType::Bool(left < right)
                    }
                    _ => panic!("Operands must be two numbers for '<' on line {}", expr.operator.line_number)
                }
            }
            TokenType::LESS_EQUAL => {
                match (left, right) {
                    (NativeType::Num(left), NativeType::Num(right)) => {
                        NativeType::Bool(left <= right)
                    }
                    _ => panic!("Operands must be two numbers for '<=' on line {}", expr.operator.line_number)
                }
            }
            TokenType::EQUAL_EQUAL => {
                if discriminant(&left) == discriminant(&right) {
                    return NativeType::Bool(left == right)
                }
                return NativeType::Bool(false)
            }
            TokenType::BANG_EQUAL => {
                if discriminant(&left) == discriminant(&right) {
                    return NativeType::Bool(left != right)
                }
                return NativeType::Bool(true)
            }
            _ => panic!("Invalid binary operator {:?} on line {}", expr.operator.token_type, expr.operator.line_number)
        }
    }


    fn truthy(&mut self, val: NativeType) -> NativeType {
        match val {
            NativeType::Bool(t) => NativeType::Bool(t),
            NativeType::Num(t) => NativeType::Bool((t == 0.0)),
            _ => NativeType::Bool(true)
        }
    }
    fn eval_unary_expr(&mut self, expr: &UnaryExpr) -> NativeType {
        let right = self.eval_expr(*expr.right.to_owned());
        match expr.operator.token_type {
            TokenType::MINUS => {
                match (right) {
                    NativeType::Num(r) => {(NativeType::Num(-r))}
                    _ => panic!("Operand must be a number for unary '-' on line {}", expr.operator.line_number)
                }
            }
            TokenType::BANG => {
                match self.truthy(right) {
                    NativeType::Bool(t) => NativeType::Bool(!t),
                    _ => panic!("Operand must be a bool for unary '!' on line {}", expr.operator.line_number)
                }
            }
            _ => panic!("Unknown unary operator {:?} at line {}", expr.operator.token_type, expr.operator.line_number)
        }
    }

    fn eval_expr(&mut self, expr: Expr) -> NativeType {
        match expr {
            Expr::Literal(e) => {self.eval_literal(&e)}
            Expr::Variable(e) => {self.eval_variable_ref(&e)}
            Expr::Binary(e) => {self.eval_binary_expr(&e)}
            Expr::Unary(e) => {self.eval_unary_expr(&e)}
            _ => panic!("Unimplemented expr {:?}", expr),
        }
    }

    // --- Statements --- //

    fn run_var_decl_stmt(&mut self, stmt: VarDeclStmt){
        let var_name = stmt.identifier.lexeme;
        match self.find_env_var(&var_name) {
            Some(expr) => panic!("attempted to redeclare variable {}", var_name),
            None => {}
        }
        match stmt.init_val {
            Some(init_val) => {}
            None => {}
        }
    }

    fn run_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::VarDecl(s) => self.run_var_decl_stmt(s),
            _ => panic!("Unimplemented stmt type {:?}", stmt)
        }
    }


    pub fn interpret(&mut self, stmts: Vec<Stmt>) -> LinkedList<HashMap<String, NativeType>> {
        self.init_environment();
        for stmt in stmts {
            self.run_stmt(stmt);
        }

        return self.environment.clone();
    }
}