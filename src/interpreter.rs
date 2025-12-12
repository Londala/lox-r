
use std::collections::{HashMap, LinkedList};
use std::mem::discriminant;
use derive_more::Display;
use crate::stmt_types::{
    Expr, Stmt,
    LiteralExpr, VariableExpr, UnaryExpr, BinaryExpr, CallExpr,
    VarDeclStmt, AssignStmt, PrintStmt, BlockStmt,
    IfStmt, WhileStmt, FunDeclStmt, ExprStmt
};
use crate::token_types::TokenType;

#[derive(Clone, Debug)]
pub struct LoxFunction {
    declaration: FunDeclStmt,
    closure: LinkedList<HashMap<String, NativeType>>,
}

impl PartialEq for LoxFunction {
    fn eq(&self, other: &Self) -> bool {
        false
    }
}

impl LoxFunction {

    pub fn new(declaration: FunDeclStmt, closure: LinkedList<HashMap<String, NativeType>>) -> LoxFunction {
        LoxFunction{declaration, closure}
    }

    pub fn arity(&self) -> usize {
        self.declaration.params.len()
    }

    pub fn call(&mut self, arguments: Vec<NativeType>) {
        let mut local_env: HashMap<String, NativeType> = HashMap::new();

        for (i, param) in self.declaration.params.iter().enumerate() {
            local_env.insert(param.lexeme.clone(), arguments[i].clone());
        }

        let mut interpreter = Interpreter::new_from_env(self.closure.clone());

        interpreter.environment.push_front(local_env);

        for statement in self.declaration.body.statements.to_owned() {
            interpreter.run_stmt(statement);
        }
    }
}


#[derive(Clone, Debug, PartialEq, Display)]
pub enum NativeType {
    #[display("{}", _0)]
    Str(String),
    #[display("{}", _0)]
    Num(f64),
    #[display("{}", _0)]
    Bool(bool),
    #[display("None")]
    NONE,
    #[display("{}", _0.declaration.fun_name.lexeme)]
    LoxFunc(LoxFunction)
}

fn rem_first_and_last(value: &str) -> &str {
    let mut chars = value.chars();
    chars.next();
    chars.next_back();
    chars.as_str()
}

fn truthy(val: NativeType) -> NativeType {
    match val {
        NativeType::Bool(t) => NativeType::Bool(t),
        NativeType::Num(t) => NativeType::Bool(t == 0.0),
        _ => NativeType::Bool(true)
    }
}

pub struct Interpreter {
    environment: LinkedList<HashMap<String, NativeType>>,
}

impl Interpreter {
    fn new_child_env(&mut self) -> &mut HashMap<String, NativeType> {
        self.environment.push_front(HashMap::new());
        self.environment.front_mut().unwrap()
    }

    fn pop_env(&mut self) -> HashMap<String, NativeType> {
        self.environment.pop_front().unwrap()
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

    pub fn new_from_env(env: LinkedList<HashMap<String, NativeType>>) -> Interpreter {
        Interpreter { environment: env }
    }

    // --- Expressions --- //

    fn eval_literal(&mut self, expr: &LiteralExpr) -> NativeType {
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
        let left = self.eval_expr(&*expr.left);
        let right = self.eval_expr(&*expr.right);
        match expr.operator.token_type {
            TokenType::PLUS => {
                match (left, right) {
                    (NativeType::Num(left), NativeType::Num(right)) => {
                        NativeType::Num(left + right)
                    }
                    (NativeType::Str(left), NativeType::Str(right)) => {
                        NativeType::Str(left + &right)
                    }
                    (NativeType::Str(left), NativeType::Num(right)) => {
                        NativeType::Str(format!("{}{}", left, right))
                    }
                    (NativeType::Num(left), NativeType::Str(right)) => {
                        NativeType::Str(format!("{}{}", left, right))
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



    fn eval_unary_expr(&mut self, expr: &UnaryExpr) -> NativeType {
        let right = self.eval_expr(&*expr.right);
        match expr.operator.token_type {
            TokenType::MINUS => {
                match right {
                    NativeType::Num(r) => {NativeType::Num(-r)}
                    _ => panic!("Operand must be a number for unary '-' on line {}", expr.operator.line_number)
                }
            }
            TokenType::BANG => {
                match truthy(right) {
                    NativeType::Bool(t) => NativeType::Bool(!t),
                    _ => panic!("Operand must be a bool for unary '!' on line {}", expr.operator.line_number)
                }
            }
            _ => panic!("Unknown unary operator {:?} at line {}", expr.operator.token_type, expr.operator.line_number)
        }
    }

    fn eval_call_expr(&mut self, expr: &CallExpr) -> NativeType {
        let callee = self.eval_expr(&*expr.callee);
        match callee {
            NativeType::LoxFunc(mut f) => {
                let mut arguments = Vec::new();
                for arg in &expr.arguments {
                    arguments.push(self.eval_expr(arg));
                }
                if arguments.len() != f.arity(){
                    panic!("Expected {} arguments but got {}", f.arity(), arguments.len());
                }
                f.call(arguments);
                NativeType::NONE
            }
            _ => panic!("Call expression called on non-LoxFunc: {:?}", callee),
        }
    }

    fn eval_expr(&mut self, expr: &Expr) -> NativeType {
        match expr {
            Expr::Literal(e) => {self.eval_literal(&e)}
            Expr::Variable(e) => {self.eval_variable_ref(&e)}
            Expr::Binary(e) => {self.eval_binary_expr(&e)}
            Expr::Unary(e) => {self.eval_unary_expr(&e)}
            Expr::Call(e) => {self.eval_call_expr(&e)}
            _ => panic!("Unimplemented expr {:?}", expr),
        }
    }

    // --- Statements --- //

    fn run_var_decl_stmt(&mut self, stmt: VarDeclStmt){
        let var_name = stmt.identifier.lexeme;
        match self.find_env_var(&var_name) {
            Some(expr) => panic!("attempted to redeclare variable {} on line {}", var_name, stmt.identifier.line_number),
            None => {}
        }
        match stmt.init_val {
            Some(init_val) => {
                let val = self.eval_expr(&init_val);
                self.assign_env_var(&var_name, val);
            }
            None => {self.assign_env_var(&var_name, NativeType::NONE)}
        }
    }

    fn run_assign_stmt(&mut self, stmt: AssignStmt){
        let var_name = stmt.identifier.lexeme;
        match self.find_env_var(&var_name) {
            Some(expr) => {
                let val = self.eval_expr(&stmt.value);
                self.assign_env_var(&var_name, val)},
            None => {panic!("attempted to assign non-existent variable {} on line {}", var_name, stmt.identifier.line_number)}
        }
    }

    fn run_print_stmt(&mut self, stmt: PrintStmt) {
        let val = self.eval_expr(&stmt.expr);
        println!("{}", val)
    }

    fn run_if_stmt(&mut self, mut stmt: IfStmt) {
        let conditional = self.eval_expr(&stmt.condition);
        if truthy(conditional) == NativeType::Bool(true) {
            match *stmt.run_if_true {
                Stmt::Block(b) => {self.run_block_stmt(b)}
                _ => panic!("If condition should be a block")
            }

        } else {
            if let Some(b) = stmt.run_if_false.take() {
                match *b {
                    Stmt::Block(block) => {self.run_block_stmt(block)}
                    _ => {}
                }
            }

        }
    }

    fn run_block_stmt(&mut self, stmt: BlockStmt) {
        self.new_child_env();
        for s in stmt.statements {
            self.run_stmt(s);
        }
        self.pop_env();
        return;
    }

    fn run_while_stmt(&mut self, stmt: WhileStmt) {
        while truthy(self.eval_expr(&stmt.condition)) == NativeType::Bool(true) {
            let body = *stmt.body.to_owned();
            match body {
                Stmt::Block(block) => {self.run_block_stmt(block)}
                _ => {}
            }
        }
    }

    fn run_fun_decl_stmt(&mut self, stmt: FunDeclStmt) {
        let func_name = stmt.fun_name.lexeme.clone();
        let function = LoxFunction::new(stmt, self.environment.clone());
        
        self.assign_env_var(&*func_name, NativeType::LoxFunc(function));
    }

    fn run_expr_stmt(&mut self, stmt: ExprStmt) {
        self.eval_expr(&stmt.expr);
        return
    }

    fn run_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::VarDecl(s) => self.run_var_decl_stmt(s),
            Stmt::Assign(s) => self.run_assign_stmt(s),
            Stmt::Print(s) => self.run_print_stmt(s),
            Stmt::If(s) => self.run_if_stmt(s),
            Stmt::While(s) => self.run_while_stmt(s),
            Stmt::FunDecl(s) => self.run_fun_decl_stmt(s),
            Stmt::ExprStmt(s) => self.run_expr_stmt(s),
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

pub fn interpret_statements(statements: Vec<Stmt>) -> LinkedList<HashMap<String, NativeType>> {
    let mut i = Interpreter::new();
    i.interpret(statements)
}