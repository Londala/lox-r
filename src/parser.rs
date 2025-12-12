use crate::tokenizer::Token;
use crate::token_types::TokenType;
use crate::stmt_types::{
    Expr, Stmt,
    LiteralExpr, VariableExpr, UnaryExpr, BinaryExpr, CallExpr,
    VarDeclStmt, AssignStmt, PrintStmt, BlockStmt,
    IfStmt, WhileStmt, FunDeclStmt, ExprStmt
};


pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn peek(&self) -> Option<&Token> {
        if self.is_at_end() { None } else { Some(&self.tokens[self.current]) }
    }

    fn next_token_type(&self) -> Option<TokenType> {
        self.peek().map(|t| t.token_type)
    }

    fn advance(&mut self) -> Option<&Token> {
        if self.is_at_end() {
            None
        } else {
            let t = &self.tokens[self.current];
            self.current += 1;
            Some(t)
        }
    }

    fn expect(&mut self, token_type: TokenType) -> Token {
        match self.peek() {
            Some(t) if t.token_type == token_type => self.advance().unwrap().clone(),
            Some(t) =>
                panic!(
                    "Expected {:?}, but got {:?} on line {}",
                    token_type,
                    t.token_type,
                    t.line_number
                ),
            None => panic!("Expected {:?}, but got end of file", token_type),
        }
    }


    // primary: NUMBER | STRING | IDENTIFIER | LEFT_PAREN expression RIGHT_PAREN
    fn parse_primary_expr(&mut self) -> Expr {
        match self.next_token_type() {
            Some(TokenType::STRING) | Some(TokenType::NUMBER) => {
                let lit = self.advance().unwrap();
                Expr::Literal(LiteralExpr {literal: lit.clone() })
            }
            Some(TokenType::IDENTIFIER) => {
                let ident = self.advance().unwrap();
                Expr::Variable(VariableExpr {identifier: ident.clone() })
            }
            Some(TokenType::LEFT_PAREN) => {
                self.advance(); // consume '('
                let expr = self.parse_expr();
                self.expect(TokenType::RIGHT_PAREN);
                expr
            }
            other => {
                let line = self.peek().map(|t| t.line_number).unwrap_or(0);
                panic!("Expected expression, found {:?} on line {}", other, line);
            }
        }
    }


    // call: primary ( LEFT_PAREN [ arguments ] RIGHT_PAREN )*
    // arguments: expression (COMMA expression)*
    fn parse_call_expr(&mut self) -> Expr {
        let mut expr = self.parse_primary_expr();

        loop {
            match self.next_token_type() {
                Some(TokenType::LEFT_PAREN) => {
                    self.advance(); // consume '('
                    let mut args = Vec::new();
                    if self.next_token_type() != Some(TokenType::RIGHT_PAREN) {
                        args.push(self.parse_expr());
                        while self.next_token_type() == Some(TokenType::COMMA) {
                            self.advance(); // consume ','
                            args.push(self.parse_expr());
                        }
                    }
                    self.expect(TokenType::RIGHT_PAREN);
                    expr = Expr::Call(CallExpr {
                        callee: Box::new(expr),
                        arguments: args,
                    });
                }
                _ => break,
            }
        }

        expr
    }


    // unary: ( BANG | MINUS ) unary | call
    fn parse_unary_expr(&mut self) -> Expr {
        match self.next_token_type() {
            Some(TokenType::BANG) | Some(TokenType::MINUS) => {
                let operator = self.advance().unwrap().clone();
                let right = self.parse_unary_expr();
                Expr::Unary(UnaryExpr {
                    operator: operator.clone(),
                    right: Box::new(right),
                })
            }
            _ => self.parse_call_expr(),
        }
    }


    // factor: unary ( ( SLASH | STAR ) unary )*
    fn parse_factor_expr(&mut self) -> Expr {
        let mut expr = self.parse_unary_expr();

        while matches!(self.next_token_type(), Some(TokenType::SLASH | TokenType::STAR)) {
            let operator = self.advance().unwrap().clone();
            let right = self.parse_unary_expr();
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator: operator.clone(),
                right: Box::new(right),
            });
        }

        expr
    }

    // term: factor ( ( MINUS | PLUS ) factor )*
    fn parse_term_expr(&mut self) -> Expr {
        let mut expr = self.parse_factor_expr();

        while matches!(self.next_token_type(), Some(TokenType::MINUS | TokenType::PLUS)) {
            let operator = self.advance().unwrap().clone();
            let right = self.parse_factor_expr();
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator: operator.clone(),
                right: Box::new(right),
            });
        }

        expr
    }

    // comparison: term ( ( GREATER | GREATER_EQUAL | LESS | LESS_EQUAL ) term )*
    fn parse_comparison_expr(&mut self) -> Expr {
        let mut expr = self.parse_term_expr();

        while matches!(
            self.next_token_type(),
            Some(TokenType::GREATER
                | TokenType::GREATER_EQUAL
                | TokenType::LESS
                | TokenType::LESS_EQUAL)
        ) {
            let operator = self.advance().unwrap().clone();
            let right = self.parse_term_expr();
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator: operator.clone(),
                right: Box::new(right),
            });
        }

        expr
    }

    // equality: comparison ( ( BANG_EQUAL | EQUAL_EQUAL ) comparison )*
    fn parse_equality_expr(&mut self) -> Expr {
        let mut expr = self.parse_comparison_expr();

        while matches!(
            self.next_token_type(),
            Some(TokenType::BANG_EQUAL | TokenType::EQUAL_EQUAL)
        ) {
            let operator = self.advance().unwrap().clone();
            let right = self.parse_comparison_expr();
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator: operator.clone(),
                right: Box::new(right),
            });
        }

        expr
    }

    // expression: equality
    fn parse_expr(&mut self) -> Expr {
        self.parse_equality_expr()
    }


    // varDeclStmt: VAR IDENTIFIER [EQUAL expression] SEMICOLON
    fn parse_var_decl_stmt(&mut self) -> Stmt {
        self.expect(TokenType::VAR);
        let var_name = self.expect(TokenType::IDENTIFIER);

        let mut init_val: Option<Expr> = None;
        if self.next_token_type() == Some(TokenType::EQUAL) {
            self.expect(TokenType::EQUAL);
            init_val = Some(self.parse_expr());
        }

        self.expect(TokenType::SEMICOLON);
        Stmt::VarDecl(VarDeclStmt {
            identifier: var_name,
            init_val,
        })
    }


    // assignStmt: IDENTIFIER EQUAL expression SEMICOLON
    fn parse_assign_stmt(&mut self) -> Stmt {
        let var_name = self.expect(TokenType::IDENTIFIER);
        self.expect(TokenType::EQUAL);
        let expr = self.parse_expr();
        self.expect(TokenType::SEMICOLON);
        Stmt::Assign(AssignStmt {
            identifier: var_name,
            value: expr,
        })
    }


    // printStmt: PRINT expression SEMICOLON
    fn parse_print_stmt(&mut self) -> Stmt {
        self.expect(TokenType::PRINT);
        let expr = self.parse_expr();
        self.expect(TokenType::SEMICOLON);
        Stmt::Print(PrintStmt { expr })
    }


    // block: LEFT_BRACE statement* RIGHT_BRACE
    fn parse_block(&mut self) -> Stmt {
        self.expect(TokenType::LEFT_BRACE);
        let mut body: Vec<Stmt> = Vec::new();

        while self.next_token_type() != Some(TokenType::RIGHT_BRACE) {
            if self.is_at_end() {
                panic!("Unterminated block: missing '}}'");
            }
            let stmt = self.parse_statement();
            body.push(stmt);
        }

        self.advance(); // consume RIGHT_BRACE
        Stmt::Block(BlockStmt{ statements: body })
    }


    // ifStmt: IF LEFT_PAREN expression RIGHT_PAREN block [ELSE block]
    fn parse_if_stmt(&mut self) -> Stmt {
        self.expect(TokenType::IF);
        self.expect(TokenType::LEFT_PAREN);
        let condition = self.parse_expr();
        self.expect(TokenType::RIGHT_PAREN);

        let run_if_true = self.parse_block();

        let run_if_false = if self.next_token_type() == Some(TokenType::ELSE) {
            self.advance(); // consume ELSE
            Some(Box::new(self.parse_block()))
        } else {
            None
        };

        Stmt::If(IfStmt {
            condition,
            run_if_true: Box::new(run_if_true),
            run_if_false,
        })
    }


    // unlessStmt: UNLESS LEFT_PAREN expression RIGHT_PAREN block [ELSE block]
    fn parse_unless_stmt(&mut self) -> Stmt {
        let unless_line_num = self.peek().map(|t| t.line_number).unwrap_or(0);

        self.expect(TokenType::UNLESS);
        self.expect(TokenType::LEFT_PAREN);
        let condition = self.parse_expr();
        self.expect(TokenType::RIGHT_PAREN);

        let run_if_true = self.parse_block();

        let run_if_false = if self.next_token_type() == Some(TokenType::ELSE) {
            self.advance(); // consume ELSE
            Some(Box::new(self.parse_block()))
        } else {
            None
        };

        // Create synthetic '!' token
        let bang_token = Token {
            token_type: TokenType::BANG,
            lexeme: "!".to_string(),
            line_number: unless_line_num,
        };

        let negated_condition =Expr::Unary(UnaryExpr {
            operator: bang_token,
            right: Box::new(condition),
        });

        Stmt::If(IfStmt{
            condition: negated_condition,
            run_if_true: Box::new(run_if_true),
            run_if_false,
        })
    }


    // whileStmt: WHILE LEFT_PAREN expression RIGHT_PAREN block
    fn parse_while_stmt(&mut self) -> Stmt {
        self.expect(TokenType::WHILE);
        self.expect(TokenType::LEFT_PAREN);
        let condition = self.parse_expr();
        self.expect(TokenType::RIGHT_PAREN);
        let loop_body = self.parse_block();

        Stmt::While(WhileStmt{
            condition,
            body: Box::new(loop_body),
        })
    }


    // funDecl: FUN IDENTIFIER LEFT_PAREN [IDENTIFIER (COMMA IDENTIFIER)*] RIGHT_PAREN block
    fn parse_fun_decl_stmt(&mut self) -> Stmt {
        self.expect(TokenType::FUN);
        let fun_name = self.expect(TokenType::IDENTIFIER);

        self.expect(TokenType::LEFT_PAREN);
        let mut params: Vec<Token> = Vec::new();

        if self.next_token_type() != Some(TokenType::RIGHT_PAREN) {
            params.push(self.expect(TokenType::IDENTIFIER));
            while self.next_token_type() == Some(TokenType::COMMA) {
                self.advance(); // consume ','
                params.push(self.expect(TokenType::IDENTIFIER));
            }
        }

        self.expect(TokenType::RIGHT_PAREN);
        let body_stmt = self.parse_block();

        match body_stmt {
            Stmt::Block(block) => {Stmt::FunDecl(FunDeclStmt{
                fun_name,
                params,
                body: block,
            })}
            _ => panic!("Function body missing on function declaration"),
        }
    }


    fn parse_expr_stmt(&mut self) -> Stmt {
        let expr = self.parse_expr();
        self.expect(TokenType::SEMICOLON);
        Stmt::ExprStmt(ExprStmt { expr })
    }


    // statement: funDeclStmt | varDeclStmt | assignStmt | printStmt |
    //            ifStmt | unlessStmt | whileStmt | exprStmt | block
    fn parse_statement(&mut self) -> Stmt {
        match self.next_token_type() {
            Some(TokenType::FUN) => self.parse_fun_decl_stmt(),
            Some(TokenType::VAR) => self.parse_var_decl_stmt(),
            Some(TokenType::PRINT) => self.parse_print_stmt(),
            Some(TokenType::IF) => self.parse_if_stmt(),
            Some(TokenType::UNLESS) => self.parse_unless_stmt(),
            Some(TokenType::WHILE) => self.parse_while_stmt(),
            Some(TokenType::LEFT_BRACE) => self.parse_block(),

            Some(TokenType::IDENTIFIER) => {
                // Lookahead for assignment vs expression
                let is_assign = {
                    let next_index = self.current + 1;
                    next_index < self.tokens.len()
                        && self.tokens[next_index].token_type == TokenType::EQUAL
                };
                if is_assign {
                    self.parse_assign_stmt()
                } else {
                    self.parse_expr_stmt()
                }
            }

            _ => self.parse_expr_stmt(),
        }
    }
}

// Convenience function to mirror your Python parse(incoming_tokens)
pub fn parse(tokens: Vec<Token>) -> Vec<Stmt> {
    let mut parser = Parser::new(tokens);
    let mut statements: Vec<Stmt> = Vec::new();

    while !parser.is_at_end() {
        let stmt = parser.parse_statement();
        statements.push(stmt);
    }

    statements
}
