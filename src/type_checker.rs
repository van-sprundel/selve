use crate::{
    ast::{Expr, Literal, Stmt, TypedExpr},
    error::{Error, ParserError, TypedError},
    symbol_table::{GlobalSymbolTable, SymbolTable},
    token::{Token, TokenKind},
    Result,
};
use std::{collections::HashMap, string::ParseError};

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Integer,
    Float,
    Boolean,
    String,
    Void,
    Function {
        parameters: Vec<Type>,
        returns: Box<Type>,
    },
}

pub struct TypeChecker {
    symbol_table: GlobalSymbolTable,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            symbol_table: GlobalSymbolTable::new(),
        }
    }

    /// Checks all types within the given AST.
    pub fn check(&mut self, ast: &mut [Stmt]) -> Result<()> {
        for stmt in ast {
            self.check_stmt(stmt)?;
        }

        Ok(())
    }

    pub fn check_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Expr(expr) => {
                self.check_expr(&mut TypedExpr {
                    expr: expr.clone(),
                    ty: None,
                })?;

                Ok(())
            }
            Stmt::Let(token, expr) => {
                if let TokenKind::Identifier(name) = &token.kind {
                    let expr_type = if let Some(e) = expr {
                        self.check_expr(&mut TypedExpr {
                            expr: e.clone(),
                            ty: None,
                        })?
                    } else {
                        Type::Void
                    };

                    self.symbol_table.insert(name, expr_type);
                    Ok(())
                } else {
                    Err(TypedError::InvalidToken.into())
                }
            }
            Stmt::Block(statements) => {
                self.symbol_table.enter_scope();

                for stmt in statements {
                    self.check_stmt(stmt)?;
                }

                self.symbol_table.exit_scope();

                Ok(())
            }
            Stmt::If(condition, then_branch, else_branch) => {
                let condition_type = self.check_expr(&mut TypedExpr {
                    expr: condition.clone(),
                    ty: None,
                })?;
                if condition_type != Type::Boolean {
                    return Err(TypedError::TypeMismatch.into());
                }
                self.check_stmt(then_branch)?;
                if let Some(else_stmt) = else_branch {
                    self.check_stmt(else_stmt)?;
                }
                Ok(())
            }
            Stmt::FunctionDecl(name, params, body) => {
                if let TokenKind::Identifier(func_name) = &name.kind {
                    // TODO: hard-coded to integers
                    let param_types = params.iter().map(|_| Type::Integer).collect();
                    let return_type = Box::new(Type::Integer);
                    self.symbol_table.insert_global(
                        func_name,
                        Type::Function {
                            parameters: param_types,
                            returns: return_type,
                        },
                    );

                    self.symbol_table.enter_scope();

                    for param in params {
                        if let TokenKind::Identifier(param_name) = &param.kind {
                            self.symbol_table.insert(param_name, Type::Integer);
                        }
                    }

                    self.check_stmt(body)?;
                    self.symbol_table.exit_scope();

                    Ok(())
                } else {
                    Err(TypedError::InvalidToken.into())
                }
            }
            Stmt::Return(_, expr) => {
                if let Some(e) = expr {
                    self.check_expr(&mut TypedExpr {
                        expr: e.clone(),
                        ty: None,
                    })?;
                }
                Ok(())
            }
        }
    }

    pub fn check_expr(&mut self, typed_expr: &mut TypedExpr) -> Result<Type> {
        let ty = match &typed_expr.expr {
            Expr::Literal(lit) => self.check_literal(lit)?,
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let mut left_expr = TypedExpr {
                    expr: (**left).clone(),
                    ty: None,
                };
                let mut right_expr = TypedExpr {
                    expr: (**right).clone(),
                    ty: None,
                };
                let left_type = self.check_expr(&mut left_expr)?;
                let right_type = self.check_expr(&mut right_expr)?;
                self.check_binary(&left_type, operator, &right_type)?
            }
            Expr::Unary { operator, expr } => {
                let mut inner_expr = TypedExpr {
                    expr: (**expr).clone(),
                    ty: None,
                };
                let expr_type = self.check_expr(&mut inner_expr)?;
                self.check_unary(operator, &expr_type)?
            }
            Expr::Grouping(expr) => {
                let mut inner_expr = TypedExpr {
                    expr: (**expr).clone(),
                    ty: None,
                };
                self.check_expr(&mut inner_expr)?
            }
            Expr::Variable(token) => {
                if let TokenKind::Identifier(name) = &token.kind {
                    self.symbol_table
                        .lookup(name)
                        .cloned()
                        .ok_or(Error::Typed(TypedError::UndefinedSymbol))?
                } else {
                    return Err(TypedError::InvalidToken.into());
                }
            }
        };

        typed_expr.ty = Some(ty.clone());
        Ok(ty)
    }

    fn check_literal(&self, lit: &Literal) -> Result<Type> {
        match lit {
            Literal::Integer(_) => Ok(Type::Integer),
            Literal::Float(_) => Ok(Type::Float),
            Literal::String(_) => Ok(Type::String),
            Literal::Boolean(_) => Ok(Type::Boolean),
            Literal::Nil => Ok(Type::Void),
        }
    }

    fn check_binary(&self, left_type: &Type, operator: &Token, right_type: &Type) -> Result<Type> {
        match operator.kind {
            TokenKind::Plus | TokenKind::Minus | TokenKind::Asterisk | TokenKind::Slash => {
                if left_type == &Type::Integer && right_type == &Type::Integer {
                    Ok(Type::Integer)
                } else if left_type == &Type::Float && right_type == &Type::Float {
                    Ok(Type::Float)
                } else {
                    Err(TypedError::TypeMismatch.into())
                }
            }
            TokenKind::EqualsEquals | TokenKind::BangEquals => {
                if left_type == right_type {
                    Ok(Type::Boolean)
                } else {
                    Err(TypedError::TypeMismatch.into())
                }
            }
            _ => Err(TypedError::InvalidOperator.into()),
        }
    }

    fn check_unary(&self, operator: &Token, expr_type: &Type) -> Result<Type> {
        match operator.kind {
            TokenKind::Minus => {
                if expr_type == &Type::Integer || expr_type == &Type::Float {
                    Ok(expr_type.clone())
                } else {
                    Err(TypedError::TypeMismatch.into())
                }
            }
            TokenKind::Bang => {
                if expr_type == &Type::Boolean {
                    Ok(Type::Boolean)
                } else {
                    Err(TypedError::TypeMismatch.into())
                }
            }
            _ => Err(TypedError::InvalidOperator.into()),
        }
    }

    fn check_call(&mut self, callee: &mut TypedExpr, arguments: &mut [TypedExpr]) -> Result<Type> {
        let callee_type = self.check_expr(callee)?;

        if let Type::Function {
            parameters: param_types,
            returns: return_type,
        } = callee_type
        {
            if param_types.len() != arguments.len() {
                return Err(TypedError::ArgumentMismatch.into());
            }

            for (param_type, arg) in param_types.iter().zip(arguments.iter_mut()) {
                let arg_type = self.check_expr(arg)?;
                if &arg_type != param_type {
                    return Err(TypedError::TypeMismatch.into());
                }
            }

            Ok(*return_type)
        } else {
            Err(TypedError::NotCallable.into())
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::{Expr, Literal, Stmt};
    use crate::token::{Token, TokenKind};

    #[test]
    fn test_integer_literal() {
        let mut checker = TypeChecker::new();
        let mut expr = TypedExpr::new(Expr::Literal(Literal::Integer(5)), None);

        let result = checker.check_expr(&mut expr);
        assert_eq!(result, Ok(Type::Integer));
        assert_eq!(expr.ty, Some(Type::Integer));
    }

    #[test]
    fn test_float_literal() {
        let mut checker = TypeChecker::new();
        let mut expr = TypedExpr::new(Expr::Literal(Literal::Float(3.14)), None);

        let result = checker.check_expr(&mut expr);
        assert_eq!(result, Ok(Type::Float));
        assert_eq!(expr.ty, Some(Type::Float));
    }

    #[test]
    fn test_boolean_literal() {
        let mut checker = TypeChecker::new();
        let mut expr = TypedExpr::new(Expr::Literal(Literal::Boolean(true)), None);

        let result = checker.check_expr(&mut expr);
        assert_eq!(result, Ok(Type::Boolean));
        assert_eq!(expr.ty, Some(Type::Boolean));
    }

    #[test]
    fn test_string_literal() {
        let mut checker = TypeChecker::new();
        let mut expr = TypedExpr::new(Expr::Literal(Literal::String("hello".to_string())), None);

        let result = checker.check_expr(&mut expr);
        assert_eq!(result, Ok(Type::String));
        assert_eq!(expr.ty, Some(Type::String));
    }

    #[test]
    fn test_nil_literal() {
        let mut checker = TypeChecker::new();
        let mut expr = TypedExpr::new(Expr::Literal(Literal::Nil), None);

        let result = checker.check_expr(&mut expr);
        assert_eq!(result, Ok(Type::Void));
        assert_eq!(expr.ty, Some(Type::Void));
    }

    #[test]
    fn test_binary_expr_addition() {
        let mut checker = TypeChecker::new();
        let left = Box::new(Expr::Literal(Literal::Integer(5)));
        let right = Box::new(Expr::Literal(Literal::Integer(3)));
        let operator = Token::new(TokenKind::Plus, 0, 0);

        let mut expr = TypedExpr::new(
            Expr::Binary {
                left,
                operator,
                right,
            },
            None,
        );

        let result = checker.check_expr(&mut expr);
        assert_eq!(result, Ok(Type::Integer));
        assert_eq!(expr.ty, Some(Type::Integer));
    }

    #[test]
    fn test_binary_expr_comparison() {
        let mut checker = TypeChecker::new();
        let left = Box::new(Expr::Literal(Literal::Integer(5)));
        let right = Box::new(Expr::Literal(Literal::Integer(3)));
        let operator = Token::new(TokenKind::EqualsEquals, 0, 0);

        let mut expr = TypedExpr::new(
            Expr::Binary {
                left,
                operator,
                right,
            },
            None,
        );

        let result = checker.check_expr(&mut expr);
        assert_eq!(result, Ok(Type::Boolean));
        assert_eq!(expr.ty, Some(Type::Boolean));
    }

    #[test]
    fn test_unary_expr_negation() {
        let mut checker = TypeChecker::new();
        let inner = Box::new(Expr::Literal(Literal::Integer(5)));
        let operator = Token::new(TokenKind::Minus, 0, 0);

        let mut expr = TypedExpr::new(
            Expr::Unary {
                operator,
                expr: inner,
            },
            None,
        );

        let result = checker.check_expr(&mut expr);
        assert_eq!(result, Ok(Type::Integer));
        assert_eq!(expr.ty, Some(Type::Integer));
    }

    #[test]
    fn test_unary_expr_logical_not() {
        let mut checker = TypeChecker::new();
        let inner = Box::new(Expr::Literal(Literal::Boolean(true)));
        let operator = Token::new(TokenKind::Bang, 0, 0);

        let mut expr = TypedExpr::new(
            Expr::Unary {
                operator,
                expr: inner,
            },
            None,
        );

        let result = checker.check_expr(&mut expr);
        assert_eq!(result, Ok(Type::Boolean));
        assert_eq!(expr.ty, Some(Type::Boolean));
    }

    #[test]
    fn test_grouping_expr() {
        let mut checker = TypeChecker::new();
        let inner = Box::new(Expr::Literal(Literal::Integer(5)));
        let mut expr = TypedExpr::new(Expr::Grouping(inner), None);

        let result = checker.check_expr(&mut expr);
        assert_eq!(result, Ok(Type::Integer));
        assert_eq!(expr.ty, Some(Type::Integer));
    }

    #[test]
    fn test_type_mismatch_binary() {
        let mut checker = TypeChecker::new();
        let left = Box::new(Expr::Literal(Literal::Integer(5)));
        let right = Box::new(Expr::Literal(Literal::Boolean(true)));
        let operator = Token::new(TokenKind::Plus, 0, 0);

        let mut expr = TypedExpr::new(
            Expr::Binary {
                left,
                operator,
                right,
            },
            None,
        );

        let result = checker.check_expr(&mut expr);
        assert!(matches!(result, Err(_)));
    }

    #[test]
    fn test_invalid_unary_operator() {
        let mut checker = TypeChecker::new();
        let inner = Box::new(Expr::Literal(Literal::Integer(5)));
        let operator = Token::new(TokenKind::Plus, 0, 0); // not a valid unary operator

        let mut expr = TypedExpr::new(
            Expr::Unary {
                operator,
                expr: inner,
            },
            None,
        );

        let result = checker.check_expr(&mut expr);
        assert!(matches!(result, Err(_)));
    }

    // Symbol table tests

    #[test]
    fn test_variable_declaration_and_lookup() {
        let mut checker = TypeChecker::new();
        let token = Token::new(TokenKind::Identifier("x".to_string()), 0, 0);
        let expr = Expr::Literal(Literal::Integer(5));

        // We simulate checking a let statement
        let stmt = Stmt::Let(token, Some(expr));
        let result = checker.check_stmt(&stmt);
        assert!(result.is_ok());

        let var_type = checker.symbol_table.lookup("x");
        assert_eq!(var_type, Some(&Type::Integer));
    }

    #[test]
    fn test_variable_scoping() {
        let mut checker = TypeChecker::new();

        let outer_stmt = Stmt::Let(
            Token::new(TokenKind::Identifier("x".to_string()), 0, 0),
            Some(Expr::Literal(Literal::Integer(5))),
        );
        checker.check_stmt(&outer_stmt).unwrap();

        checker.symbol_table.enter_scope();

        let inner_stmt = Stmt::Let(
            Token::new(TokenKind::Identifier("x".to_string()), 0, 0),
            Some(Expr::Literal(Literal::Boolean(true))),
        );
        checker.check_stmt(&inner_stmt).unwrap();

        assert_eq!(checker.symbol_table.lookup("x"), Some(&Type::Boolean));

        checker.symbol_table.exit_scope();

        assert_eq!(checker.symbol_table.lookup("x"), Some(&Type::Integer));
    }

    #[test]
    fn test_undefined_variable() {
        let mut checker = TypeChecker::new();

        let define_stmt = Stmt::Let(
            Token::new(TokenKind::Identifier("x".to_string()), 0, 0),
            Some(Expr::Literal(Literal::Integer(5))),
        );
        checker.check_stmt(&define_stmt).unwrap();

        // this should work
        let defined_var = Expr::Variable(Token::new(TokenKind::Identifier("x".to_string()), 0, 0));
        let mut defined_expr = TypedExpr::new(defined_var, None);

        let undefined_var =
            Expr::Variable(Token::new(TokenKind::Identifier("y".to_string()), 0, 0));
        let mut undefined_expr = TypedExpr::new(undefined_var, None);

        assert!(checker.check_expr(&mut undefined_expr).is_err());
    }

    #[test]
    fn test_variable_shadowing() {
        let mut checker = TypeChecker::new();

        // Define 'x' in outer scope
        let outer_stmt = Stmt::Let(
            Token::new(TokenKind::Identifier("x".to_string()), 0, 0),
            Some(Expr::Literal(Literal::Integer(5))),
        );
        checker.check_stmt(&outer_stmt).unwrap();

        checker.symbol_table.enter_scope();

        // Define 'x' in inner scope
        let inner_stmt = Stmt::Let(
            Token::new(TokenKind::Identifier("x".to_string()), 0, 0),
            Some(Expr::Literal(Literal::Boolean(true))),
        );
        checker.check_stmt(&inner_stmt).unwrap();

        // Use 'x' in inner scope
        let inner_var = Expr::Variable(Token::new(TokenKind::Identifier("x".to_string()), 0, 0));
        let mut inner_expr = TypedExpr::new(inner_var, None);
        let inner_result = checker.check_expr(&mut inner_expr);
        assert_eq!(inner_result, Ok(Type::Boolean));

        checker.symbol_table.exit_scope();

        // Use 'x' in outer scope
        let outer_var = Expr::Variable(Token::new(TokenKind::Identifier("x".to_string()), 0, 0));
        let mut outer_expr = TypedExpr::new(outer_var, None);
        let outer_result = checker.check_expr(&mut outer_expr);
        assert_eq!(outer_result, Ok(Type::Integer));
    }
}
