use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicValueEnum, FunctionValue, PointerValue},
};
use std::collections::HashMap;

use crate::{
    ast::{Expr, Literal, Stmt},
    token::{Token, TokenKind},
};

pub fn compile(ast: &[Stmt]) {
    let context = Context::create();
    let module = context.create_module("repl");
    let builder = context.create_builder();

    let mut codegen = CodeGen::new(&context, &module, &builder);
    codegen.generate(ast);

    println!("{}", module.print_to_string().to_string());
}

pub struct CodeGen<'ctx, 'a> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    variables: HashMap<String, PointerValue<'ctx>>,
    current_function: Option<FunctionValue<'ctx>>,
}

impl<'ctx, 'a> CodeGen<'ctx, 'a> {
    pub fn new(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
    ) -> Self {
        CodeGen {
            context,
            module,
            builder,
            variables: HashMap::new(),
            current_function: None,
        }
    }

    pub fn generate(&mut self, ast: &[Stmt]) {
        // main
        let i32_type = self.context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        self.current_function = Some(function);

        for stmt in ast {
            self.generate_statement(stmt);
        }

        // func return
        if basic_block.get_terminator().is_none() {
            self.builder
                .build_return(Some(&i32_type.const_int(0, false)));
        }
    }

    fn generate_statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let(token, initializer) => self.generate_let_statement(token, initializer),
            Stmt::FunctionDecl(token, params, body) => {
                self.generate_function_declaration(token, params, body)
            }
            Stmt::If(condition, then_branch, else_branch) => {
                self.generate_if_statement(condition, then_branch, else_branch)
            }
            Stmt::Return(_, value) => self.generate_return_statement(value),
            Stmt::Expr(expr) => {
                self.generate_expression(expr);
            }
            Stmt::Block(statements) => self.generate_block(statements),
        }
    }

    fn generate_block(&mut self, statements: &[Stmt]) {
        for stmt in statements {
            self.generate_statement(stmt);
        }
    }

    fn generate_let_statement(&mut self, token: &Token, initializer: &Option<Expr>) {
        if let TokenKind::Identifier(name) = &token.kind {
            let alloca = self
                .builder
                .build_alloca(self.context.i32_type(), name)
                .expect("Couldnt build alloca");
            self.variables.insert(name.clone(), alloca);

            if let Some(init) = initializer {
                let init_value = self.generate_expression(init);
                self.builder.build_store(alloca, init_value);
            }
        }
    }

    fn generate_if_statement(
        &mut self,
        condition: &Expr,
        then_branch: &Stmt,
        else_branch: &Option<Box<Stmt>>,
    ) {
        let condition_value = self.generate_expression(condition);
        let zero = self.context.i32_type().const_int(0, false);
        let comparison = self
            .builder
            .build_int_compare(
                inkwell::IntPredicate::NE,
                condition_value.into_int_value(),
                zero,
                "ifcond",
            )
            .expect("Couldnt build comparison");

        let function = self.current_function.unwrap();
        let then_block = self.context.append_basic_block(function, "then");
        let else_block = self.context.append_basic_block(function, "else");
        let merge_block = self.context.append_basic_block(function, "ifcont");

        self.builder
            .build_conditional_branch(comparison, then_block, else_block);

        self.builder.position_at_end(then_block);
        self.generate_statement(then_branch);
        self.builder.build_unconditional_branch(merge_block);

        self.builder.position_at_end(else_block);
        if let Some(else_stmt) = else_branch {
            self.generate_statement(else_stmt);
        }
        self.builder.build_unconditional_branch(merge_block);

        self.builder.position_at_end(merge_block);
    }

    fn generate_function_declaration(&mut self, token: &Token, params: &[Token], body: &Stmt) {
        if let TokenKind::Identifier(name) = &token.kind {
            let param_types = vec![self.context.i32_type().into(); params.len()];
            let function_type = self.context.i32_type().fn_type(&param_types, false);
            let function = self.module.add_function(name, function_type, None);

            let basic_block = self.context.append_basic_block(function, "entry");
            self.builder.position_at_end(basic_block);

            let previous_function = self.current_function;
            self.current_function = Some(function);

            let mut previous_variables = HashMap::new();
            std::mem::swap(&mut self.variables, &mut previous_variables);

            for (i, param) in params.iter().enumerate() {
                if let TokenKind::Identifier(param_name) = &param.kind {
                    let alloca = self
                        .builder
                        .build_alloca(self.context.i32_type(), param_name)
                        .expect("Couldnt build alloca");
                    self.builder
                        .build_store(alloca, function.get_nth_param(i as u32).unwrap());
                    self.variables.insert(param_name.clone(), alloca);
                }
            }

            self.generate_statement(body);

            if basic_block.get_terminator().is_none() {
                self.builder
                    .build_return(Some(&self.context.i32_type().const_int(0, false)));
            }

            self.current_function = previous_function;
            self.variables = previous_variables;
        }
    }

    fn generate_return_statement(&mut self, value: &Option<Expr>) {
        if let Some(expr) = value {
            let return_value = self.generate_expression(expr);
            self.builder.build_return(Some(&return_value));
        } else {
            self.builder.build_return(None);
        }
    }

    fn generate_expression(&mut self, expr: &Expr) -> BasicValueEnum<'ctx> {
        match expr {
            Expr::Literal(literal) => self.generate_literal(literal),
            Expr::Variable(token) => self.generate_variable(token),
            Expr::Binary {
                left,
                operator,
                right,
            } => self.generate_binary_expression(left, right, operator),
            Expr::Unary { operator, expr } => self.generate_unary_expression(operator, expr),
            Expr::Grouping(expr) => self.generate_expression(expr),
        }
    }

    fn generate_binary_expression(
        &mut self,
        left: &Expr,
        right: &Expr,
        operator: &Token,
    ) -> BasicValueEnum<'ctx> {
        let lhs = self.generate_expression(left);
        let rhs = self.generate_expression(right);

        match operator.kind {
            TokenKind::Plus => {
                self.builder
                    .build_int_add(lhs.into_int_value(), rhs.into_int_value(), "addtmp")
            }
            TokenKind::Minus => {
                self.builder
                    .build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "subtmp")
            }
            TokenKind::Asterisk => {
                self.builder
                    .build_int_mul(lhs.into_int_value(), rhs.into_int_value(), "multmp")
            }
            TokenKind::Slash => self.builder.build_int_signed_div(
                lhs.into_int_value(),
                rhs.into_int_value(),
                "divtmp",
            ),
            _ => panic!("Unsupported binary operator"),
        }
        .expect("Couldnt build binary expr")
        .into()
    }

    fn generate_unary_expression(&mut self, operator: &Token, expr: &Expr) -> BasicValueEnum<'ctx> {
        let operand = self.generate_expression(expr);

        match operator.kind {
            TokenKind::Minus => self
                .builder
                .build_int_neg(operand.into_int_value(), "negtmp"),
            TokenKind::Bang => {
                let zero = self.context.i32_type().const_int(0, false);
                self.builder.build_int_compare(
                    inkwell::IntPredicate::EQ,
                    operand.into_int_value(),
                    zero,
                    "nottmp",
                )
            }
            _ => panic!("Unsupported unary operator"),
        }
        .expect("Couldnt build unary expr")
        .into()
    }

    fn generate_literal(&self, literal: &Literal) -> BasicValueEnum<'ctx> {
        match literal {
            Literal::Integer(value) => self
                .context
                .i32_type()
                .const_int(*value as u64, false)
                .into(),
            Literal::Float(value) => self.context.f64_type().const_float(*value).into(),
            _ => unimplemented!(),
        }
    }

    fn generate_variable(&self, token: &Token) -> BasicValueEnum<'ctx> {
        if let TokenKind::Identifier(name) = &token.kind {
            if let Some(alloca) = self.variables.get(name) {
                let pointee_type = alloca.get_type();

                self.builder
                    .build_load(pointee_type, *alloca, name)
                    .expect("Failed to build load")
            } else {
                panic!("Undefined variable: {}", name);
            }
        } else {
            panic!("Expected identifier token");
        }
    }
}
