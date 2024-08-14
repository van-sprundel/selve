use std::collections::HashMap;

use crate::type_checker::Type;

pub type SymbolTable = HashMap<String, Type>;

/// Holds the symbols for every scope
pub struct GlobalSymbolTable {
    scopes: Vec<SymbolTable>,
}

impl GlobalSymbolTable {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    /// Looks up a symbol in the current scope chain
    pub fn lookup(&self, name: &str) -> Option<&Type> {
        // starting from the inner scope
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }

        None
    }

    /// Inserts a symbol in the current scope
    pub fn insert(&mut self, name: &str, ty: Type) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(name.to_string(), ty);
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
        if self.scopes.is_empty() {
            // Should always be least one global scope
            self.scopes.push(HashMap::new());
        }
    }

    /// Inserts a symbol in the global scope
    pub fn insert_global(&mut self, name: &str, ty: Type) {
        if let Some(global_scope) = self.scopes.first_mut() {
            global_scope.insert(name.to_string(), ty);
        }
    }
}
