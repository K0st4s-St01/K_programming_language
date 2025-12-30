#ifndef CODEGEN_HPP
#define CODEGEN_HPP

#include "../../headers/ast/ast_base.hpp"
#include "../../headers/ast/ast_expr.hpp"
#include "../../headers/ast/ast_stmt.hpp"
#include "../../headers/ast/ast_decl.hpp"
// Core LLVM
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Verifier.h"

// Optional for constants and literals
#include "llvm/IR/Constants.h"

// Optional for writing to file / object / bitcode
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/IR/LegacyPassManager.h"

// Standard C++ includes
#include <llvm/ADT/iterator_range.h>
#include <llvm/IR/Value.h>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

class CodeGen {
public:
    explicit CodeGen(Program* prog);

    void generate();           // entry point
private:
    void generateModule(Module* mod);
    llvm::Function* generateFunction(FunctionDecl* func);
    llvm::Value* generateStmt(Stmt* stmt);

    llvm::Value* generateExpr(Expr* expr);
    llvm::Type* getLLVMType(TypeInfo t);
    llvm::Value* generateBinary(BinaryExpr* bin);
    llvm::Value* generateMemberPtr(MemberAccessExpr* mem) ;
    llvm::Value* createAlloca(llvm::Function*, llvm::Type*, const std::string&);
    llvm::Value* generateUnary(UnaryExpr*);
    llvm::Value* generateCall(CallExpr*);
    void pushScope();
    void popScope();

    void declareStruct(StructDecl* s) ;
    void defineStruct(StructDecl* s) ;
    void error(const std::string& message,SourceLocation loc);
    void emitIR();
    void emitObject();

    

private:
    Program* program;

    // LLVM context and module
    llvm::LLVMContext context;
    std::unique_ptr<llvm::Module> module;
    std::unique_ptr<llvm::IRBuilder<>> builder;

   std::vector<std::unordered_map<std::string, Symbol>> scopes;


    // Current function and symbol table
    llvm::Function* currentFunction = nullptr;
    std::unordered_map<std::string, llvm::Value*> symbolTable;

    // Struct name → LLVM struct type
    std::unordered_map<std::string, llvm::StructType*> structTypes;

    // Struct name → field name → index
    std::unordered_map<
        std::string,
        std::unordered_map<std::string, unsigned>
    > structFieldIndex;
};


#endif