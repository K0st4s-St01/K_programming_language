#ifndef DECL_H
#define DECL_H

#include "ast_base.hpp"
#include "ast_stmt.hpp"
#include <memory>
#include <string>
#include <vector>


struct Param {
    TypeInfo t;
    std::string name;
};
struct Decl : ASTNode {
    using ASTNode::ASTNode;
};

struct FunctionDecl : Decl {
    TypeInfo type;
    std::string name;
    std::vector<Param> params;
    std::unique_ptr<BlockStmt> body;
    bool isMethod;
    std::string structName;
 FunctionDecl(SourceLocation loc) : Decl(loc) {}
FunctionDecl(TypeInfo type,
             std::string name,
             std::vector<Param> params,
             std::unique_ptr<BlockStmt> body,
             SourceLocation loc,
             bool isMethod = false,
             std::string structName = "")
    : Decl(loc),
      type(type),
      name(std::move(name)),
      params(std::move(params)),
      body(std::move(body)),
      isMethod(isMethod),
      structName(std::move(structName)) {}
};

// =======================
// Struct Declaration
// =======================

struct StructField {
    TypeInfo type;
    std::string name;
};

struct StructDecl : Decl {
    std::string name;
    std::vector<StructField> fields;
    std::vector<std::unique_ptr<FunctionDecl>> methods; // new
StructDecl(SourceLocation loc) : Decl(loc) {}
    StructDecl(std::string name,
               std::vector<StructField> fields,
               std::vector<std::unique_ptr<FunctionDecl>> methods,
               SourceLocation loc)
        : Decl(loc),
          name(std::move(name)),
          fields(std::move(fields)),
          methods(std::move(methods)) {}
};
struct ModuleDecl : Decl{
    std::string name;

    ModuleDecl(std::string name, SourceLocation loc)
        : Decl(loc), name(std::move(name)) {}
};

struct ImportDecl;
struct Module {
    std::string name;
    SourceLocation loc;

    std::vector<std::unique_ptr<ImportDecl>> imports;
    std::vector<std::unique_ptr<Decl>> declarations;

    std::unordered_map<std::string, Decl*> publicSymbols;
};

struct ImportDecl : Decl {
    std::string moduleName;   // "math"

    // Filled during semantic analysis
    Module* resolvedModule = nullptr;

    ImportDecl(std::string moduleName,
               SourceLocation loc)
        : Decl(loc),
          moduleName(std::move(moduleName)) {}
};


struct Program {
    std::vector<std::unique_ptr<Module>> modules;
    Module* mainModule = nullptr;  // convenience pointer

    // Optional global compiler state
    // SymbolTable globalSymbols;
    // Diagnostics diagnostics;

    void setMainModule(Module* mod) {
        mainModule = mod;
    }
};


#endif