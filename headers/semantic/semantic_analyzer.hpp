#ifndef SEMANTIC_HPP
#define SEMANTIC_HPP
#include "../../headers/ast/ast_base.hpp"
#include "../../headers/ast/ast_expr.hpp"
#include "../../headers/ast/ast_stmt.hpp"
#include "../../headers/ast/ast_decl.hpp"
class SemanticAnalyzer {
public:
    explicit SemanticAnalyzer(Program* program)
        : program(program) {}

    void analyze();

private:
    Program* program;
    int loopDepth = 0;


    Module* currentModule = nullptr;
    StructDecl* currentStruct = nullptr;
    FunctionDecl* currentFunction = nullptr;

    StructDecl* lookupStruct(const std::string& name);
    FunctionDecl* lookupFunction(const std::string& name);
    FunctionDecl* lookupMethod(StructDecl* st, const std::string& name);


    // scopes
    std::vector<std::unordered_map<std::string, Symbol>> scopes;

    void pushScope();
    void popScope();
    void declare(const std::string& name, Symbol sym);
    Symbol* resolve(const std::string& name);

    // passes
    void analyzeModule(Module* mod);
    void analyzeStruct(StructDecl* s);
    void analyzeFunction(FunctionDecl* f);
    void analyzeStmt(Stmt* stmt);
    TypeInfo analyzeExpr(Expr* expr);

    void error(const std::string& msg, SourceLocation loc);
};
#endif