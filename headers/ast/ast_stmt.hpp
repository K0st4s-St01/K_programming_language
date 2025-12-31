#ifndef STMT_H
#define STMT_H

#include "ast_base.hpp"
#include <memory>
#include <utility>
#include <vector>

// =======================
// Expression Statement
// =======================

struct ExprStmt : Stmt {
    ExprPtr expr;
    ExprStmt(ExprPtr e, SourceLocation loc)
        : Stmt(loc), expr(std::move(e)) {}
};

// =======================
// Variable Declaration
// =======================

struct VarDeclStmt : Stmt {
    std::string typeName;   // "i32", "char", struct name, etc.
    int pointerDepth;       // char** -> 2
    std::string name;
    ExprPtr initializer;    // may be null

    VarDeclStmt(std::string type,
                int ptrDepth,
                std::string name,
                ExprPtr init,
                SourceLocation loc)
        : Stmt(loc),
          typeName(std::move(type)),
          pointerDepth(ptrDepth),
          name(std::move(name)),
          initializer(std::move(init)) {}
};

// =======================
// Return Statement
// =======================

struct ReturnStmt : Stmt {
    ExprPtr value; // may be null

    ReturnStmt(ExprPtr v, SourceLocation loc)
        : Stmt(loc), value(std::move(v)) {}
};

// =======================
// Block Statement
// =======================

struct BlockStmt : Stmt {
    std::vector<StmtPtr> statements;

    explicit BlockStmt(SourceLocation loc,std::vector<StmtPtr> statements)
        : Stmt(loc) , statements(std::move(statements)) {}
};

// =======================
// If Statement
// =======================
struct ElifClause {
    ExprPtr condition;
    StmtPtr body;
};
struct IfStmt : Stmt {
    ExprPtr condition;
    StmtPtr thenBranch;
    std::vector<ElifClause> elifClauses;
    StmtPtr elseBranch; // may be null

    // IfStmt(ExprPtr cond,
    //        StmtPtr thenB,
    //        StmtPtr elseB,
    //        SourceLocation loc)
    //     : Stmt(loc),
    //       condition(std::move(cond)),
    //       thenBranch(std::move(thenB)),
    //       elseBranch(std::move(elseB)) {}
    IfStmt(ExprPtr cond,
           StmtPtr thenB,
           StmtPtr elseB,
           std::vector<ElifClause> elifClauses,
           SourceLocation loc)
        : Stmt(loc),
          condition(std::move(cond)),
          thenBranch(std::move(thenB)),
          elseBranch(std::move(elseB)),
          elifClauses(std::move(elifClauses)) {}

};

// =======================
// While Statement
// =======================

struct WhileStmt : Stmt {
    ExprPtr condition;
    StmtPtr body;

    WhileStmt(ExprPtr cond, StmtPtr body, SourceLocation loc)
        : Stmt(loc),
          condition(std::move(cond)),
          body(std::move(body)) {}
};
struct ForStmt : Stmt {
    std::unique_ptr<Stmt> initializer;   // e.g., var declaration or assignment
    std::unique_ptr<Expr> condition;     // loop condition
    std::unique_ptr<Expr> increment;     // e.g., i = i + 1
    std::unique_ptr<Stmt> body;  
    ForStmt(StmtPtr init,ExprPtr cond,ExprPtr increment,StmtPtr body,SourceLocation loc)
    :initializer(std::move(init)),condition(std::move(cond)),increment(std::move(increment)),body(std::move(body)),Stmt(loc){}
};
struct BreakStmt : Stmt {
    BreakStmt(SourceLocation loc):Stmt(loc){}
};

struct ContinueStmt : Stmt {
    ContinueStmt(SourceLocation loc):Stmt(loc){}
};

#endif