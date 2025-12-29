#ifndef AST_HPP
#define AST_HPP

#include "../lexer/lexer.hpp"
#include <memory>

struct ASTNode{
    SourceLocation loc;
    explicit ASTNode(SourceLocation loc):loc(loc){}
    virtual ~ASTNode() = default;
};

struct TypeInfo{
    std::string name;
    int pointerDepth = 0;
};

struct Expr : ASTNode{
    TypeInfo type;
    Expr(SourceLocation loc,TypeInfo type):ASTNode(loc),type(type){}
};

using ExprPtr = std::unique_ptr<Expr>;

struct Stmt : ASTNode{
    using ASTNode::ASTNode;
};

using StmtPtr = std::unique_ptr<Stmt>;

#endif