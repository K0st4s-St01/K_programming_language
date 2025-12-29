#ifndef AST_EXPR_HPP
#define AST_EXPR_HPP
#include "ast_base.hpp"
#include <string>
#include <utility>
#include <vector>
struct IntLiteralExpr:Expr{
    long long value;
    IntLiteralExpr(long long v,SourceLocation loc,std::string bytes) : Expr(loc,{"i"+bytes,0}),value(v){}
};
struct FloatLiteralExpr:Expr{
    double value;
    FloatLiteralExpr(double v,SourceLocation loc,std::string bytes) : Expr(loc,{"f"+bytes,0}),value(v){}
};
struct CharLiteralExpr : Expr{
    char value;
    CharLiteralExpr(char v,SourceLocation loc):Expr(loc, {"char",0} ),value(v){}
};
struct BoolLiteralExpr : Expr{
    bool value;
    BoolLiteralExpr(bool v,SourceLocation loc):Expr(loc,{"bool",0}),value(v){}
};

struct StringLiteralExpr : Expr{
    std::string value;
    StringLiteralExpr(std::string v,SourceLocation loc):Expr(loc,{"char",1}),value(std::move(v)){}
};
struct IdentifierExpr : Expr{
    std::string name;
    IdentifierExpr(std::string n,SourceLocation loc,TypeInfo type):Expr(loc,type),name(std::move(n)){}
};

enum class UnaryOp{
    Negative,
    Not,
    Dereference,
    AddressOf,
};

enum class BinaryOp {
    Add, Sub, Mul, Div,
    Equal, NotEqual,
    Less, LessEqual,
    Greater, GreaterEqual,
    And, Or,Xor,
    Assign
};

struct BinaryExpr : Expr {
    BinaryOp op;
    ExprPtr lhs;
    ExprPtr rhs;

    BinaryExpr(BinaryOp op, ExprPtr lhs, ExprPtr rhs, SourceLocation loc)
        : Expr(loc,{"none",0}), op(op),
          lhs(std::move(lhs)),
          rhs(std::move(rhs)) {}
};

struct UnaryExpr : Expr{
    UnaryOp op;
    ExprPtr operand;
    UnaryExpr(UnaryOp op,ExprPtr operand,SourceLocation loc,TypeInfo type) : Expr(loc,type), op(op), operand(std::move(operand)){}
};
struct TernaryExpr : Expr{
    ExprPtr condition;
    ExprPtr thenExpr;
    ExprPtr elseExpr;

    TernaryExpr(ExprPtr condition,ExprPtr thenExpr,ExprPtr elseExpr,SourceLocation source):Expr(loc,TypeInfo{"none",0}),condition(std::move(condition)),thenExpr(std::move(thenExpr)),elseExpr(std::move(elseExpr)){}
};
// ---------------- Call ----------------
struct CallExpr : Expr {
    ExprPtr callee;
    std::vector<ExprPtr> arguments;

    CallExpr(ExprPtr callee,
             std::vector<ExprPtr> args,
             SourceLocation loc,TypeInfo type)
        : Expr(loc,type),
          callee(std::move(callee)),
          arguments(std::move(args)) {}
};

// ---------------- Member Access ----------------
struct MemberExpr : Expr {
    ExprPtr object;
    std::string memberName;

    MemberExpr(ExprPtr obj, std::string mem, SourceLocation loc,TypeInfo type)
        : Expr(loc,type),
          object(std::move(obj)),
          memberName(std::move(mem)) {}
};

// ---------------- Logical ----------------
enum class LogicalOp { And, Or, Not };

struct LogicalExpr : Expr {
    LogicalOp op;
    ExprPtr lhs; // null for Not
    ExprPtr rhs;

    LogicalExpr(LogicalOp op, ExprPtr lhs, ExprPtr rhs, SourceLocation loc,TypeInfo type)
        : Expr(loc,type), op(op),
          lhs(std::move(lhs)),
          rhs(std::move(rhs)) {}
};
struct AssignExpr : Expr {
    std::unique_ptr<Expr> target; // usually an IdentifierExpr
    std::unique_ptr<Expr> value;

    AssignExpr(std::unique_ptr<Expr> tgt, std::unique_ptr<Expr> val, SourceLocation loc)
        : Expr(loc, tgt->type), target(std::move(tgt)), value(std::move(val)) {}
};

#endif