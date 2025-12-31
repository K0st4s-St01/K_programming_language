#ifndef PARSER_HPP
#define PARSER_HPP
#include <cstddef>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>
#include "../ast/ast_base.hpp";
#include "../ast/ast_expr.hpp";
#include "../ast/ast_decl.hpp";
#include "../ast/ast_stmt.hpp";

class Parser{
    public:
        explicit Parser(std::vector<std::string> files);
        std::unique_ptr<Program> parseProgram();
         
    private:
        std::unordered_map<std::string, Module*> moduleMap;
        std::unique_ptr<Module> parseModuleFile();
        std::unique_ptr<ModuleDecl> parseModuleDecl();
        std::unique_ptr<ImportDecl> parseImportDecl();
        std::unique_ptr<FunctionDecl> parseFunctionDecl();
        std::unique_ptr<StructDecl> parseStructDecl();
        std::vector<Param> parseParamList();
        TypeInfo parseType();
        
        std::unique_ptr<Stmt> parseStatement();
        std::unique_ptr<BlockStmt> parseBlock();
        std::unique_ptr<Stmt> parseIf();
        std::unique_ptr<Stmt> parseWhile();
        std::unique_ptr<Stmt> parseFor();
        std::unique_ptr<Stmt> parseReturn();
        std::unique_ptr<Stmt> parseVarDecl();
        StmtPtr parseBreak();
        StmtPtr parseContinue();
        StmtPtr parseExpressionStmt();

        ExprPtr parseExpression();
        ExprPtr parseAssignment();
        ExprPtr parseTernary();
        ExprPtr parseOr();
        ExprPtr parseAnd();
        ExprPtr parseEquality();
        ExprPtr parseComparison();
        ExprPtr parseTerm();
        ExprPtr parseFactor();
        ExprPtr parseUnary();
        ExprPtr parsePrimary();
        ExprPtr parseCall(ExprPtr callee);
        
    private:
        bool isType(const Token& tok);
        bool isMethodDeclaration();
        std::vector<std::string> srcFiles;
        Token currentToken = Token(TokenType::Invalid,"",0,0);
        Token previousToken = Token(TokenType::Invalid,"",0,0);
        Lexer* currentLexer = nullptr;
        bool hadError = false;

        Token peek(size_t n = 1);
        std::vector<std::unique_ptr<Module>> modules;
        
        Token consume(Lexer* lexer,TokenType type, const std::string& errMsg);
        std::vector<Token> lookaheadBuffer;

        void advance();
        bool match(TokenType type);
        bool check(TokenType type);
        void synchronize();
        void error(const std::string& message);
        Token consume(TokenType type, const std::string& errMsg);

};

#endif