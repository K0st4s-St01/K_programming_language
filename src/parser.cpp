#include "../headers/parser/parser.hpp"
#include <algorithm>
#include <exception>
#include <fstream>
#include <iostream>
#include <iterator>
#include <memory>
#include <stdexcept>
#include <utility>
#include <vector>

Parser::Parser(std::vector<std::string> src_files){
    this->srcFiles = std::move(src_files);
}
void Parser::error(const std::string& message) {
    std::cerr << "[Syntax Error] "
              << currentToken.location.to_string()
              << ": "
              << message
              << "\n";

    hadError = true;
}
void Parser::synchronize() {
    while (!currentLexer->ended()) {
        if (currentToken.type == TokenType::Semicolon ||
            currentToken.type == TokenType::RBrace)
            return;
        advance();
    }
}

void Parser::advance() {
    previousToken = currentToken;

    if (!lookaheadBuffer.empty()) {
        currentToken = lookaheadBuffer.front();
        lookaheadBuffer.erase(lookaheadBuffer.begin());
    } else {
        currentToken = currentLexer->nextToken();
    }
}
Token Parser::peek(size_t n) {
    while (lookaheadBuffer.size() < n) {
        lookaheadBuffer.push_back(currentLexer->nextToken());
    }
    return lookaheadBuffer[n - 1];
}

bool Parser::match(TokenType type) {
    if (currentToken.type == type) {
        advance();
        return true;
    }
    return false;
}

bool Parser::check(TokenType type) {
    return currentToken.type == type;
}

Token Parser::consume(TokenType type, const std::string& errMsg) {
    if (check(type)) {
        Token t = currentToken;
        advance();
        return t;
    }
    error(errMsg + " at line " + std::to_string(currentToken.location.line));
    synchronize();
    return currentToken;
}

bool Parser::isType(const Token& tok) {
    switch (tok.type) {
        case TokenType::I16:
        case TokenType::I32:
        case TokenType::I64:
        case TokenType::F16:
        case TokenType::F32:
        case TokenType::F64:
        case TokenType::Char:
        case TokenType::Boolean:
        case TokenType::Void:
        case TokenType::Identifier:
            return true;
        default:
            return false;
    }
}


std::unique_ptr<Program> Parser::parseProgram(){
    advance();
    for(const auto& path :this->srcFiles){
        std::ifstream file(path);
        if(!file.is_open()){
            error("Failed to open source file: "+ path);
        }
        std::string source((std::istreambuf_iterator<char>(file)),std::istreambuf_iterator<char>());
        Lexer lexer(source);

        this->currentLexer = &lexer;
        auto module = parseModuleFile();
        moduleMap[module->name] = module.get();
        modules.push_back(std::move(module));
    }
    auto program = std::make_unique<Program>();
    for (auto& m : modules) {
        if (m->name == "main") {
            program->mainModule = m.get();
        }
        program->modules.push_back(std::move(m));
    }

    return program;
}

std::unique_ptr<Module> Parser::parseModuleFile(){
    auto moduleDecl = parseModuleDecl();
    auto module = std::make_unique<Module>();
    module->name = moduleDecl->name;
    module-> loc = moduleDecl->loc;

    while (check(TokenType::Include)) {
        module->imports.push_back(parseImportDecl());
    }
    while(!currentLexer->ended()){
        if(check(TokenType::Struct)){
            module->declarations.push_back(parseStructDecl());
        } else if(isType(currentToken)){
            module->declarations.push_back(parseFunctionDecl());
        } else{
            error("Invalid top-level declaration "+currentToken.location.to_string()); 
            synchronize();
        }
    }
    return module;
}

std::unique_ptr<ModuleDecl> Parser::parseModuleDecl(){
    auto loc = currentToken.location;
    consume(TokenType::Module,"Expected module definition");
    std::string name = consume(TokenType::Identifier,"Expected module name").lexeme;
    consume(TokenType::Semicolon,"Expected ';'");
    return std::make_unique<ModuleDecl>(name,loc);
}

std::unique_ptr<ImportDecl> Parser::parseImportDecl(){
    auto loc = currentToken.location;
    consume(TokenType::Include,"Expected 'include'");
    std::string name = consume(TokenType::Identifier,"Expected module name").lexeme;
    consume(TokenType::Semicolon,"Expected ';'");
    return std::make_unique<ImportDecl>(name,loc);
}

TypeInfo Parser::parseType(){
    TypeInfo t;
    if(!isType(currentToken)){
        error("Expected type name");
        synchronize();
        return t;
    }
    t.name = currentToken.lexeme;
    advance();
    t.pointerDepth=0;
    while (match(TokenType::Star)) {
        t.pointerDepth++;
    }
    return t;
}
std::vector<Param> Parser::parseParamList(){
     std::vector<Param> params;

    do {
        TypeInfo type = parseType();  // ← includes pointer depth
        std::string name =
            consume(TokenType::Identifier, "Expected parameter name").lexeme;

        params.push_back(Param{type, name});
    } while (match(TokenType::Comma));

    return params;
}

std::unique_ptr<FunctionDecl> Parser::parseFunctionDecl(){
    auto loc = currentToken.location;
    TypeInfo returnType = parseType();
    std::string name = consume(TokenType::Identifier,"Expected function name").lexeme;

    consume(TokenType::LParen,"Expected '('");
    std::vector<Param> params;
    if(!check(TokenType::RParen)){
        params = parseParamList();
    }
    consume(TokenType::RParen,"Expected ')'");

    auto body = parseBlock();
    return std::make_unique<FunctionDecl>(
        returnType,name,std::move(params),std::move(body),loc
    );
}
bool Parser::isMethodDeclaration() {
    if (!isType(currentToken)) return false;

    Token next = peek(1);
    if (next.type != TokenType::Identifier) return false;

    Token after = peek(2);
    return after.type == TokenType::LParen;
}

std::unique_ptr<StructDecl> Parser::parseStructDecl(){
    auto loc = currentToken.location;
    

    consume(TokenType::Struct,"Expected 'struct'");
    std::string name =  consume(TokenType::Identifier,"Expected struct name").lexeme;
    std::vector<StructField> fields;
    std::vector<std::unique_ptr<FunctionDecl>> methods;
    consume(TokenType::LBrace, "Expected '{' after struct name");

    while (!check(TokenType::RBrace) && !currentLexer->ended()) {
        if(!isType(currentToken)){
            error("Expected field or method declaration");
            synchronize();
            continue;
        }
        if (isMethodDeclaration()) {
            auto method = parseFunctionDecl();
            method->isMethod=true;
            method->structName = name;
            methods.push_back(std::move(method));
        }else{
            TypeInfo type = parseType();
            std::string fieldName = consume(TokenType::Identifier,"Expected field name").lexeme;
            consume(TokenType::Semicolon,"Expected ';' after field");
            fields.push_back(StructField{type,fieldName});
        }
    }
    consume(TokenType::RBrace,"Expecter '}' after struct body");
    return std::make_unique<StructDecl>(
        name,
        std::move(fields),
        std::move(methods),
        loc
    );
}
std::unique_ptr<BlockStmt> Parser::parseBlock(){
    auto loc = currentToken.location;
    consume(TokenType::LBrace,"Expected '{' to start block");

    std::vector<StmtPtr> statements;
    while (!check(TokenType::RBrace) && !currentLexer->ended()) {
        auto stmt = parseStatement();
        if(!stmt){
            synchronize();
            continue;
        }
        statements.push_back(std::move(stmt));
    }

    consume(TokenType::RBrace,"Expected '}' to close block");
    return std::make_unique<BlockStmt>(
        loc,
        std::move(statements)
    );
}
StmtPtr Parser::parseStatement() {
    if (match(TokenType::If))     return parseIf();
    if (match(TokenType::While))  return parseWhile();
    if (match(TokenType::For))    return parseFor();
    if (match(TokenType::Return)) return parseReturn();
    if (match(TokenType::LBrace)) return parseBlock();

    if (isType(currentToken)) return parseVarDecl();

    return parseExpressionStmt();
}
StmtPtr Parser::parseExpressionStmt() {
    auto expr = parseExpression();
    consume(TokenType::Semicolon, "Expected ';' after expression");
    return std::make_unique<ExprStmt>(std::move(expr), expr->loc);
}

ExprPtr Parser::parseExpression(){
    return parseAssignment();
}

ExprPtr Parser::parseAssignment() {
    auto expr = parseTernary();

    if (match(TokenType::Assign)) {
        Token equals = previousToken;
        auto value = parseAssignment(); // right-associative

        // Only identifiers (variables) are valid targets for now
        if (auto id = dynamic_cast<IdentifierExpr*>(expr.get())) {
            return std::make_unique<AssignExpr>(std::move(expr), std::move(value), equals.location);
        } else {
            error("Invalid assignment target");
            return expr; // fallback to original expression
        }
    }

    return expr;
}

ExprPtr Parser::parseTernary(){
    auto expr = parseOr();
    auto loc = currentToken.location;
    if(match(TokenType::Question)){
        auto trueExpr = parseExpression();
        consume(TokenType::Colon,"Expected ':' in ternary expression");
        auto falseExpr = parseExpression();
        return std::make_unique<TernaryExpr>(
            std::move(expr),
            std::move(trueExpr),
            std::move(falseExpr),
            loc
        );
    }
    return expr;
}
BinaryOp tokenToBinaryOp(TokenType type) {
    switch (type) {
        case TokenType::Plus:        return BinaryOp::Add;
        case TokenType::Minus:       return BinaryOp::Sub;
        case TokenType::Star:        return BinaryOp::Mul;
        case TokenType::Slash:       return BinaryOp::Div;
        case TokenType::Equal:       return BinaryOp::Equal;
        case TokenType::NotEqual:    return BinaryOp::NotEqual;
        case TokenType::Less:        return BinaryOp::Less;
        case TokenType::LessEqual:   return BinaryOp::LessEqual;
        case TokenType::Greater:     return BinaryOp::Greater;
        case TokenType::GreaterEqual:return BinaryOp::GreaterEqual;
        case TokenType::Ampersand:   return BinaryOp::And;
        case TokenType::Or:          return BinaryOp::Or;
        case TokenType::Xor:         return BinaryOp::Xor;
        case TokenType::Assign:      return BinaryOp::Assign;
        default:
            throw std::runtime_error("Unknown binary operator");
    }
}

ExprPtr Parser::parseOr(){
    auto loc = currentToken.location;
    auto expr = parseAnd();
    while (match(TokenType::Or)) {
        Token op = previousToken;
        auto right = parseAnd();
        expr = std::make_unique<BinaryExpr>(tokenToBinaryOp(op.type),std::move(expr),std::move(right),loc);
    }
    return expr;
}
ExprPtr Parser::parseAnd(){
    auto loc = currentToken.location;
    auto expr = parseEquality();
    while (match(TokenType::Ampersand)) {
        Token op = previousToken;
        auto right = parseEquality();
        expr = std::make_unique<BinaryExpr>(tokenToBinaryOp(op.type),std::move(expr),std::move(right),loc);
    }
    return expr;
}
ExprPtr Parser::parseEquality(){
    auto loc = currentToken.location;
    auto expr = parseComparison();
    while (match(TokenType::Equal) || match(TokenType::NotEqual)) {
        auto expr = parseComparison();
        Token op = previousToken;
        auto right = parseComparison();
        expr = std::make_unique<BinaryExpr>(tokenToBinaryOp(op.type),std::move(expr),std::move(right),loc);
    }
    return expr;
}
ExprPtr Parser::parseComparison(){
    auto loc = currentToken.location;
    auto expr = parseTerm();
    while (match(TokenType::Less) || match(TokenType::LessEqual) || match(TokenType::Greater) || match(TokenType::GreaterEqual)) {
        Token op = previousToken;
        auto right =parseTerm();
        expr = std::make_unique<BinaryExpr>(tokenToBinaryOp(op.type),std::move(expr),std::move(right),loc);
    }
    return expr;
}

ExprPtr Parser::parseTerm(){
    auto loc = currentToken.location;
    auto expr = parseFactor();
    while (match(TokenType::Plus) || match(TokenType::Minus)) {
        Token op = previousToken;
        auto right = parseFactor();
        expr = std::make_unique<BinaryExpr>(tokenToBinaryOp(op.type),std::move(expr),std::move(right),loc);
    }
    return expr;
}
ExprPtr Parser::parseFactor() {
    auto expr = parseUnary();

    while (match(TokenType::Star) || match(TokenType::Slash)) {
        BinaryOp op = tokenToBinaryOp(previousToken.type);
        auto right = parseUnary();
        expr = std::make_unique<BinaryExpr>(
            op,
            std::move(expr),
            std::move(right),
            previousToken.location
        );
    }

    return expr;
}

UnaryOp tokenToUnaryOp(TokenType type) {
    switch (type) {
        case TokenType::Minus:      return UnaryOp::Negative;
        case TokenType::Not:        return UnaryOp::Not;
        case TokenType::Star:       return UnaryOp::Dereference;
        case TokenType::Ampersand:  return UnaryOp::AddressOf;
        default:
            throw std::runtime_error("Invalid unary operator");
    }
}
ExprPtr Parser::parseUnary(){
    auto loc = currentToken.location;
    if (match(TokenType::Minus) ||
        match(TokenType::Not) ||
        match(TokenType::Star) ||
        match(TokenType::Ampersand)) {
        Token op = previousToken;
        auto right = parseUnary();
        return std::make_unique<UnaryExpr>(tokenToUnaryOp(op.type),std::move(right),loc,TypeInfo{"not_yet",0});
    }
    return parseCall(parsePrimary());
}
ExprPtr Parser::parseCall(ExprPtr expr) {
    while (true) {
        // Function call
        if (match(TokenType::LParen)) {
            std::vector<ExprPtr> args;

            if (!check(TokenType::RParen)) {
                do {
                    args.push_back(parseExpression());
                } while (match(TokenType::Comma));
            }

            consume(TokenType::RParen, "Expected ')' after arguments");

            expr = std::make_unique<CallExpr>(
                std::move(expr),
                std::move(args),
                previousToken.location,
                TypeInfo{"unknown",0}
            );
        }
        // Member access
        else if (match(TokenType::Dot)) {
            Token name =
                consume(TokenType::Identifier, "Expected member name");

            expr = std::make_unique<MemberAccessExpr>(
                std::move(expr),
                name.lexeme,
                name.location
            );
        }
        else {
            break;
        }
    }

    return expr;
}
ExprPtr Parser::parsePrimary() {
    Token tok = currentToken;
    if (match(TokenType::This)) {
        return std::make_unique<ThisExpr>(previousToken.location);
    }

    if (match(TokenType::IntLiteral)) {
        return std::make_unique<IntLiteralExpr>(
            std::stoll(tok.lexeme),
            tok.location,
            "32"
        );
    }

    if (match(TokenType::FloatLiteral)) {
        return std::make_unique<FloatLiteralExpr>(
            std::stod(tok.lexeme),
            tok.location,
            "64"
        );
    }

    if (match(TokenType::CharLiteral)) {
        return std::make_unique<CharLiteralExpr>(
            tok.lexeme[0],
            tok.location
        );
    }

    if (match(TokenType::Identifier)) {
        return std::make_unique<IdentifierExpr>(
            tok.lexeme,
            tok.location,
            TypeInfo{"unknown", 0}
        );
    }

    if (match(TokenType::LParen)) {
        auto expr = parseExpression();
        consume(TokenType::RParen, "Expected ')'");
        return expr;
    }

    error("Expected expression");
    synchronize();
    return nullptr;
}

std::unique_ptr<Stmt> Parser::parseIf() {
    auto loc = previousToken.location; // 'if' token
    auto condition = parseExpression();
    auto thenBranch = parseBlock();

    // parse optional elifs
    std::vector<ElifClause> elifClauses;
    while (match(TokenType::Elif)) {
        auto elifCond = parseExpression();
        auto elifBody = parseBlock();
        elifClauses.push_back({std::move(elifCond), std::move(elifBody)});
    }

    // parse optional else
    StmtPtr elseBranch = nullptr;
    if (match(TokenType::Else)) {
        elseBranch = parseBlock();
    }
     return std::make_unique<IfStmt>(
        std::move(condition),
        std::move(thenBranch),
        std::move(elseBranch),
        std::move(elifClauses),
        loc
    );
}
std::unique_ptr<Stmt> Parser::parseWhile() {
    auto loc = previousToken.location;
    auto condition = parseExpression();
    auto body = parseBlock();
    return std::make_unique<WhileStmt>(std::move(condition), std::move(body), loc);
}
std::unique_ptr<Stmt> Parser::parseFor() {
    auto loc = previousToken.location;

    consume(TokenType::LParen, "Expected '(' after 'for'");

    // optional initializer
    StmtPtr init = nullptr;
    if (!check(TokenType::Semicolon)) {
        if (isType(currentToken))
            init = parseVarDecl();
        else
            init = parseExpressionStmt();
    }
    consume(TokenType::Semicolon, "Expected ';' after for-loop initializer");

    // optional condition
    ExprPtr condition = nullptr;
    if (!check(TokenType::Semicolon)) {
        condition = parseExpression();
    }
    consume(TokenType::Semicolon, "Expected ';' after for-loop condition");

    // optional increment
    ExprPtr increment = nullptr;
    if (!check(TokenType::RParen)) {
        increment = parseExpression();
    }
    consume(TokenType::RParen, "Expected ')' after for-loop increment");

    auto body = parseBlock();

    return std::make_unique<ForStmt>(
        std::move(init),
        std::move(condition),
        std::move(increment),
        std::move(body),
        loc
    );
}
std::unique_ptr<Stmt> Parser::parseReturn() {
    auto loc = previousToken.location;
    ExprPtr value = nullptr;

    if (!check(TokenType::Semicolon)) {
        value = parseExpression();
    }
    consume(TokenType::Semicolon, "Expected ';' after return statement");

    return std::make_unique<ReturnStmt>(std::move(value), loc);
}
std::unique_ptr<Stmt> Parser::parseVarDecl() {
    auto loc = currentToken.location;
    TypeInfo type = parseType();
    std::string name = consume(TokenType::Identifier, "Expected variable name").lexeme;

    ExprPtr initializer = nullptr;
    if (match(TokenType::Assign)) {
        initializer = parseExpression();
    }

    consume(TokenType::Semicolon, "Expected ';' after variable declaration");
    return std::make_unique<VarDeclStmt>(type.name,type.pointerDepth, name, std::move(initializer), loc);
}
