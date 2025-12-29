#include "../headers/lexer/lexer.hpp"
#include <cctype>
#include <cstddef>
#include <string>
bool Lexer::ended(){
    return this->current >= source.size();
}
std::string SourceLocation::to_string(){
    return "line: "+std::to_string(this->line)+" "+std::to_string(this->column);
}
static char decodeEscape(char c) {
    switch (c) {
        case 'n': return '\n';
        case 't': return '\t';
        case 'r': return '\r';
        case '0': return '\0';
        case '\\': return '\\';
        case '\'': return '\'';
        case '"': return '"';
        default: return c;
    }
}
char Lexer::peek() const{
    if(current >= source.size())
        return '\0';
    return source[current];
}
char Lexer::advance(){
    char c = peek();
    current++;
    column++;
    return c;
}

bool Lexer::match(char expected){
    if(peek()!= expected)
        return false;
    advance();
    return true;
}

void Lexer::skipWhitespace(){
    while (true) {
        char c = peek();
        if(c == ' ' || c=='\t' || c=='\r'){
            advance();
        }else if(c == '\n'){
            advance();
            line++;
            column=1;
        }else if (c == '/' && current + 1 < source.size() && source[current+1] == '/'){
            while (peek() != '\n' && peek() != '\0') {
                advance();
            }
        }else{
            break;
        }
    }
}
Token Lexer::identifierOrKeyword(){
    int startCol = column;
    size_t start = current;
    while (std::isalnum(peek()) || peek()=='_') {
        advance();
    }
    std::string text = source.substr(start,current - start);
    auto it = keywords.find(text);
    if(it != keywords.end()){
        return Token(it->second,text,line,startCol);
    }
    return Token(TokenType::Identifier,text,line,startCol);
}

Token Lexer::number(){
    int startCol = column;
    size_t start = current;
    while (std::isdigit(peek())) {
        advance();
    }
    if(peek() == '.'){
        advance();
        while (std::isdigit(peek())) {
            advance();
        }
        std::string text = source.substr(start,current-start);
        return Token(TokenType::FloatLiteral,text,line,startCol);
    }else{
        std::string text = source.substr(start,current-start);
        return Token(TokenType::IntLiteral,text,line,startCol);
    }
}
Token Lexer::stringLiteral(){
    int startCol = column;
    advance();

    std::string result;

    while (peek() != '"' && peek() != '\0') {
        if (peek() == '\\') {
            advance();
            result.push_back(decodeEscape(advance()));
        }else{
            if (peek() == '\n') {
                line++;
                column = 1;
            }
            result.push_back(advance());
        }
    }
    match('"');

    return Token(TokenType::StringLiteral,result,line,startCol);
}
Token Lexer::charLiteral(){
    int startCol = column;
    advance();

    char value;
    if (peek() == '\\') {
        advance();
        value = decodeEscape(advance());
    }else {
        value=advance();
    }
    match('\'');
    return Token(TokenType::CharLiteral,std::string(1,value),line,startCol);
}

Token Lexer::nextToken(){
    skipWhitespace();
    int startCol = column;
    char c = peek();

    if(c == '\0'){
        return Token(TokenType::EndOfFile,"",line,column);
    }
    if(std::isalpha(c) || c == '_'){
        return identifierOrKeyword();
    }
    if(std::isdigit(c)){
        return number();
    }

    switch (c) {
     case '+': return Token(TokenType::Plus, "+", line, startCol);
        case '-': return Token(TokenType::Minus, "-", line, startCol);
        case '*': return Token(TokenType::Star, "*", line, startCol);
        case '/': return Token(TokenType::Slash, "/", line, startCol);

        case '&':
            return Token(TokenType::Ampersand, "&", line, startCol);

        case '|':
                return Token(TokenType::Or, "|", line, startCol);
            break;

        case '=':
            return match('=') ? Token(TokenType::Equal, "==", line, startCol)
                              : Token(TokenType::Assign, "=", line, startCol);

        case '!':
            return match('=') ? Token(TokenType::NotEqual, "!=", line, startCol)
                              : Token(TokenType::Not, "!", line, startCol);

        case '<':
            return match('=') ? Token(TokenType::LessEqual, "<=", line, startCol)
                              : Token(TokenType::Less, "<", line, startCol);

        case '>':
            return match('=') ? Token(TokenType::GreaterEqual, ">=", line, startCol)
                              : Token(TokenType::Greater, ">", line, startCol);

        case '?': return Token(TokenType::Question, "?", line, startCol);
        case ':': return Token(TokenType::Colon, ":", line, startCol);

        case '(': return Token(TokenType::LParen, "(", line, startCol);
        case ')': return Token(TokenType::RParen, ")", line, startCol);
        case '{': return Token(TokenType::LBrace, "{", line, startCol);
        case '}': return Token(TokenType::RBrace, "}", line, startCol);
        case '[': return Token(TokenType::LBracket, "[", line, startCol);
        case ']': return Token(TokenType::RBracket, "]", line, startCol);

        case ',': return Token(TokenType::Comma, ",", line, startCol);
        case ';': return Token(TokenType::Semicolon, ";", line, startCol);
        case '.': return Token(TokenType::Dot, ".", line, startCol);

        case '"': return stringLiteral();
        case '\'': return charLiteral();
        case '^':return Token(TokenType::Xor,"^",line,startCol);
    }
    return Token(TokenType::Invalid,std::string(1,c),line,startCol);

}