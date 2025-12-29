#ifndef LEXER_HPP
#define LEXER_HPP

#include <string>
#include <unordered_map>
#include <utility>

enum class TokenType{
  Invalid,
  EndOfFile,

  Identifier,
  I16,I32,I64,
  F16,F32,F64,
  Char,Boolean,Void,

  IntLiteral,FloatLiteral,CharLiteral,StringLiteral,

  Struct,Module,Include,Return,
  If,Elif,Else,
  While,For,

  Plus,Minus,Star,Slash,
  Assign,
  Equal,NotEqual,
  Less,LessEqual,
  Greater,GreaterEqual,
  Or,Not,Xor,

  Question,Colon,

  LParen,RParen,
  LBrace,RBrace,
  LBracket,RBracket,
  Comma,Semicolon,
  Dot,
  Ampersand,

};
struct SourceLocation{
  int line,column;
  SourceLocation() = default;
  explicit SourceLocation(int line,int column):line(line),column(column){}
  std::string to_string();
};
struct Token{
  TokenType type;
  std::string lexeme;
  SourceLocation location;
  Token(TokenType type,std::string lexme,int line,int col):type(type),lexeme(std::move(lexme)),location(line, col){}
};
class Lexer{
  public:
    explicit Lexer(const std::string& src) : source(src){}
    Token nextToken();
    bool ended();
  private:
    char peek() const;
    char advance();
    bool match(char expected);

    void skipWhitespace();
    Token identifierOrKeyword();
    Token number();
    Token stringLiteral();
    Token charLiteral();
  private:
    std::string source;
    size_t current=0;
    int line = 1;
    int column = 1;
};

inline std::unordered_map<std::string, TokenType> keywords = {
    {"struct",TokenType::Struct},
    {"module",TokenType::Module},
    {"include",TokenType::Include},
    {"return",TokenType::Return},
    {"i16",TokenType::I16},
    {"i32",TokenType::I32},
    {"i64",TokenType::I64},
    {"f16",TokenType::F16},
    {"f32",TokenType::F32},
    {"f64",TokenType::F64},
    {"char",TokenType::Char},
    {"bool",TokenType::Boolean},
    {"void",TokenType::Void},
    {"if",TokenType::If},
    {"else",TokenType::Else},
    {"while",TokenType::While},
    {"for",TokenType::For},
    {"elif",TokenType::Elif},
};

#endif
