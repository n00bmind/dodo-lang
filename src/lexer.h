#include "common.h"

struct TokenKindValue
{
    char const* shortName;
    u32 flags;
};

#define TOKENS(x) \
    x(Unknown,          "unknown",      ( "???", 0 )) \
    \
    x(Exclamation,      "!",            ( " ! ", 0 )) \
    x(Pound,            "#",            ( " # ", 0 )) \
    x(Dollar,           "$",            ( " $ ", 0 )) \
    x(Percent,          "%",            ( " % ", 0 )) \
    x(Ampersand,        "&",            ( " & ", 0 )) \
    x(SingleQuote,      "'",            ( " ' ", 0 )) \
    x(OpenParen,        "(",            ( " ( ", 0 )) \
    x(CloseParen,       ")",            ( " ) ", 0 )) \
    x(Asterisk,         "*",            ( " * ", 0 )) \
    x(Plus,             "+",            ( " + ", 0 )) \
    x(Comma,            ",",            ( " , ", 0 )) \
    x(Minus,            "-",            ( " - ", 0 )) \
    x(Dot,              ".",            ( " . ", 0 )) \
    x(Slash,            "/",            ( " / ", 0 )) \
    x(Colon,            ":",            ( " : ", 0 )) \
    x(Semicolon,        ";",            ( " ; ", 0 )) \
    x(LessThan,         "<",            ( " < ", 0 )) \
    x(Equal,            "=",            ( " = ", 0 )) \
    x(GreaterThan,      ">",            ( " > ", 0 )) \
    x(Question,         "?",            ( " ? ", 0 )) \
    x(At,               "@",            ( " @ ", 0 )) \
    x(OpenBracket,      "[",            ( " [ ", 0 )) \
    x(Backslash,        "\\",           ( " \\ ", 0 )) \
    x(CloseBracket,     "]",            ( " ] ", 0 )) \
    x(Caret,            "^",            ( " ^ ", 0 )) \
    x(Underscore,       "_",            ( " _ ", 0 )) \
    x(BackTick,         "`",            ( " ` ", 0 )) \
    x(OpenBrace,        "{",            ( " { ", 0 )) \
    x(Pipe,             "|",            ( " | ", 0 )) \
    x(CloseBrace,       "}",            ( " } ", 0 )) \
    x(Tilde,            "~",            ( " ~ ", 0 )) \
    \
    x(Name,             "identifier",   ( "IDN", 0 )) \
    x(Keyword,          "keyword",      ( "KWD", 0 )) \
    x(StringLiteral,    "string",       ( "STR", 0 )) \
    x(IntLiteral,       "integer",      ( "INT", 0 )) \
    x(FloatLiteral,     "float",        ( "FLT", 0 )) \
    x(Comment,          "comment",      ( "/*/", 0 )) \
    x(Spacing,          "spacing",      ( "   ", 0 )) \
    x(Newline,          "newline",      ( "NLN", 0 )) \
    x(EndOfStream,      "EOS",          ( "EOS", 0 )) \

STRUCT_ENUM_WITH_NAMES_VALUES(TokenKind, TokenKindValue, TOKENS)
#undef TOKENS


struct SourcePos
{
    char const* filename;
    i32 lineNumber;
    i32 columnNumber;
};

struct Token
{
    // TODO 
    enum Flags
    {
        None        = 0,
        PostfixOp   = 0x1,
        UnaryOp     = 0x2,
        AddOp       = 0x4,
        MulOp       = 0x8,
        CmpOp       = 0x10,
    };

    SourcePos pos;
    String text;
    TokenKind::Enum kind; 

    union
    {
        char const* ident;  // Interned
        f64 floatValue;
        u64 intValue;
    };
};

struct InternString
{
    enum Flags : u16
    {
        None = 0,
        Keyword = 0x1,
    };
    
    char const* data;
    u32 hash;
    i16 length;
    Flags flags;
};

struct InternStringBuffer
{
    MemoryArena arena;
    // TODO This should be a _growable_ hashtable (with linear probing!)?
    BucketArray<InternString> entries;
};

