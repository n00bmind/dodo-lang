
struct TokenTypeValue
{
    char const* shortName;
    char const* displayName;
};

#define TOKENS(x) \
    x(Unknown,      ARGS( "???", "unknown" )) \
    \
    x(Exclamation,  ARGS( " ! ", "!" )) \
    x(Pound,        ARGS( " # ", "#" )) \
    x(Dollar,       ARGS( " $ ", "$" )) \
    x(Percent,      ARGS( " % ", "%" )) \
    x(Ampersand,    ARGS( " & ", "&" )) \
    x(SingleQuote,  ARGS( " ' ", "'" )) \
    x(OpenParen,    ARGS( " ( ", "(" )) \
    x(CloseParen,   ARGS( " ) ", ")" )) \
    x(Asterisk,     ARGS( " * ", "*" )) \
    x(Plus,         ARGS( " + ", "+" )) \
    x(Comma,        ARGS( " , ", "," )) \
    x(Minus,        ARGS( " - ", "-" )) \
    x(Dot,          ARGS( " . ", "." )) \
    x(Slash,        ARGS( " / ", "/" )) \
    x(Colon,        ARGS( " : ", ":" )) \
    x(Semicolon,    ARGS( " ; ", ";" )) \
    x(LessThan,     ARGS( " < ", "<" )) \
    x(Equal,        ARGS( " = ", "=" )) \
    x(GreaterThan,  ARGS( " > ", ">" )) \
    x(Question,     ARGS( " ? ", "?" )) \
    x(At,           ARGS( " @ ", "@" )) \
    x(OpenBracket,  ARGS( " [ ", "[" )) \
    x(Backslash,    ARGS( " \\ ", "\\" )) \
    x(CloseBracket, ARGS( " ] ", "]" )) \
    x(Caret,        ARGS( " ^ ", "^" )) \
    x(Underscore,   ARGS( " _ ", "_" )) \
    x(BackTick,     ARGS( " ` ", "`" )) \
    x(OpenBrace,    ARGS( " { ", "{" )) \
    x(Pipe,         ARGS( " | ", "|" )) \
    x(CloseBrace,   ARGS( " } ", "}" )) \
    x(Tilde,        ARGS( " ~ ", "~" )) \
    \
    x(Identifier,       ARGS( "IDN", "identifier" )) \
    x(StringLiteral,    ARGS( "STR", "string" )) \
    x(NumericLiteral,   ARGS( "NUM", "number" )) \
    x(Comment,          ARGS( "/*/", "comment" )) \
    x(Spacing,          ARGS( "   ", "spacing" )) \
    x(Newline,          ARGS( "NLN", "newline" )) \
    x(EndOfStream,      ARGS( "EOS", "EOS" )) \

STRUCT_ENUM_WITH_VALUES(TokenKind, TokenTypeValue, TOKENS)
#undef TOKENS

struct Token
{
    String text;
    char const* filename;
    i32 lineNumber;
    i32 columnNumber;
    TokenKind kind; 

    union
    {
        f64 f64;
        u64 u64;
        char const* ident;  // Interned
    };
};

struct InternString
{
    String str;
    u32 hash;
};

struct InternStringBuffer
{
    MemoryArena arena;
    // TODO This should be a _growable_ hashtable (with linear probing!)?
    BucketArray<InternString> entries;
};

