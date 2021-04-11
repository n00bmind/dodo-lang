
enum TokenFlags : u32
{
    Tk_None     = 0,
    PostfixOp   = 0x1,
    UnaryOp     = 0x2,
    AddOp       = 0x4,
    MulOp       = 0x8,
    CmpOp       = 0x10,
    AssignOp    = 0x20,
};

struct TokenKindValue
{
    char const* shortName;
    u32 flags;
};

// TODO Review identifier names here. Use 'semantic' names everywhere
#define TOKENS(x) \
    x(Unknown,          "unknown",      ( "???", 0 )) \
    \
    x(Exclamation,      "!",            ( " ! ", UnaryOp )) \
    x(Pound,            "#",            ( " # ", PostfixOp )) \
    x(Dollar,           "$",            ( " $ ", 0 )) \
    x(Percent,          "%",            ( " % ", MulOp )) \
    x(Ampersand,        "&",            ( " & ", UnaryOp | MulOp )) \
    x(SingleQuote,      "'",            ( " ' ", 0 )) \
    x(OpenParen,        "(",            ( " ( ", PostfixOp )) \
    x(CloseParen,       ")",            ( " ) ", 0 )) \
    x(Asterisk,         "*",            ( " * ", UnaryOp | MulOp )) \
    x(Plus,             "+",            ( " + ", UnaryOp | AddOp )) \
    x(Comma,            ",",            ( " , ", 0 )) \
    x(Minus,            "-",            ( " - ", UnaryOp | AddOp )) \
    x(Dot,              ".",            ( " . ", PostfixOp )) \
    x(Slash,            "/",            ( " / ", MulOp )) \
    x(Colon,            ":",            ( " : ", 0 )) \
    x(Semicolon,        ";",            ( " ; ", 0 )) \
    x(LessThan,         "<",            ( " < ", CmpOp )) \
    x(Assign,           "=",            ( " = ", AssignOp )) \
    x(GreaterThan,      ">",            ( " > ", CmpOp )) \
    x(Question,         "?",            ( " ? ", 0 )) \
    x(At,               "@",            ( " @ ", 0 )) \
    x(OpenBracket,      "[",            ( " [ ", PostfixOp )) \
    x(Backslash,        "\\",           ( " \\ ", 0 )) \
    x(CloseBracket,     "]",            ( " ] ", 0 )) \
    x(Caret,            "^",            ( " ^ ", AddOp )) \
    x(Underscore,       "_",            ( " _ ", 0 )) \
    x(BackTick,         "`",            ( " ` ", 0 )) \
    x(OpenBrace,        "{",            ( " { ", 0 )) \
    x(Pipe,             "|",            ( " | ", AddOp )) \
    x(CloseBrace,       "}",            ( " } ", 0 )) \
    x(Tilde,            "~",            ( " ~ ", UnaryOp )) \
    \
    x(RightArrow,       "->",           ( "-> ", 0 )) \
    x(LeftShift,        "<<",           ( "<< ", MulOp )) \
    x(RightShift,       ">>",           ( ">> ", MulOp )) \
    x(Equal,            "==",           ( "== ", CmpOp )) \
    x(NotEqual,         "!=",           ( "!= ", CmpOp )) \
    x(LTEqual,          "<=",           ( "<= ", CmpOp )) \
    x(GTEqual,          ">=",           ( ">= ", CmpOp )) \
    x(LogicAnd,         "&&",           ( "&& ", 0 )) \
    x(LogicOr,          "||",           ( "|| ", 0 )) \
    x(ColonAssign,      ":=",           ( ":= ", AssignOp )) \
    x(PlusAssign,       "+=",           ( "+= ", AssignOp )) \
    x(MinusAssign,      "-=",           ( "-= ", AssignOp )) \
    x(MulAssign,        "*=",           ( "*= ", AssignOp )) \
    x(DivAssign,        "/=",           ( "/= ", AssignOp )) \
    x(ModAssign,        "%=",           ( "%= ", AssignOp )) \
    x(OrAssign,         "|=",           ( "|= ", AssignOp )) \
    x(AndAssign,        "&=",           ( "&= ", AssignOp )) \
    x(XorAssign,        "^=",           ( "^= ", AssignOp )) \
    x(LShiftAssign,     "<<=",          ( "<<=", AssignOp )) \
    x(RShiftAssign,     ">>=",          ( ">>=", AssignOp )) \
    x(Range,            "..",           ( ".. ", 0 )) \
    x(Ellipsis,         "...",          ( "...", 0 )) \
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

ENUM_STRUCT_WITH_NAMES_VALUES(TokenKind, TokenKindValue, TOKENS)
#undef TOKENS


#define KEYWORDS(x) \
    x( Struct,      "struct" ) \
    x( Union,       "union" ) \
    x( Enum,        "enum" ) \
    x( Sizeof,      "sizeof" ) \
    x( If,          "if" ) \
    x( Else,        "else" ) \
    x( While,       "while" ) \
    x( Do,          "do" ) \
    x( For,         "for" ) \
    x( Switch,      "switch" ) \
    x( Case,        "case" ) \
    x( Default,     "default" ) \
    x( Break,       "break" ) \
    x( Continue,    "continue" ) \
    x( Return,      "return" ) \
    x( In,          "in" ) \
    x( Length,      "length" ) \

ENUM_STRUCT_WITH_NAMES(Keyword, KEYWORDS)
#undef KEYWORDS

#define META(x) \
    x( Length,      "length" ) \
    x( Size,        "size" ) \
    x( Offset,      "offset" ) \
    x( Index,       "index" ) \
    x( Name,        "name" ) \

ENUM_STRUCT_WITH_NAMES(MetaAttr, META)
#undef META

extern char const* globalMetaAttrs[MetaAttr::itemCount];

struct DirectiveInfo
{
    char const* argNames[16] = {};
};

#define DIRECTIVES(x) \
    x( Foreign,         "foreign",          () ) \
    x( Expect,          "expect",           ("condition", "errormsg"))  /* TODO Varargs */\
    x( ExpectError,     "expect_error",     ()) \
    x( DebugBreak,      "debugbreak",       ()) \

ENUM_STRUCT_WITH_NAMES_VALUES(Directive, DirectiveInfo, DIRECTIVES)
#undef DIRECTIVES

extern char const* globalDirectives[Directive::itemCount];


struct SourcePos
{
    char const* filename;
    i32 lineNumber;
    i32 columnNumber;
};

struct Token
{
    enum LiteralMod : u32
    {
        None        = 0,
        Hexadecimal = 0x1,
        Octal       = 0x2,
        Binary      = 0x4,
    };

    SourcePos pos;
    String text;            // NOTE Not null terminated
    TokenKind::Enum kind; 

    union
    {
        String strValue;
        f64 floatValue;
        i64 intValue;
        char const* ident;  // Interned
    };
    LiteralMod mod;

    Token()
        : pos()
        , text()
        , kind(TokenKind::Unknown)
        , strValue()
        , mod(None)
    {}

    char const* Text() const;

    bool HasFlag( TokenFlags f ) const
    {
        return (TokenKind::items[ kind ].value.flags & (u32)f) != 0;
    }
};

struct InternString
{
    enum Flags : u16
    {
        None = 0,
        Keyword = 0x1,
        MetaAttr = 0x2,
        Directive = 0x4,
    };
    
    char const* data;
    i32 length;
    u32 flags;
};

struct InternStringBuffer
{
    MemoryArena arena;
    // TODO Can this just live in temporary memory and be discarded after parsing is done?
    Hashtable<String, InternString, MemoryArena> entries;
};


struct Lexer
{
    SourcePos pos;
    Token token;
    String stream;

    bool error;

    Lexer() {}
    Lexer( String const& input, char const* filename_ );

    INLINE void Advance( int count = 1 )
    {
        ASSERT( count <= stream.length );

        stream.length -= count;
        stream.data += count;

        pos.columnNumber += count;
    }

    INLINE bool IsValid()
    {
        return !error;
    }
};


