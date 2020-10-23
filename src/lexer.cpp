namespace Lexer
{
    
struct TokenTypeValue
{
    char const* shortName;
    char const* displayName;
};

#define VALUES(x) \
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

STRUCT_ENUM_WITH_VALUES(TokenType, TokenTypeValue, VALUES)
#undef VALUES

static int globalSymbolLUT[256] = {};


struct Token
{
    String text;
    char const* filename;
    i32 lineNumber;
    i32 columnNumber;
    TokenType type; 

    //union   // Could parse values in-place
    //{
        //f32 f32;
        //i32 i32;
    //}
};

struct Lexer
{
    String stream;
    char const* filename;
    i32 lineNumber;
    i32 columnNumber;
    char at[2];

    bool error;

    Lexer( String const& input, char const* filename_ )
        : stream( input )
        , filename( filename_ )
        , lineNumber( 1 )
        , columnNumber( 1 )
    {
        Refill();
    }

    void Advance( int count = 1 )
    {
        if( count > stream.length )
            count = stream.length;

        stream.length -= count;
        stream.data += count;

        columnNumber += count;

        Refill();
    }

    bool IsValid()
    {
        return !error;
    }

private:
    void Refill()
    {
        if( stream.length == 0 )
        {
            at[0] = 0;
            at[1] = 0;
        }
        else if( stream.length == 1 )
        {
            at[0] = stream.data[0];
            at[1] = 0;
        }
        else
        {
            at[0] = stream.data[0];
            at[1] = stream.data[1];
        }
    }
};

static void Error( Lexer* lexer, Token onToken, char const* fmt, ... )
{
    va_list arg_list;
    va_start( arg_list, fmt );

    vfprintf( stderr, fmt, arg_list );
    va_end( arg_list );

    lexer->error = true;
}

#undef ERROR
#define ERROR( token, msg, ... ) Error( lexer, token, "%s(%d,%d): "msg, \
                                        token.filename, token.lineNumber, token.columnNumber, ##__VA_ARGS__ );

static bool IsNumeric( char c )
{
    // TODO
    //NOT_IMPLEMENTED
    return false;
}

static void ParseNumber( Lexer* lexer )
{

}

static void AdvanceNewline( Lexer* lexer, char c0, char c1 )
{
    // Account for double char end of lines
    if( (c0 == '\r' && c1 == '\n')
        || (c0 == '\n' && c1 == '\r') )
        lexer->Advance();

    lexer->lineNumber++;
    lexer->columnNumber = 1;
}

static Token GetTokenRaw( Lexer* lexer )
{
    Token token = {};
    token.text = lexer->stream;
    token.filename = lexer->filename;
    token.lineNumber = lexer->lineNumber;
    token.columnNumber = lexer->columnNumber;

    char c = lexer->at[0];
    lexer->Advance();

    // First use the lookup for symbols
    int lookupIndex = globalSymbolLUT[c];
    if( lookupIndex )
        token.type = TokenType::Values::items[lookupIndex];
    else
    {
        switch( c )
        {
            case '\0':
            {
                token.type = TokenType::EndOfStream();
            } break;

            case '"':
            {
                token.type = TokenType::StringLiteral();

                while( lexer->at[0] && lexer->at[0] != '"' )
                {
                    // Skip escape sequences
                    if( lexer->at[0] == '\\' && lexer->at[1] )
                    {
                        lexer->Advance();
                    }
                    lexer->Advance();
                }
                // Skip last quote
                if( lexer->at[0] )
                    lexer->Advance();
            }

            case '/':
            {
                // C++ style
                if( lexer->at[0] == '/' )
                {
                    token.type = TokenType::Comment();
                    lexer->Advance();

                    while( lexer->at[0] && !IsNewline( lexer->at[0] ) )
                        lexer->Advance();
                }
                // C style
                else if( lexer->at[0] == '*' )
                {
                    token.type = TokenType::Comment();
                    lexer->Advance();

                    while( lexer->at[0] && !(lexer->at[0] == '*' && lexer->at[1] == '/') )
                    {
                        // NOTE This is slightly weird in that we potentially advance the line number before skipping over a multi-byte newline
                        if( IsNewline( lexer->at[0] ) )
                        {
                            AdvanceNewline( lexer, lexer->at[0], lexer->at[1] );
                        }
                        lexer->Advance();
                    }

                    if( lexer->at[0] )
                        lexer->Advance( 2 );
                }
                else
                {
                    // TODO
                }
            } break;

            default:
            {
                if( IsSpacing( c ) )
                {
                    token.type = TokenType::Spacing();

                    while( IsSpacing( lexer->at[0] ) )
                        lexer->Advance();
                }
                else if( IsNewline( c ) )
                {
                    token.type = TokenType::Newline();
#if 0
                    // Account for double char end of lines
                    if( (c == '\r' && lexer->at[0] == '\n')
                        || (c == '\n' && lexer->at[0] == '\r') )
                        lexer->Advance();

                    lexer->lineNumber++;
                    lexer->columnNumber = 1;
#else
                    AdvanceNewline( lexer, c, lexer->at[0] );
#endif
                }
                else if( IsAlpha( c ) )
                {
                    token.type = TokenType::Identifier();

                    while( IsAlpha( lexer->at[0] ) || IsNumber( lexer->at[0] ) || lexer->at[0] == '_' )
                        lexer->Advance();
                }
                else if( IsNumeric( c ) )
                {
                    token.type = TokenType::NumericLiteral();

                    ParseNumber( lexer );
                }
                else
                    token.type = TokenType::Unknown();

            } break;
        }
    }
    token.text.length = I32( lexer->stream.data - token.text.data );

    return token;
}

static Token GetToken( Lexer* lexer )
{
    Token token;
    while( true )
    {
        token = GetTokenRaw( lexer );
        if( token.type == TokenType::Spacing() ||
            token.type == TokenType::Newline() ||
            token.type == TokenType::Comment() )
        {} // Ignore
        else
        {
            if( token.type == TokenType::StringLiteral() )
            {
                if( token.text.length && token.text.data[0] == '"' )
                {
                    token.text.data++;
                    token.text.length--;
                }
                if( token.text.length && token.text.data[ token.text.length - 1 ] == '"' )
                {
                    token.text.length--;
                }
            }
            break;
        }
    }

    return token;
}

static Token RequireToken( Lexer* lexer, TokenType const& wantedType )
{
    Token token = GetToken( lexer );

    if( token.type != wantedType )
        ERROR( token, "Unexpected token type (wanted '%s', got '%s')",
               wantedType.value.displayName, token.type.value.displayName );

    return token;
}

static Token PeekToken( Lexer* lexer )
{
    Lexer temp = *lexer;
    Token result = GetToken( &temp );
    return result;
}

//static bool OptionalToken( Lexer* lexer, u32 wantedType )
//{
    //Token token = GetToken( lexer );
    //return token.type == wantedType;
//}

void Scan( String const& program, char const* filename )
{
    Lexer lexer = Lexer( program, filename );

    // Init symbol look up table
    globalSymbolLUT['!'] = TokenType::Exclamation().index;
    globalSymbolLUT['#'] = TokenType::Pound().index;
    globalSymbolLUT['$'] = TokenType::Dollar().index;
    globalSymbolLUT['%'] = TokenType::Percent().index;
    globalSymbolLUT['&'] = TokenType::Ampersand().index;
    globalSymbolLUT['\''] = TokenType::SingleQuote().index;
    globalSymbolLUT['('] = TokenType::OpenParen().index;
    globalSymbolLUT[')'] = TokenType::CloseParen().index;
    globalSymbolLUT['*'] = TokenType::Asterisk().index;
    globalSymbolLUT['+'] = TokenType::Plus().index;
    globalSymbolLUT[','] = TokenType::Comma().index;
    globalSymbolLUT['-'] = TokenType::Minus().index;
    globalSymbolLUT['.'] = TokenType::Dot().index;
    globalSymbolLUT['/'] = TokenType::Slash().index;
    globalSymbolLUT[':'] = TokenType::Colon().index;
    globalSymbolLUT[';'] = TokenType::Semicolon().index;
    globalSymbolLUT['<'] = TokenType::LessThan().index;
    globalSymbolLUT['='] = TokenType::Equal().index;
    globalSymbolLUT['>'] = TokenType::GreaterThan().index;
    globalSymbolLUT['?'] = TokenType::Question().index;
    globalSymbolLUT['@'] = TokenType::At().index;
    globalSymbolLUT['['] = TokenType::OpenBracket().index;
    globalSymbolLUT['\\'] = TokenType::Backslash().index;
    globalSymbolLUT[']'] = TokenType::CloseBracket().index;
    globalSymbolLUT['^'] = TokenType::Caret().index;
    globalSymbolLUT['_'] = TokenType::Underscore().index;
    globalSymbolLUT['`'] = TokenType::BackTick().index;
    globalSymbolLUT['{'] = TokenType::OpenBrace().index;
    globalSymbolLUT['|'] = TokenType::Pipe().index;
    globalSymbolLUT['}'] = TokenType::CloseBrace().index;
    globalSymbolLUT['~'] = TokenType::Tilde().index;

    bool parsing = true;
    while( parsing )
    {
        Token token = GetToken( &lexer );
        switch( token.type.index )
        {
            default:
            {
                globalPlatform.Print( "%s - %.*s\n", token.type.value.shortName, token.text.length, token.text.data );
            } break;
            case TokenType::EndOfStream().index:
            {
                parsing = false;
            } break;
        }
    }
}

#undef ERROR

} // namespace Lexer

