namespace Tk
{
    
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

STRUCT_ENUM_WITH_VALUES(Type, TokenTypeValue, TOKENS)
#undef TOKENS

} // namespace Lexer

#define KEYWORDS(x ) \
    x( None,    "" ) \
    x( Struct,  "struct" ) \
    x( Enum,    "enum" ) \

STRUCT_ENUM_WITH_NAMES(Keyword, KEYWORDS)
#undef KEYWORDS


internal int globalSymbolLUT[128];
internal char const* globalKeywords[Keyword::Values::count];


struct Token
{
    String text;
    char const* filename;
    i32 lineNumber;
    i32 columnNumber;
    Tk::Type type; 

    union
    {
        f64 f64;
        u64 u64;
        char const* ident;  // Interned
    };
};


internal bool InternsAreEqual( InternString const& a, InternString const& b )
{
    return a.hash == b.hash
        && StringsEqual( a.data, b.data );
}

internal char const* Intern( String const& string )
{
    char const* result = nullptr;
    InternString intern = { string.data, string.length, Hash32( string.data, string.length ) };

    InternString* entry = globalInternStrings.entries.Find( intern, InternsAreEqual );
    if( entry )
        result = entry->data;
    else
    {
        char* stringData = PUSH_STRING( &globalInternStrings.arena, string.length + 1 );
        string.CopyToNullTerminated( stringData );

        intern.data = stringData;
        globalInternStrings.entries.Push( intern );

        result = intern.data;
    }
    
    return result;
}

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
        // Init symbol look up table
        globalSymbolLUT['!'] = Tk::Type::Exclamation().index;
        globalSymbolLUT['#'] = Tk::Type::Pound().index;
        globalSymbolLUT['$'] = Tk::Type::Dollar().index;
        globalSymbolLUT['%'] = Tk::Type::Percent().index;
        globalSymbolLUT['&'] = Tk::Type::Ampersand().index;
        globalSymbolLUT['\''] = Tk::Type::SingleQuote().index;
        globalSymbolLUT['('] = Tk::Type::OpenParen().index;
        globalSymbolLUT[')'] = Tk::Type::CloseParen().index;
        globalSymbolLUT['*'] = Tk::Type::Asterisk().index;
        globalSymbolLUT['+'] = Tk::Type::Plus().index;
        globalSymbolLUT[','] = Tk::Type::Comma().index;
        globalSymbolLUT['-'] = Tk::Type::Minus().index;
        globalSymbolLUT['.'] = Tk::Type::Dot().index;
        //globalSymbolLUT['/'] = Tk::Type::Slash().index;
        globalSymbolLUT[':'] = Tk::Type::Colon().index;
        globalSymbolLUT[';'] = Tk::Type::Semicolon().index;
        globalSymbolLUT['<'] = Tk::Type::LessThan().index;
        globalSymbolLUT['='] = Tk::Type::Equal().index;
        globalSymbolLUT['>'] = Tk::Type::GreaterThan().index;
        globalSymbolLUT['?'] = Tk::Type::Question().index;
        globalSymbolLUT['@'] = Tk::Type::At().index;
        globalSymbolLUT['['] = Tk::Type::OpenBracket().index;
        globalSymbolLUT['\\'] = Tk::Type::Backslash().index;
        globalSymbolLUT[']'] = Tk::Type::CloseBracket().index;
        globalSymbolLUT['^'] = Tk::Type::Caret().index;
        globalSymbolLUT['_'] = Tk::Type::Underscore().index;
        globalSymbolLUT['`'] = Tk::Type::BackTick().index;
        globalSymbolLUT['{'] = Tk::Type::OpenBrace().index;
        globalSymbolLUT['|'] = Tk::Type::Pipe().index;
        globalSymbolLUT['}'] = Tk::Type::CloseBrace().index;
        globalSymbolLUT['~'] = Tk::Type::Tilde().index;

        // Intern keywords
        for( Keyword const& k : Keyword::Values::items )
        {
            globalKeywords[k.index] = Intern( String( k.name ) );
        }

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

internal void Error( Lexer* lexer, Token onToken, char const* fmt, ... )
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

internal bool IsNumeric( char c )
{
    // TODO
    //NOT_IMPLEMENTED
    return false;
}

internal void ParseNumber( Lexer* lexer )
{

}

internal void AdvanceNewline( Lexer* lexer, char c0, char c1 )
{
    // Account for double char end of lines
    if( (c0 == '\r' && c1 == '\n')
        || (c0 == '\n' && c1 == '\r') )
        lexer->Advance();

    lexer->lineNumber++;
    lexer->columnNumber = 1;
}

internal Token GetTokenRaw( Lexer* lexer )
{
    Token token = {};
    token.text = lexer->stream;
    token.filename = lexer->filename;
    token.lineNumber = lexer->lineNumber;
    token.columnNumber = lexer->columnNumber;

    char c = lexer->at[0];
    // TODO Remove this and go back to only advancing when necessary to increase the amount of advances by more than 1 char
    lexer->Advance();

    // First use the lookup for symbols
    int lookupIndex = globalSymbolLUT[c];
    if( lookupIndex )
        token.type = Tk::Type::Values::items[lookupIndex];
    else
    {
        switch( c )
        {
            case '\0':
            {
                token.type = Tk::Type::EndOfStream();
            } break;

            case '"':
            {
                token.type = Tk::Type::StringLiteral();

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
                    token.type = Tk::Type::Comment();
                    lexer->Advance();

                    while( lexer->at[0] && !IsNewline( lexer->at[0] ) )
                        lexer->Advance();
                }
                // C style
                else if( lexer->at[0] == '*' )
                {
                    token.type = Tk::Type::Comment();
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
                // TODO Move the first char of these to the outer switch for speed?
                if( IsSpacing( c ) )
                {
                    token.type = Tk::Type::Spacing();

                    while( IsSpacing( lexer->at[0] ) )
                        lexer->Advance();
                }
                else if( IsNewline( c ) )
                {
                    token.type = Tk::Type::Newline();
                    AdvanceNewline( lexer, c, lexer->at[0] );
                }
                else if( IsAlpha( c ) )
                {
                    token.type = Tk::Type::Identifier();

                    while( IsAlpha( lexer->at[0] ) || IsNumber( lexer->at[0] ) || lexer->at[0] == '_' )
                        lexer->Advance();

                    int length = I32( lexer->stream.data - token.text.data );
                    token.ident = Intern( String( token.text.data, length ) );
                }
                else if( IsNumeric( c ) )
                {
                    token.type = Tk::Type::NumericLiteral();

                    ParseNumber( lexer );
                }
                else
                    token.type = Tk::Type::Unknown();

            } break;
        }
    }
    token.text.length = I32( lexer->stream.data - token.text.data );

    return token;
}

internal Token GetToken( Lexer* lexer )
{
    Token token;
    while( true )
    {
        token = GetTokenRaw( lexer );
        if( token.type == Tk::Type::Spacing() ||
            token.type == Tk::Type::Newline() ||
            token.type == Tk::Type::Comment() )
        {} // Ignore
        else
        {
            if( token.type == Tk::Type::StringLiteral() )
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

internal Token RequireToken( Lexer* lexer, Tk::Type const& wantedType )
{
    Token token = GetToken( lexer );

    if( token.type != wantedType )
        ERROR( token, "Unexpected token type (wanted '%s', got '%s')",
               wantedType.value.displayName, token.type.value.displayName );

    return token;
}

internal Token PeekToken( Lexer* lexer )
{
    Lexer temp = *lexer;
    Token result = GetToken( &temp );
    return result;
}

//internal bool OptionalToken( Lexer* lexer, u32 wantedType )
//{
    //Token token = GetToken( lexer );
    //return token.type == wantedType;
//}

internal bool MatchKeyword( Token const& token, Keyword const& k )
{
    return token.ident == globalKeywords[ k.index ];
}

#if !CFG_RELEASE
void DebugDumpScan( String const& program, char const* filename )
{
    Lexer lexer = Lexer( program, filename );

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
            case Tk::Type::EndOfStream().index:
            {
                parsing = false;
            } break;
        }
    }
}
#endif

#undef ERROR

