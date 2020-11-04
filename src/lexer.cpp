//#include "common.h"

#if 1
#define KEYWORDS(x ) \
    x( None,    "" ) \
    x( Struct,  "struct" ) \
    x( Enum,    "enum" ) \
    x( Sizeof,  "sizeof" ) \

STRUCT_ENUM_WITH_NAMES(Keyword, KEYWORDS)
#undef KEYWORDS

#else
struct Keyword
{
    enum Enum : i32
    {
        None, Struct, Enum, Sizeof,
    };

    char const* name;
    i32 value;
    i32 index;

    bool operator ==( Keyword const& other ) const { return index == other.index; }
    bool operator !=( Keyword const& other ) const { return index != other.index; }
    struct Values;
};
struct Keyword::Values
{
    using EnumName = Keyword;
    using ValueType = i32;

    static constexpr Keyword items[] =
    {
        { "", ValueType(), (i32)EnumName::None },
        { "struct", ValueType(), (i32)EnumName::Struct },
        { "enum", ValueType(), (i32)EnumName::Enum },
        { "sizeof", ValueType(), (i32)EnumName::Sizeof },
    }; 
    static constexpr char const* const names[] = 
    { 
        items[EnumName::None].name, 
        items[EnumName::Struct].name, 
        items[EnumName::Enum].name,
        items[EnumName::Sizeof].name,
    };
    static constexpr int count = (sizeof(items) / sizeof((items)[0]));

    static constexpr EnumName const& None = items[EnumName::None];
    static constexpr EnumName const& Struct = items[EnumName::Struct];
    static constexpr EnumName const& Enum = items[EnumName::Enum];
    static constexpr EnumName const& Sizeof = items[EnumName::Sizeof];
};

#endif

internal int globalSymbolLUT[128];
internal char const* globalKeywords[Keyword::Values::count];



internal bool InternsAreEqual( InternString const& a, InternString const& b )
{
    return a.hash == b.hash
        && StringsEqual( String( a.data, a.length ), String( b.data, b.length ) );
}

internal InternString* Intern( String const& string )
{
    InternString* result = nullptr;
    InternString intern = { string.data, Hash32( string.data, string.length ), I16( string.length ) };

    InternString* entry = globalInternStrings.entries.Find( intern, InternsAreEqual );
    if( entry )
        result = entry;
    else
    {
        char* stringData = PUSH_STRING( &globalInternStrings.arena, string.length + 1 );
        string.CopyToNullTerminated( stringData );

        intern.data = stringData;
        result = globalInternStrings.entries.Push( intern );
    }
    
    return result;
}

struct Lexer
{
    SourcePos pos;
    Token token;
    String stream;
    char at[2];

    bool error;

    Lexer( String const& input, char const* filename_ )
        : stream( input )
    {
        pos = { filename_, 1, 1 };

        // Init symbol look up table
        globalSymbolLUT['!'] = TokenKind::Exclamation;
        globalSymbolLUT['#'] = TokenKind::Pound;
        globalSymbolLUT['$'] = TokenKind::Dollar;
        globalSymbolLUT['%'] = TokenKind::Percent;
        globalSymbolLUT['&'] = TokenKind::Ampersand;
        globalSymbolLUT['\''] = TokenKind::SingleQuote;
        globalSymbolLUT['('] = TokenKind::OpenParen;
        globalSymbolLUT[')'] = TokenKind::CloseParen;
        globalSymbolLUT['*'] = TokenKind::Asterisk;
        globalSymbolLUT['+'] = TokenKind::Plus;
        globalSymbolLUT[','] = TokenKind::Comma;
        globalSymbolLUT['-'] = TokenKind::Minus;
        globalSymbolLUT['.'] = TokenKind::Dot;
        //globalSymbolLUT['/'] = TokenKind::Slash;
        globalSymbolLUT[':'] = TokenKind::Colon;
        globalSymbolLUT[';'] = TokenKind::Semicolon;
        globalSymbolLUT['<'] = TokenKind::LessThan;
        globalSymbolLUT['='] = TokenKind::Equal;
        globalSymbolLUT['>'] = TokenKind::GreaterThan;
        globalSymbolLUT['?'] = TokenKind::Question;
        globalSymbolLUT['@'] = TokenKind::At;
        globalSymbolLUT['['] = TokenKind::OpenBracket;
        globalSymbolLUT['\\'] = TokenKind::Backslash;
        globalSymbolLUT[']'] = TokenKind::CloseBracket;
        globalSymbolLUT['^'] = TokenKind::Caret;
        globalSymbolLUT['_'] = TokenKind::Underscore;
        globalSymbolLUT['`'] = TokenKind::BackTick;
        globalSymbolLUT['{'] = TokenKind::OpenBrace;
        globalSymbolLUT['|'] = TokenKind::Pipe;
        globalSymbolLUT['}'] = TokenKind::CloseBrace;
        globalSymbolLUT['~'] = TokenKind::Tilde;

        // Intern keywords
        for( Keyword const& k : Keyword::Values::items )
        {
            globalKeywords[k.index] = Intern( String( k.name ) )->data;
        }

        Refill();
    }

    void Advance( int count = 1 )
    {
        if( count > stream.length )
            count = stream.length;

        stream.length -= count;
        stream.data += count;

        pos.columnNumber += count;

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

internal void Error( Lexer* lexer, Token const& onToken, char const* fmt, ... )
{
    va_list arg_list;
    va_start( arg_list, fmt );

    vfprintf( stderr, fmt, arg_list );
    va_end( arg_list );

    lexer->error = true;
    globalRunning = false;
}

#define PARSE_ERROR( token, msg, ... ) Error( lexer, token, "%s(%d,%d): "msg, \
                                        token.pos.filename, token.pos.lineNumber, token.pos.columnNumber, ##__VA_ARGS__ );

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

    lexer->pos.lineNumber++;
    lexer->pos.columnNumber = 1;
}

internal Token NextTokenRaw( Lexer* lexer )
{
    Token token = {};
    token.text = lexer->stream;
    token.pos = lexer->pos;

    char c = lexer->at[0];
    // TODO Remove this and go back to only advancing when necessary to increase the amount of advances by more than 1 char
    lexer->Advance();

    // First use the lookup for symbols
    int lookupIndex = globalSymbolLUT[c];
    if( lookupIndex )
        token.kind = (TokenKind::Enum)lookupIndex;
    else
    {
        switch( c )
        {
            case '\0':
            {
                token.kind = TokenKind::EndOfStream;
            } break;

            case '"':
            {
                token.kind = TokenKind::StringLiteral;

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
                    token.kind = TokenKind::Comment;
                    lexer->Advance();

                    while( lexer->at[0] && !IsNewline( lexer->at[0] ) )
                        lexer->Advance();
                }
                // C style
                else if( lexer->at[0] == '*' )
                {
                    token.kind = TokenKind::Comment;
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
                    token.kind = TokenKind::Spacing;

                    while( IsSpacing( lexer->at[0] ) )
                        lexer->Advance();
                }
                else if( IsNewline( c ) )
                {
                    token.kind = TokenKind::Newline;
                    AdvanceNewline( lexer, c, lexer->at[0] );
                }
                else if( IsAlpha( c ) )
                {
                    token.kind = TokenKind::Name;

                    while( IsAlpha( lexer->at[0] ) || IsNumber( lexer->at[0] ) || lexer->at[0] == '_' )
                        lexer->Advance();

                    int length = I32( lexer->stream.data - token.text.data );
                    InternString* intern = Intern( String( token.text.data, length ) );
                    token.ident = intern->data;

                    if( intern->flags & InternString::Keyword )
                        token.kind = TokenKind::Keyword;
                }
                // TODO 
                //else if( IsNumeric( c ) )
                //{
                    //token.kind = TokenKind::NumericLiteral;

                    //ParseNumber( lexer );
                //}
                else
                    token.kind = TokenKind::Unknown;

            } break;
        }
    }
    token.text.length = I32( lexer->stream.data - token.text.data );

    return token;
}

Token const& NextToken( Lexer* lexer )
{
    Token& token = lexer->token;
    while( true )
    {
        token = NextTokenRaw( lexer );
        if( token.kind == TokenKind::Spacing ||
            token.kind == TokenKind::Newline ||
            token.kind == TokenKind::Comment )
        {} // Ignore
        else
        {
            if( token.kind == TokenKind::StringLiteral )
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

Token RequireToken( TokenKind::Enum wantedType, Lexer* lexer )
{
    Token token = NextToken( lexer );

    if( token.kind != wantedType )
    {
        PARSE_ERROR( token, "Unexpected token type (wanted '%s', got '%s')",
               TokenKind::Values::names[wantedType], TokenKind::Values::names[token.kind] );
    }

    return token;
}

void EnsureToken( TokenKind::Enum wantedKind, Token const& token, Lexer* lexer )
{
    if( token.kind != wantedKind )
    {
        PARSE_ERROR( token, "Unexpected token type (wanted '%s', got '%s')",
               TokenKind::Values::names[wantedKind], TokenKind::Values::names[token.kind] );
    }
}

bool MatchToken( TokenKind::Enum wantedKind, Token const& token )
{
    return token.kind == wantedKind;
}

bool MatchKeyword( int kw, Token const& token )
{
    return token.ident == globalKeywords[ kw ];
}

internal Token PeekToken( Lexer* lexer )
{
    Lexer temp = *lexer;
    Token result = NextToken( &temp );
    return result;
}

#if !CFG_RELEASE
void DebugDumpScan( String const& program, char const* filename )
{
    Lexer lexer = Lexer( program, filename );

    bool parsing = true;
    while( parsing )
    {
        Token token = NextToken( &lexer );
        switch( token.kind )
        {
            default:
            {
                globalPlatform.Print( "%s - %.*s\n", TokenKind::Values::items[token.kind].value.shortName, token.text.length, token.text.data );
            } break;
            case TokenKind::EndOfStream:
            {
                parsing = false;
            } break;
        }
    }
}
#endif

