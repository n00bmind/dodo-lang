#define KEYWORDS(x ) \
    x( None,    "" ) \
    x( Struct,  "struct" ) \
    x( Enum,    "enum" ) \

STRUCT_ENUM_WITH_NAMES(Keyword, KEYWORDS)
#undef KEYWORDS


internal int globalSymbolLUT[128];
internal char const* globalKeywords[Keyword::Values::count];



internal bool InternsAreEqual( InternString const& a, InternString const& b )
{
    return a.hash == b.hash
        && StringsEqual( a.str, b.str );
}

internal char const* Intern( String const& string )
{
    char const* result = nullptr;
    InternString intern = { String( string.data, string.length ), Hash32( string.data, string.length ) };

    InternString* entry = globalInternStrings.entries.Find( intern, InternsAreEqual );
    if( entry )
        result = entry->str.data;
    else
    {
        char* stringData = PUSH_STRING( &globalInternStrings.arena, string.length + 1 );
        string.CopyToNullTerminated( stringData );

        intern.str.data = stringData;
        globalInternStrings.entries.Push( intern );

        result = stringData;
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
        globalSymbolLUT['!'] = TokenKind::Exclamation().index;
        globalSymbolLUT['#'] = TokenKind::Pound().index;
        globalSymbolLUT['$'] = TokenKind::Dollar().index;
        globalSymbolLUT['%'] = TokenKind::Percent().index;
        globalSymbolLUT['&'] = TokenKind::Ampersand().index;
        globalSymbolLUT['\''] = TokenKind::SingleQuote().index;
        globalSymbolLUT['('] = TokenKind::OpenParen().index;
        globalSymbolLUT[')'] = TokenKind::CloseParen().index;
        globalSymbolLUT['*'] = TokenKind::Asterisk().index;
        globalSymbolLUT['+'] = TokenKind::Plus().index;
        globalSymbolLUT[','] = TokenKind::Comma().index;
        globalSymbolLUT['-'] = TokenKind::Minus().index;
        globalSymbolLUT['.'] = TokenKind::Dot().index;
        //globalSymbolLUT['/'] = TokenKind::Slash().index;
        globalSymbolLUT[':'] = TokenKind::Colon().index;
        globalSymbolLUT[';'] = TokenKind::Semicolon().index;
        globalSymbolLUT['<'] = TokenKind::LessThan().index;
        globalSymbolLUT['='] = TokenKind::Equal().index;
        globalSymbolLUT['>'] = TokenKind::GreaterThan().index;
        globalSymbolLUT['?'] = TokenKind::Question().index;
        globalSymbolLUT['@'] = TokenKind::At().index;
        globalSymbolLUT['['] = TokenKind::OpenBracket().index;
        globalSymbolLUT['\\'] = TokenKind::Backslash().index;
        globalSymbolLUT[']'] = TokenKind::CloseBracket().index;
        globalSymbolLUT['^'] = TokenKind::Caret().index;
        globalSymbolLUT['_'] = TokenKind::Underscore().index;
        globalSymbolLUT['`'] = TokenKind::BackTick().index;
        globalSymbolLUT['{'] = TokenKind::OpenBrace().index;
        globalSymbolLUT['|'] = TokenKind::Pipe().index;
        globalSymbolLUT['}'] = TokenKind::CloseBrace().index;
        globalSymbolLUT['~'] = TokenKind::Tilde().index;

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
    globalRunning = false;
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
        token.kind = TokenKind::Values::items[lookupIndex];
    else
    {
        switch( c )
        {
            case '\0':
            {
                token.kind = TokenKind::EndOfStream();
            } break;

            case '"':
            {
                token.kind = TokenKind::StringLiteral();

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
                    token.kind = TokenKind::Comment();
                    lexer->Advance();

                    while( lexer->at[0] && !IsNewline( lexer->at[0] ) )
                        lexer->Advance();
                }
                // C style
                else if( lexer->at[0] == '*' )
                {
                    token.kind = TokenKind::Comment();
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
                    token.kind = TokenKind::Spacing();

                    while( IsSpacing( lexer->at[0] ) )
                        lexer->Advance();
                }
                else if( IsNewline( c ) )
                {
                    token.kind = TokenKind::Newline();
                    AdvanceNewline( lexer, c, lexer->at[0] );
                }
                else if( IsAlpha( c ) )
                {
                    token.kind = TokenKind::Identifier();

                    while( IsAlpha( lexer->at[0] ) || IsNumber( lexer->at[0] ) || lexer->at[0] == '_' )
                        lexer->Advance();

                    int length = I32( lexer->stream.data - token.text.data );
                    token.ident = Intern( String( token.text.data, length ) );
                }
                else if( IsNumeric( c ) )
                {
                    token.kind = TokenKind::NumericLiteral();

                    ParseNumber( lexer );
                }
                else
                    token.kind = TokenKind::Unknown();

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
        if( token.kind == TokenKind::Spacing() ||
            token.kind == TokenKind::Newline() ||
            token.kind == TokenKind::Comment() )
        {} // Ignore
        else
        {
            if( token.kind == TokenKind::StringLiteral() )
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

internal Token RequireToken( Lexer* lexer, TokenKind const& wantedType )
{
    Token token = GetToken( lexer );

    if( token.kind != wantedType )
    {
        ERROR( token, "Unexpected token type (wanted '%s', got '%s')",
               wantedType.value.displayName, token.kind.value.displayName );
    }

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
    //return token.kind == wantedType;
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
        switch( token.kind.index )
        {
            default:
            {
                globalPlatform.Print( "%s - %.*s\n", token.kind.value.shortName, token.text.length, token.text.data );
            } break;
            case TokenKind::EndOfStream().index:
            {
                parsing = false;
            } break;
        }
    }
}
#endif

#undef ERROR

