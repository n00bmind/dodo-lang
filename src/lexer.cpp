
#define KEYWORDS(x ) \
    x( Struct,  "struct" ) \
    x( Enum,    "enum" ) \
    x( Sizeof,  "sizeof" ) \

STRUCT_ENUM_WITH_NAMES(Keyword, KEYWORDS)
#undef KEYWORDS

internal char const* globalKeywords[Keyword::Values::count];



internal bool InternsAreEqual( InternString const& a, InternString const& b )
{
    return a.hash == b.hash
        && StringsEqual( String( a.data, a.length ), String( b.data, b.length ) );
}

internal InternString* Intern( String const& string, u16 flags = 0 )
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
        intern.flags = flags;
        result = globalInternStrings.entries.Push( intern );
    }
    
    return result;
}

struct Lexer
{
    SourcePos pos;
    Token token;
    String stream;

    bool error;

    Lexer()
    {}
    Lexer( String const& input, char const* filename_ )
        : stream( input )
        , error( false )
    {
        pos = { filename_, 1, 1 };
        token = {};

        // Intern keywords
        for( Keyword const& k : Keyword::Values::items )
        {
            InternString* intern = Intern( String( k.name ), InternString::Keyword );
            globalKeywords[k.index] = intern->data;
        }
    }

    void Advance( int count = 1 )
    {
        ASSERT( count <= stream.length );

        stream.length -= count;
        stream.data += count;

        pos.columnNumber += count;
    }

    bool IsValid()
    {
        return !error;
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

internal void ScanInt( Lexer* lexer )
{
    // TODO 
}

internal void ScanFloat( Lexer* lexer )
{
    // TODO 
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
    char const*& stream = lexer->stream.data;

    Token token = {};
    token.text = lexer->stream;
    token.pos = lexer->pos;

    switch( stream[0] )
    {
        case '\0':
        {
            token.kind = TokenKind::EndOfStream;
        } break;

        case '\r': case '\n':
        {
            char c0 = stream[0];
            char c1 = stream[1];

            token.kind = TokenKind::Newline;
            lexer->Advance();

            AdvanceNewline( lexer, c0, c1 );
        } break;

#define CASE(c) case c:

#define SPACING(x) x(' ') x('\t') x('\f') x('\v')
        SPACING(CASE)
        {
            lexer->Advance();
            token.kind = TokenKind::Spacing;
            while( IsSpacing( stream[0] ) )
                lexer->Advance();
        } break;
#undef SPACING

#define DIGITS(x) x('0') x('1') x('2') x('3') x('4') x('5') x('6') x('7') x('8') x('9')
        DIGITS(CASE)
        {
            char const* c = stream + 1;
            while( IsNumber( *c ) )
                c++;

            if( *c == '.' || *c == 'e' || *c == 'E' )
                ScanFloat( lexer );
            else
                ScanInt( lexer );
        } break;
#undef DIGITS

#define IDENT(x) \
        x('a') x('b') x('c') x('d') x('e') x('f') x('g') x('h') x('i') x('j') x('k') x('l') x('m') \
        x('n') x('o') x('p') x('q') x('r') x('s') x('t') x('u') x('v') x('w') x('x') x('y') x('z') \
        x('A') x('B') x('C') x('D') x('E') x('F') x('G') x('H') x('I') x('J') x('K') x('L') x('M') \
        x('N') x('O') x('P') x('Q') x('R') x('S') x('T') x('U') x('V') x('W') x('X') x('Y') x('Z') x('_')
        IDENT(CASE)
        {
            token.kind = TokenKind::Name;
            lexer->Advance();

            while( IsAlpha( stream[0] ) || IsNumber( stream[0] ) || stream[0] == '_' )
                lexer->Advance();

            int length = I32( lexer->stream.data - token.text.data );
            InternString* intern = Intern( String( token.text.data, length ) );
            token.ident = intern->data;

            if( intern->flags & InternString::Keyword )
                token.kind = TokenKind::Keyword;
        } break;
#undef IDENT

#undef CASE
        // TODO Chars
        case '"':
        {
            lexer->Advance();
            token.kind = TokenKind::StringLiteral;

            while( stream[0] && stream[0] != '"' )
            {
                // TODO 
                // Skip escape sequences
                if( stream[0] == '\\' && stream[1] )
                {
                    lexer->Advance();
                }
                lexer->Advance();
            }
            // Skip last quote
            if( stream[0] )
                lexer->Advance();
        } break;

        case '/':
        {
            // C++ style comment
            if( stream[1] == '/' )
            {
                token.kind = TokenKind::Comment;
                lexer->Advance( 2 );

                while( stream[0] && !IsNewline( stream[0] ) )
                    lexer->Advance();
            }
            // C style comment
            else if( stream[1] == '*' )
            {
                token.kind = TokenKind::Comment;
                lexer->Advance( 2 );

                while( stream[0] && !(stream[0] == '*' && stream[1] == '/') )
                {
                    // NOTE This is slightly weird in that we potentially advance the line number before skipping over a multi-byte newline
                    if( IsNewline( stream[0] ) )
                    {
                        AdvanceNewline( lexer, stream[0], stream[1] );
                    }
                    lexer->Advance();
                }

                if( stream[0] )
                    lexer->Advance( 2 );
            }
            else
            {
                token.kind = TokenKind::Slash;
                lexer->Advance();

                if( stream[0] == '=' )
                {
                    token.kind = TokenKind::DivAssign;
                    lexer->Advance();
                }
            }
        } break;

        case '.':
        {
            if( IsNumber( stream[1] ) )
                ScanFloat( lexer );
            else
            {
                token.kind = TokenKind::Dot;
                lexer->Advance();
            }
        } break;

        case '<':
        {
            token.kind = TokenKind::LessThan;
            lexer->Advance();
            if( stream[0] == '<' )
            {
                token.kind = TokenKind::LeftShift;
                lexer->Advance();
                if( stream[0] == '=' )
                {
                    token.kind = TokenKind::LShiftAssign;
                    lexer->Advance();
                }
            }
            else if( stream[0] == '=' )
            {
                token.kind = TokenKind::LTEqual;
                lexer->Advance();
            }
        } break;

        case '>':
        {
            token.kind = TokenKind::GreaterThan;
            lexer->Advance();
            if( stream[0] == '>' )
            {
                token.kind = TokenKind::RightShift;
                lexer->Advance();
                if( stream[0] == '=' )
                {
                    token.kind = TokenKind::RShiftAssign;
                    lexer->Advance();
                }
            }
            else if( stream[0] == '=' )
            {
                token.kind = TokenKind::GTEqual;
                lexer->Advance();
            }
        } break;

#define CASE1(c1, k1)                      \
        case c1:                       \
                                       token.kind = k1;           \
        lexer->Advance();          \
        break;

#define CASE2(c1, k1, c2, k2)              \
        case c1:                       \
                                       token.kind = k1;           \
        lexer->Advance();          \
        if( stream[0] == c2 )      \
        {                          \
            token.kind = k2;       \
            lexer->Advance();      \
        }                          \
        break;

#define CASE3(c1, k1, c2, k2, c3, k3)      \
        case c1:                       \
                                       token.kind = k1;           \
        lexer->Advance();          \
        if( stream[0] == c2 )      \
        {                          \
            token.kind = k2;       \
            lexer->Advance();      \
        }                          \
        else if( stream[0] == c3 ) \
        {                          \
            token.kind = k3;       \
            lexer->Advance();      \
        }                          \
        break;

        CASE1( '(', TokenKind::OpenParen );
        CASE1( ')', TokenKind::CloseParen );
        CASE1( '[', TokenKind::OpenBracket );
        CASE1( ']', TokenKind::CloseBracket );
        CASE1( '{', TokenKind::OpenBrace );
        CASE1( '}', TokenKind::CloseBrace );
        CASE1( ',', TokenKind::Comma );
        CASE1( '@', TokenKind::At );
        CASE1( '#', TokenKind::Pound );
        CASE1( '?', TokenKind::Question );
        CASE1( ';', TokenKind::Semicolon );
        CASE1( '~', TokenKind::Tilde );
        CASE2( '=', TokenKind::Assign, '=', TokenKind::Equal );
        CASE2( '!', TokenKind::Exclamation, '=', TokenKind::NotEqual );
        CASE2( ':', TokenKind::Colon, '=', TokenKind::ColonAssign );
        CASE2( '+', TokenKind::Plus, '=', TokenKind::PlusAssign );
        CASE2( '-', TokenKind::Minus, '=', TokenKind::MinusAssign );
        CASE2( '*', TokenKind::Asterisk, '=', TokenKind::MulAssign );
        CASE2( '%', TokenKind::Percent, '=', TokenKind::ModAssign );
        CASE2( '^', TokenKind::Caret, '=', TokenKind::XorAssign );
        CASE3( '&', TokenKind::Ampersand, '=', TokenKind::AndAssign, '&', TokenKind::LogicAnd );
        CASE3( '|', TokenKind::Pipe, '=', TokenKind::OrAssign, '|', TokenKind::LogicOr );

#undef CASE1
#undef CASE2
#undef CASE3

        default:
        {
            token.kind = TokenKind::Unknown;
            PARSE_ERROR( token, "Invalid token '%c', skipping", stream[0] );

            lexer->Advance();
        } break;
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

