
struct Token
{
    enum : u32
    {
        Unknown = 0,

        Identifier,
        StringLiteral,
        NumericLiteral,

        EndOfStream
    };

    String text;
    u32 type; 
};

struct Lexer
{
    char const* at;

    Lexer( char const* stream )
    {
        at = stream;
    }
};

static bool IsNewline( char c )
{
    bool result = (c == '\n' || c == '\r' );
    return result;
}

static bool IsWhitespace( char c )
{
    bool result = (c == ' ' ||
                   c == '\t' ||
                   c == '\f' ||
                   c == '\v' ||
                   IsNewline( c ));
    return result;
}

static bool IsAlpha( char c )
{
    bool result = (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z' );
    return result;
}

static bool IsNumber( char c )
{
    bool result = (c >= '0' && c <= '9');
    return result;
}

static bool IsNumeric( char c )
{
    // TODO
    //NOT_IMPLEMENTED
    return false;
}

static void EatAllWhitespace( Lexer* lexer )
{
    while( true )
    {
        if( IsWhitespace( lexer->at[0] ) )
            lexer->at++;
        else if( lexer->at[0] == '/' && lexer->at[1] == '/' )
        {
            lexer->at += 2;
            while( *lexer->at && !IsNewline( lexer->at[0] ) )
                ++lexer->at;
        }
        else if( lexer->at[0] == '/' && lexer->at[1] == '*' )
        {
            lexer->at += 2;
            while( *lexer->at && !(lexer->at[0] == '*' && lexer->at[1] == '/') )
                ++lexer->at;

            if( *lexer->at )
                lexer->at += 2;
        }
        else
            break;
    }
}

static void ParseNumber( Lexer* lexer )
{

}

static Token GetToken( Lexer* lexer )
{
    EatAllWhitespace( lexer );

    Token token = {};
    token.text.data = lexer->at;
    token.text.length = 1;

    char c = *lexer->at++;
    switch( c )
    {
        case '\0':
        {
            token.type = Token::EndOfStream;
        } break;

        case '"':
        {
            token.type = Token::StringLiteral;

            token.text.data = lexer->at;
            while( *lexer->at && lexer->at[0] != '"' )
            {
                // Skip scaped sequences
                if( lexer->at[0] == '\\' && lexer->at[1] )
                {
                    ++lexer->at;
                }
                ++lexer->at;
            }
            token.text.length = I32( lexer->at - token.text.data );
            // Skip last quote
            if( *lexer->at )
                ++lexer->at;
        }
        default:
        {
            if( IsAlpha( c ) )
            {
                token.type = Token::Identifier;

                while( IsAlpha( lexer->at[0] ) || IsNumber( lexer->at[0] ) || lexer->at[0] == '_' )
                    ++lexer->at;

                token.text.length = I32( lexer->at - token.text.data );
            }
            else if( IsNumeric( c ) )
            {
                token.type = Token::NumericLiteral;

                ParseNumber( lexer );
            }
            else
                token.type = Token::Unknown;

        } break;
    }

    return token;
}

static bool RequireToken( Lexer* lexer, u32 wantedType )
{
    Token token = GetToken( lexer );
    bool result = token.type == wantedType;
    return result;
}

void Parse( String const& program )
{
    Lexer lexer = Lexer( program.data );

    bool parsing = true;
    while( parsing )
    {
        Token token = GetToken( &lexer );
        switch( token.type )
        {
            default:
            {
                globalPlatform.Print( "%d - %.*s\n", token.type, token.text.length, token.text.data );
            } break;
            case Token::EndOfStream:
            {
                parsing = false;
            } break;
        }
    }
}
