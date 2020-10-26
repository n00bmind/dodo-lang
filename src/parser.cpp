
enum class Keyword
{
    None = 0,
    Struct,
    Enum,

    COUNT
};

static bool globalParsing = true;

Keyword FindKeyword( Token const& token )
{
    // TODO Intern strings
    if( token.ident < (int)Keyword::COUNT )
    {

    }
    return Keyword::None;
}

void ParseStruct( Lexer* lexer )
{
    Token token = GetToken( lexer );
    while( token.type != Tk::Type::CloseBrace() );
    {

        token = GetToken( lexer );
    }
}

void ParseEnum( Lexer* lexer )
{
    Token token = GetToken( lexer );
    while( token.type != Tk::Type::CloseBrace() );
    {

        token = GetToken( lexer );
    }
}

void ParseDeclaration( Lexer* lexer )
{
    Token token = GetToken( lexer );
    while( token.type != Tk::Type::Semicolon() );
    {

        token = GetToken( lexer );
    }
}

void Parse( String const& program, char const* filename )
{
    Lexer lexer = Lexer( program, filename );

    while( globalParsing )
    {
        Token token = GetToken( &lexer );
        if( token.type == Tk::Type::EndOfStream() )
            break;
        else if( token.type == Tk::Type::Identifier() )
        {
            if( Keyword kw = FindKeyword( token ) )
            {
                switch( kw )
                {
                    case Keyword::Struct:
                    {
                        Token ident = RequireToken( &lexer, Tk::Type::Identifier() );
                        RequireToken( &lexer, Tk::Type::OpenBrace() );

                        ParseStruct( &lexer );
                    } break;
                    case Keyword::Enum:
                    {
                        Token ident = RequireToken( &lexer, Tk::Type::Identifier() );
                        RequireToken( &lexer, Tk::Type::OpenBrace() );

                        ParseEnum( &lexer );
                    } break;
                }
            }
            else
            {
                // Declaring var or function
                ParseDeclaration( &lexer );
            }
        }
        else
        {
            // TODO Error
        }
    }
}
