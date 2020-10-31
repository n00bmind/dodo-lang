
internal bool globalParsing = true;

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
        switch( token.type.index )
        {
            case Tk::Type::EndOfStream().index:
                break;

            case Tk::Type::Identifier().index:
            {
                if( MatchKeyword( token, Keyword::Struct() ) )
                {
                    Token ident = RequireToken( &lexer, Tk::Type::Identifier() );
                    RequireToken( &lexer, Tk::Type::OpenBrace() );

                    ParseStruct( &lexer );
                }
                else if( MatchKeyword( token, Keyword::Enum() ) )
                {
                    Token ident = RequireToken( &lexer, Tk::Type::Identifier() );
                    RequireToken( &lexer, Tk::Type::OpenBrace() );

                    ParseEnum( &lexer );
                }
                else
                {
                    // Declaring var or function
                    ParseDeclaration( &lexer );
                }
            } break;

            default:
            {
                // TODO Error
            } break;
        }
    }
}
