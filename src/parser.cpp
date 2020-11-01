
void ParseStruct( Lexer* lexer )
{
    if( !lexer->IsValid() )
        return;

    Token token = GetToken( lexer );
    while( token.kind != TokenKind::CloseBrace() );
    {

        token = GetToken( lexer );
    }
}

void ParseEnum( Lexer* lexer )
{
    if( !lexer->IsValid() )
        return;

    Token token = GetToken( lexer );
    while( token.kind != TokenKind::CloseBrace() );
    {

        token = GetToken( lexer );
    }
}

void ParseDeclaration( Lexer* lexer )
{
    if( !lexer->IsValid() )
        return;

    Token token = GetToken( lexer );
    while( token.kind != TokenKind::Semicolon() );
    {

        token = GetToken( lexer );
    }
}

void Parse( String const& program, char const* filename )
{
    Lexer lexer = Lexer( program, filename );

    while( globalRunning )
    {
        Token token = GetToken( &lexer );
        switch( token.kind.index )
        {
            case TokenKind::EndOfStream().index:
                break;

            case TokenKind::Identifier().index:
            {
                if( MatchKeyword( token, Keyword::Struct() ) )
                {
                    Token ident = RequireToken( &lexer, TokenKind::Identifier() );
                    RequireToken( &lexer, TokenKind::OpenBrace() );

                    ParseStruct( &lexer );
                }
                else if( MatchKeyword( token, Keyword::Enum() ) )
                {
                    Token ident = RequireToken( &lexer, TokenKind::Identifier() );
                    RequireToken( &lexer, TokenKind::OpenBrace() );

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
