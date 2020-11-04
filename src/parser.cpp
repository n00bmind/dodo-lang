TypeSpec* ParseBaseTypeSpec( Lexer* lexer )
{
    Token token = NextToken( lexer );
    SourcePos pos = token.pos;

    if( MatchToken( TokenKind::Name, token ) )
    {
        return NewNameTypeSpec( pos, token.ident );
    }

    return nullptr;
}

TypeSpec* ParseTypeSpec( Lexer* lexer )
{
    Token const& token = lexer->token;
    SourcePos const& pos = lexer->token.pos;

    TypeSpec* type = nullptr;
    if( MatchToken( TokenKind::OpenBracket, token ) )
    {

    }
    else if( MatchToken( TokenKind::Asterisk, token ) )
    {
        NextToken( lexer );
        TypeSpec* ofType = ParseTypeSpec( lexer );

        // TODO Associativity !?
        if( lexer->IsValid() )
            type = NewPtrTypeSpec( pos, ofType );
    }
    else
    {
        type = ParseBaseTypeSpec( lexer );
    }

    return type;
}
 

struct CompoundField
{
    enum Kind
    {
        Name,
        Index,
    };

    SourcePos pos;
    union
    {
        char const* name;
        Expr* index;
    };
    Expr *initValue;
    Kind kind;
};

Expr* ParseExpr( Lexer* lexer );

CompoundField ParseCompoundFieldExpr( Lexer* lexer )
{
    Token const& token = lexer->token;
    SourcePos const& pos = lexer->token.pos;

    CompoundField result = {};
    if( MatchToken( TokenKind::OpenBracket, token ) )
    {
        NextToken( lexer );
        Expr* indexExpr = ParseExpr( lexer );
        RequireToken( TokenKind::CloseBracket, lexer );
        RequireToken( TokenKind::Equal, lexer );
        Expr* valueExpr = ParseExpr( lexer );

        if( lexer->IsValid() )
        {
            // C++ suckzzzzz
            result.pos = pos;
            result.index = indexExpr;
            result.initValue = valueExpr;
            result.kind = CompoundField::Index;
        }
    }
    else if( MatchToken( TokenKind::Name, token ) )
    {
        char const* name = token.ident;
        NextToken( lexer );
        RequireToken( TokenKind::Equal, lexer );
        Expr* valueExpr = ParseExpr( lexer );

        if( lexer->IsValid() )
            result = { pos, name, valueExpr, CompoundField::Name };
    }
    else
        PARSE_ERROR( token, "Unrecognized initializer type in compound literal. Must be either a field name or an index" );

    return result;
}

Expr* ParseCompoundExpr( Lexer* lexer )
{
    SourcePos const& pos = lexer->token.pos;
    BucketArray<CompoundField> fields( &globalTmpArena, 16 );

    RequireToken( TokenKind::OpenBrace, lexer );
    while( !MatchToken( TokenKind::CloseBrace, lexer->token ) )
    {
        CompoundField field = ParseCompoundFieldExpr( lexer );
        fields.Push( field );

        if( !MatchToken( TokenKind::Comma, lexer->token ) )
            break;
    }
    RequireToken( TokenKind::CloseBrace, lexer );

    Expr* expr = nullptr;
    if( lexer->IsValid() )
        expr = NewCompoundExpr( pos, fields );
    return expr;
}

Expr* ParseBaseExpr( Lexer* lexer )
{
    Token const& token = lexer->token;
    SourcePos const& pos = token.pos;

    bool advance = true;
    Expr* expr = nullptr;
    if( MatchToken( TokenKind::Name, token ) )
    {
        expr = NewNameExpr( pos, token.ident );
    }
    else if( MatchToken( TokenKind::IntLiteral, token ) )
    {
        u64 val = token.intValue;
        // TODO Modifiers
        expr = NewIntExpr( pos, val );
    }
    else if( MatchToken( TokenKind::FloatLiteral, token ) )
    {
        f64 val = token.floatValue;
        expr = NewFloatExpr( pos, val );
    }
    else if( MatchToken( TokenKind::StringLiteral, token ) )
    {
        String val = token.text;
        expr = NewStringExpr( TokenKind::StringLiteral, val );
    }
    else if( MatchKeyword( Keyword::Sizeof, token ) )
    {
        NextToken( lexer );
        RequireToken( TokenKind::OpenParen, lexer );
        Expr* typeExpr = ParseExpr( lexer );
        RequireToken( TokenKind::CloseParen, lexer );
        advance = false;

        if( lexer->IsValid() )
            expr = NewSizeofExpr( pos, typeExpr );
    }
    else if( MatchToken( TokenKind::OpenBrace, token ) )
    {
        expr = ParseCompoundExpr( lexer );
        advance = false;
    }
    else if( MatchToken( TokenKind::OpenParen, token ) )
    {
        NextToken( lexer );
        expr = ParseExpr( lexer );
        RequireToken( TokenKind::CloseParen, lexer );
        advance = false;
    }
    else
        PARSE_ERROR( token, "Unexpected token '%s' in expression", token.kind.displayName );

    if( advance && lexer->IsValid() )
        NextToken( lexer );

    return expr;
}

Expr* ParsePostfixExpr( Lexer* lexer )
{
    Expr* expr = ParseBaseExpr( lexer );
    while( lexer->token.kind.HasFlag( Token::Flags::PostfixOp ) )
    {
        SourcePos pos = lexer->token.pos;
        if( MatchToken( TokenKind::OpenParen, lexer->token ) )
        {
            BucketArray<Expr*> args( &globalTmpArena, 16 );

            Token token = NextToken( lexer );
            if( !MatchToken( TokenKind::CloseParen, token ) )
            {
                args.Push( ParseExpr( lexer ) );
                while( MatchToken( TokenKind::Comma, lexer->token ) )
                    args.Push( ParseExpr( lexer ) );
            }
            RequireToken( TokenKind::CloseParen, lexer );

            if( lexer->IsValid() )
                expr = NewCallExpr( pos, expr, args );
        }
        else if( MatchToken( TokenKind::OpenBracket, lexer->token ) )
        {
            Expr* index = ParseExpr( lexer );
            RequireToken( TokenKind::CloseBracket, lexer );
            expr = NewIndexExpr( pos, expr, index );
        }
        else if( MatchToken( TokenKind::Dot, lexer->token ) )
        {
            Token field = RequireToken( TokenKind::Name, lexer );
            expr = NewFieldExpr( pos, expr, field.ident );
        }
    }
    
    return expr;
}

Expr* ParseUnaryExpr( Lexer* lexer )
{
    if( lexer->token.kind.HasFlag( Token::Flags::UnaryOp ) )
    {
        SourcePos pos = lexer->token.pos;
        TokenKind::Enum op = lexer->token.kind;
        NextToken( lexer );

        return NewUnaryExpr( pos, op, ParseUnaryExpr( lexer ) );
    }
    else
        return ParseBaseExpr( lexer );
}

Expr* ParseMulExpr( Lexer* lexer )
{
    Expr* expr = ParseUnaryExpr( lexer );
    while( lexer->token.kind.HasFlag( Token::Flags::MulOp ) )
    {
        SourcePos pos = lexer->token.pos;
        TokenKind::Enum op = lexer->token.kind;
        NextToken( lexer );

        expr = NewBinaryExpr( pos, op, expr, ParseUnaryExpr( lexer ) );
    }

    return expr;
}

Expr* ParseAddExpr( Lexer* lexer )
{
    Expr* expr = ParseMulExpr( lexer );
    while( lexer->token.kind.HasFlag( Token::Flags::AddOp ) )
    {
        SourcePos pos = lexer->token.pos;
        TokenKind::Enum op = lexer->token.kind;
        NextToken( lexer );

        expr = NewBinaryExpr( pos, op, expr, ParseMulExpr( lexer ) );
    }

    return expr;
}

Expr* ParseCmpExpr( Lexer* lexer )
{
    Expr* expr = ParseAddExpr( lexer );
    while( lexer->token.kind.HasFlag( Token::Flags::CmpOp ) )
    {
        SourcePos pos = lexer->token.pos;
        TokenKind::Enum op = lexer->token.kind;
        NextToken( lexer );

        expr = NewBinaryExpr( pos, op, expr, ParseAddExpr( lexer ) );
    }

    return expr;
}

Expr* ParseAndExpr( Lexer* lexer )
{
    Expr* expr = ParseCmpExpr( lexer );
    while( MatchToken( TokenKind::LogicAnd, lexer->token ) )
    {
        SourcePos pos = lexer->token.pos;
        NextToken( lexer );

        expr = NewBinaryExpr( pos, TokenKind::LogicAnd, expr, ParseCmpExpr( lexer ) );
    }

    return expr;
}

Expr* ParseOrExpr( Lexer* lexer )
{
    Expr* expr = ParseAndExpr( lexer );
    while( MatchToken( TokenKind::LogicOr, lexer->token ) )
    {
        SourcePos pos = lexer->token.pos;
        NextToken( lexer );

        expr = NewBinaryExpr( pos, TokenKind::LogicOr, expr, ParseAndExpr( lexer ) );
    }

    return expr;
}

Expr* ParseTernaryExpr( Lexer* lexer )
{
    Expr* expr = ParseOrExpr( lexer );
    if( MatchToken( TokenKind::Question, lexer->token ) )
    {
        NextToken( lexer );
        Expr* thenExpr = ParseTernaryExpr( lexer );
        RequireToken( TokenKind::Colon, lexer );
        Expr* elseExpr = ParseTernaryExpr( lexer );

        if( lexer->IsValid() )
            expr = NewTernaryExpr( expr->pos, expr, thenExpr, elseExpr );
    }
    return expr;
}

Expr* ParseExpr( Lexer* lexer )
{
    return ParseTernaryExpr( lexer );
}


Decl* ParseStruct( SourcePos const& pos, Lexer* lexer )
{
#if 0
    if( !lexer->IsValid() )
        return;

    Token token = NextToken( lexer );
    while( token.kind != TokenKind::CloseBrace );
    {

        token = NextToken( lexer );
    }
#endif
    return nullptr;
}

Decl* ParseEnum( SourcePos const& pos, Lexer* lexer )
{
#if 0
    if( !lexer->IsValid() )
        return;

    Token token = NextToken( lexer );
    while( token.kind != TokenKind::CloseBrace );
    {

        token = NextToken( lexer );
    }
#endif

    return nullptr;
}

Decl* ParseDecl( Lexer* lexer )
{
    Token token = lexer->token;
    SourcePos pos = token.pos;

    if( MatchToken( TokenKind::Keyword, token ) )
    {
        if( MatchKeyword( Keyword::Struct, token ) )
            return ParseStruct( pos, lexer );
        else if( MatchKeyword( Keyword::Enum, token ) )
            return ParseEnum( pos, lexer );
    }
    else
    {
        // Try parsing a var / const
        EnsureToken( TokenKind::Name, token, lexer );
        char const* name = token.ident;
        RequireToken( TokenKind::Colon, lexer );

        bool isConst = false;
        Expr* expr = nullptr;
        TypeSpec* type = nullptr;

        token = NextToken( lexer );
        if( MatchToken( TokenKind::Colon, token ) )
            // Const
            expr = ParseExpr( lexer );
        else
        {
            type = ParseTypeSpec( lexer );
            token = NextToken( lexer );
            if( MatchToken( TokenKind::Equal, lexer->token ) )
                expr = ParseExpr( lexer );
        }
        RequireToken( TokenKind::Semicolon, lexer );

        if( lexer->IsValid() )
        {
            return NewVarDecl( pos, name, type, expr, isConst );
        }
    }

    return nullptr;
}

Array<Decl*> Parse( String const& program, char const* filename )
{
    Lexer lexer = Lexer( program, filename );

    ScopedTmpMemory tmpMemory( &globalTmpArena );
    BucketArray<Decl*> decls( &globalTmpArena, 16, Temporary() );

    while( globalRunning )
    {
        Token token = NextToken( &lexer );
        if( token.kind == TokenKind::EndOfStream )
            break;

        Decl* decl = ParseDecl( &lexer );
        if( decl )
            decls.Push( decl );
    }

    Array<Decl*> result( &globalArena, decls.count );
    decls.CopyTo( &result );

    return result;
}
