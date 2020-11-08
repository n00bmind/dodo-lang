TypeSpec* NewTypeSpec( SourcePos const& pos, TypeSpec::Kind kind )
{
    TypeSpec* result = PUSH_STRUCT( &globalArena, TypeSpec );
    result->pos = pos;
    result->kind = TypeSpec::Name;

    return result;
}

TypeSpec* NewNameTypeSpec( SourcePos const& pos, char const* name )
{
    TypeSpec* result = NewTypeSpec( pos, TypeSpec::Name );
    result->name = name;

    return result;
}

TypeSpec* NewPtrTypeSpec( SourcePos const& pos, TypeSpec* ofType )
{
    TypeSpec* result = NewTypeSpec( pos, TypeSpec::Pointer );
    result->base = ofType;

    return result;
}

TypeSpec* ParseBaseTypeSpec( Lexer* lexer )
{
    Token const& token = lexer->token;
    SourcePos const& pos = token.pos;

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
 
Expr* NewExpr( SourcePos const& pos, Expr::Kind kind )
{
    Expr* result = PUSH_STRUCT( &globalArena, Expr );
    result->pos = pos;
    result->kind = kind;

    return result;
}

Expr* NewCompoundExpr( SourcePos const& pos, BucketArray<CompoundField> const& fields )
{
    Expr* result = NewExpr( pos, Expr::Compound );
    new( &result->compound.fields ) Array<CompoundField>( &globalArena, fields.count );
    fields.CopyTo( &result->compound.fields );

    return result;
}

Expr* NewNameExpr( SourcePos const& pos, char const* name )
{
    Expr* result = NewExpr( pos, Expr::Name );
    result->name = name;

    return result;
}

Expr* NewIntExpr( SourcePos const &pos, u64 value, Token::LiteralMod modifier )
{
    Expr* result = NewExpr( pos, Expr::Int );
    result->literal.intValue = value;
    result->literal.modifier = modifier;

    return result;
}

Expr* NewFloatExpr( SourcePos const &pos, f64 value, Token::LiteralMod modifier )
{
    Expr* result = NewExpr( pos, Expr::Float );
    result->literal.floatValue = value;
    result->literal.modifier = modifier;

    return result;
}

Expr* NewStringExpr( SourcePos const &pos, String const& value, Token::LiteralMod modifier )
{
    Expr* result = NewExpr( pos, Expr::Str );
    result->literal.strValue = value;
    result->literal.modifier = modifier;

    return result;
}

Expr* NewSizeofExpr( SourcePos const& pos, Expr* expr )
{
    Expr* result = NewExpr( pos, Expr::Sizeof );
    result->sizeof_.expr = expr;

    return result;
}

Expr* NewCallExpr( SourcePos const& pos, Expr* expr, BucketArray<Expr*> const& args )
{
    Expr* result = NewExpr( pos, Expr::Call );
    result->call.func = expr;
    new( &result->call.args ) Array<Expr*>( &globalArena, args.count );
    args.CopyTo( &result->call.args );

    return result;
}

Expr* NewIndexExpr( SourcePos const& pos, Expr* base, Expr* index )
{
    Expr* result = NewExpr( pos, Expr::Index );
    result->index.base = base;
    result->index.index = index;

    return result;
}

Expr* NewFieldExpr( SourcePos const& pos, Expr* base, char const* name )
{
    Expr* result = NewExpr( pos, Expr::Field );
    result->field.base = base;
    result->field.name = name;

    return result;
}

Expr* NewUnaryExpr( SourcePos const& pos, TokenKind::Enum op, Expr* expr )
{
    Expr* result = NewExpr( pos, Expr::Unary );
    result->unary.expr = expr;
    result->unary.op = op;

    return result;
}

Expr* NewBinaryExpr( SourcePos const& pos, TokenKind::Enum op, Expr* left, Expr* right )
{
    Expr* result = NewExpr( pos, Expr::Binary );
    result->binary.left = left;
    result->binary.right = right;
    result->binary.op = op;

    return result;
}

Expr* NewTernaryExpr( SourcePos const& pos, Expr* cond, Expr* thenExpr, Expr* elseExpr )
{
    Expr* result = NewExpr( pos, Expr::Ternary );
    result->ternary.cond = cond;
    result->ternary.thenExpr = thenExpr;
    result->ternary.elseExpr = elseExpr;

    return result;
}

// NOTE All ParseXXXExpr() functions leave the token "cursor" at the next immediate token after the expression
// (This is required as sometimes we don't know whether an expression has finished until we find a token that's not part of it)
// (I guess we could PeekToken() in those cases, but this seems simpler?)
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
        NextToken( lexer );
        RequireToken( TokenKind::Assign, lexer );
        NextToken( lexer );
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
    else
    {
        Expr* expr = ParseExpr( lexer );
        if( MatchToken( TokenKind::Assign, token ) )
        {
            if( expr->kind == Expr::Name )
            {
                NextToken( lexer );
                Expr* valueExpr = ParseExpr( lexer );

                if( lexer->IsValid() )
                    result = { pos, expr->name, valueExpr, CompoundField::Name };
            }
            else
                PARSE_ERROR( token, "Unrecognized initializer type in compound literal. Must be either a field name or an index" );
        }
        else
            result = { pos, nullptr, expr, CompoundField::Default };
    }

    return result;
}

Expr* ParseCompoundExpr( Lexer* lexer )
{
    SourcePos const& pos = lexer->token.pos;
    BucketArray<CompoundField> fields( &globalTmpArena, 16 );

    RequireToken( TokenKind::OpenBrace, lexer );
    NextToken( lexer );
    while( !MatchToken( TokenKind::CloseBrace, lexer->token ) )
    {
        CompoundField field = ParseCompoundFieldExpr( lexer );
        fields.Push( field );

        if( !MatchToken( TokenKind::Comma, lexer->token ) )
            break;
        NextToken( lexer );
    }
    RequireToken( TokenKind::CloseBrace, lexer );
    NextToken( lexer );

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
        expr = NewIntExpr( pos, token.intValue, token.mod );
    }
    else if( MatchToken( TokenKind::FloatLiteral, token ) )
    {
        f64 val = token.floatValue;
        expr = NewFloatExpr( pos, token.floatValue, token.mod );
    }
    else if( MatchToken( TokenKind::StringLiteral, token ) )
    {
        expr = NewStringExpr( pos, token.strValue, token.mod );
    }
    else if( MatchKeyword( Keyword::Sizeof, token ) )
    {
        NextToken( lexer );
        RequireToken( TokenKind::OpenParen, lexer );
        NextToken( lexer );
        Expr* typeExpr = ParseExpr( lexer );
        RequireToken( TokenKind::CloseParen, lexer );

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
    }
    else
    {
        char const* desc = TokenKind::Values::names[ token.kind ];
        if( token.kind == TokenKind::Name || token.kind == TokenKind::Keyword )
            desc = token.ident;

        PARSE_ERROR( token, "Unexpected token '%s' in expression", desc );
    }

    if( advance && lexer->IsValid() )
        NextToken( lexer );

    return expr;
}

Expr* ParsePostfixExpr( Lexer* lexer )
{
    Expr* expr = ParseBaseExpr( lexer );

    while( lexer->token.HasFlag( TokenFlags::PostfixOp ) )
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
                {
                    NextToken( lexer );
                    args.Push( ParseExpr( lexer ) );
                }
            }
            RequireToken( TokenKind::CloseParen, lexer );

            if( lexer->IsValid() )
                expr = NewCallExpr( pos, expr, args );
        }
        else if( MatchToken( TokenKind::OpenBracket, lexer->token ) )
        {
            NextToken( lexer );
            Expr* index = ParseExpr( lexer );
            RequireToken( TokenKind::CloseBracket, lexer );

            if( lexer->IsValid() )
                expr = NewIndexExpr( pos, expr, index );
        }
        else if( MatchToken( TokenKind::Dot, lexer->token ) )
        {
            Token field = NextToken( lexer );
            RequireToken( TokenKind::Name, lexer );

            if( lexer->IsValid() )
                expr = NewFieldExpr( pos, expr, field.ident );
        }

        NextToken( lexer );
    }
    
    return expr;
}

Expr* ParseUnaryExpr( Lexer* lexer )
{
    if( lexer->token.HasFlag( TokenFlags::UnaryOp ) )
    {
        SourcePos pos = lexer->token.pos;
        TokenKind::Enum op = lexer->token.kind;

        NextToken( lexer );
        return NewUnaryExpr( pos, op, ParseUnaryExpr( lexer ) );
    }
    else
        return ParsePostfixExpr( lexer );
}

Expr* ParseMulExpr( Lexer* lexer )
{
    Expr* expr = ParseUnaryExpr( lexer );

    while( lexer->token.HasFlag( TokenFlags::MulOp ) )
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

    while( lexer->token.HasFlag( TokenFlags::AddOp ) )
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

    while( lexer->token.HasFlag( TokenFlags::CmpOp ) )
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

        NextToken( lexer );
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

#define APPEND(fmt, ...) \
{ \
    len = snprintf( outBuf, maxLen, fmt, ##__VA_ARGS__ ); \
    outBuf += len; \
    maxLen -= len; \
} 

void DebugPrintSExpr( Expr* expr, char*& outBuf, sz& maxLen )
{
    int len = 0;
    switch( expr->kind )
    {
        case Expr::Int:
        {
            APPEND( "%llu", expr->literal.intValue );
        } break;
        case Expr::Float:
        {
            APPEND( "%.16g", expr->literal.floatValue );
        } break;
        case Expr::Str:
        {
            APPEND( "'%.*s'", expr->literal.strValue.length, expr->literal.strValue.data );
        } break;
        case Expr::Name:
        {
            APPEND( "%s", expr->name );
        } break;
        case Expr::Call:
        {
            APPEND( "(call " );
            DebugPrintSExpr( expr->call.func, outBuf, maxLen );
            APPEND( " (" );
            for( int i = 0; i < expr->call.args.count; ++i )
            {
                if( i )
                    APPEND( ", " );
                DebugPrintSExpr( expr->call.args[i], outBuf, maxLen );
            }
            APPEND( "))" );
        } break;
        case Expr::Index:
        {
            APPEND( "([] " );
            DebugPrintSExpr( expr->index.base, outBuf, maxLen );
            APPEND( " " );
            DebugPrintSExpr( expr->index.index, outBuf, maxLen );
            APPEND( ")" );
        } break;
        case Expr::Field:
        {
            APPEND( "(. " );
            DebugPrintSExpr( expr->field.base, outBuf, maxLen );
            APPEND( " %s)", expr->field.name );
        } break;
        case Expr::Compound:
        {
            APPEND( "({} " );
            int i = 0;
            for( CompoundField const& f : expr->compound.fields )
            {
                if( i++ )
                    APPEND( ", " );
                if( f.kind == CompoundField::Index )
                {
                    APPEND( "[" );
                    DebugPrintSExpr( f.index, outBuf, maxLen );
                    APPEND( "]: " );
                }
                else if( f.kind == CompoundField::Name )
                {
                    APPEND( "%s: ", f.name );
                }
                DebugPrintSExpr( f.initValue, outBuf, maxLen );
            }
            APPEND( ")" );
        } break;
        case Expr::Unary:
        {
            APPEND( "(%s ", TokenKind::Values::names[ expr->unary.op ] );
            DebugPrintSExpr( expr->unary.expr, outBuf, maxLen );
            APPEND( ")" );
        } break;
        case Expr::Binary:
        {
            APPEND( "(%s ", TokenKind::Values::names[ expr->binary.op ] );
            DebugPrintSExpr( expr->binary.left, outBuf, maxLen );
            APPEND( " " );
            DebugPrintSExpr( expr->binary.right, outBuf, maxLen );
            APPEND( ")" );
        } break;
        case Expr::Ternary:
        {
            APPEND( "(" );
            DebugPrintSExpr( expr->ternary.cond, outBuf, maxLen );
            APPEND( " ? " );
            DebugPrintSExpr( expr->ternary.thenExpr, outBuf, maxLen );
            APPEND( " : " );
            DebugPrintSExpr( expr->ternary.elseExpr, outBuf, maxLen );
            APPEND( ")" );
        } break;
        case Expr::Sizeof:
        {
            APPEND( "(sizeof " );
            DebugPrintSExpr( expr->sizeof_.expr, outBuf, maxLen );
            APPEND( ")" );
        } break;
    }
}
#undef APPEND


Decl* ParseStructDecl( SourcePos const& pos, Lexer* lexer )
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

Decl* ParseEnumDecl( SourcePos const& pos, Lexer* lexer )
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
            return ParseStructDecl( pos, lexer );
        else if( MatchKeyword( Keyword::Enum, token ) )
            return ParseEnumDecl( pos, lexer );
    }
    else
    {
        // Try parsing a var / const
        RequireToken( TokenKind::Name, lexer );
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
            if( MatchToken( TokenKind::Assign, lexer->token ) )
                expr = ParseExpr( lexer );
        }
        RequireToken( TokenKind::Semicolon, lexer );

        if( lexer->IsValid() )
        {
#if 0
            return NewVarDecl( pos, name, type, expr, isConst );
#endif
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
