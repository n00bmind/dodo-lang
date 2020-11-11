TypeSpec* NewTypeSpec( SourcePos const& pos, TypeSpec::Kind kind )
{
    TypeSpec* result = PUSH_STRUCT( &globalArena, TypeSpec );
    result->pos = pos;
    result->kind = kind;

    return result;
}

TypeSpec* NewNameTypeSpec( SourcePos const& pos, BucketArray<char const*> const& names )
{
    TypeSpec* result = NewTypeSpec( pos, TypeSpec::Name );
    new( &result->names ) Array<char const*>( &globalArena, names.count );
    names.CopyTo( &result->names );

    return result;
}

TypeSpec* NewPtrTypeSpec( SourcePos const& pos, TypeSpec* ofType )
{
    TypeSpec* result = NewTypeSpec( pos, TypeSpec::Pointer );
    result->base = ofType;

    return result;
}

TypeSpec* NewFuncTypeSpec( SourcePos const& pos, BucketArray<TypeSpec*> const& args, TypeSpec* returnType )
{
    TypeSpec* result = NewTypeSpec( pos, TypeSpec::Func );
    new( &result->func.args ) Array<TypeSpec*>( &globalArena, args.count );
    args.CopyTo( &result->func.args );
    result->func.returnType = returnType;

    return result;
}

TypeSpec* NewArrayTypeSpec( SourcePos const& pos, TypeSpec* ofType, Expr* size )
{
    TypeSpec* result = NewTypeSpec( pos, TypeSpec::Array );
    result->base = ofType;
    result->arraySize = size;

    return result;
}

// NOTE All ParseXXX() functions leave the token "cursor" at the next immediate token after the expression
// (This is required as sometimes we don't know whether an expression has finished until we find a token that's not part of it)
// (I guess we could PeekToken() in those cases, but this seems simpler?)
Expr* ParseExpr( Lexer* lexer );
TypeSpec* ParseTypeSpec( Lexer* lexer );

TypeSpec* ParseBaseTypeSpec( Lexer* lexer )
{
    TypeSpec* type = nullptr;

    Token const& token = lexer->token;
    SourcePos pos = token.pos;

    if( MatchToken( TokenKind::Name, token ) )
    {
        BucketArray<char const*> names( &globalTmpArena, 8 );

        names.Push( token.ident );
        NextToken( lexer );
        while( MatchToken( TokenKind::Dot, token ) )
        {
            NextToken( lexer );
            RequireToken( TokenKind::Name, lexer );

            names.Push( token.ident );
            NextToken( lexer );
        }

        if( lexer->IsValid() )
            type = NewNameTypeSpec( pos, names );
    }
    else if( MatchToken( TokenKind::OpenParen, token ) )
    {
        // Function type
        BucketArray<TypeSpec*> args( &globalTmpArena, 8 );

        NextToken( lexer );
        while( !MatchToken( TokenKind::CloseParen, token ) )
        {
            TypeSpec* arg = ParseTypeSpec( lexer );
            if( !MatchToken( TokenKind::Comma, token ) )
                break;
            NextToken( lexer );
        }
        RequireToken( TokenKind::CloseParen, lexer );

        TypeSpec* returnType = nullptr;
        if( MatchToken( TokenKind::RightArrow, token ) )
        {
            NextToken( lexer );
            returnType = ParseTypeSpec( lexer );
        }

        // TODO Capture / closure / restrict

        if( lexer->IsValid() )
            type = NewFuncTypeSpec( pos, args, returnType );
    }

    return type;
}

TypeSpec* ParseTypeSpec( Lexer* lexer )
{
    TypeSpec* type = nullptr;

    Token const& token = lexer->token;
    SourcePos pos = token.pos;

    if( MatchToken( TokenKind::OpenBracket, token ) )
    {
        Expr* size = nullptr;

        NextToken( lexer );
        if( !MatchToken( TokenKind::CloseBracket, token ) )
        {
            size = ParseExpr( lexer );
        }
        RequireToken( TokenKind::CloseBracket, lexer );

        NextToken( lexer );
        TypeSpec* ofType = ParseTypeSpec( lexer );

        if( lexer->IsValid() )
            type = NewArrayTypeSpec( pos, ofType, size );
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

CompoundField ParseCompoundFieldExpr( Lexer* lexer )
{
    Token const& token = lexer->token;
    SourcePos pos = lexer->token.pos;

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
    SourcePos pos = lexer->token.pos;
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
    SourcePos pos = token.pos;

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


AggregateItem NewSubAggregate( SourcePos const& pos, Decl* subAggregate )
{
    AggregateItem result = {};
    result.pos = pos;
    result.kind = AggregateItem::SubAggregate;
    result.subAggregate = subAggregate;

    return result;
}

AggregateItem NewAggregateField( SourcePos const& pos, BucketArray<char const*> const& names, TypeSpec* type )
{
    AggregateItem result = {};
    result.pos = pos;
    result.kind = AggregateItem::Field;
    new( &result.names ) Array<char const*>( &globalArena, names.count );
    names.CopyTo( &result.names );
    result.type = type;

    return result;
}

Decl* ParseEnumDecl( Lexer* lexer );
Decl* ParseAggregateDecl( Lexer* lexer, int kind );

AggregateItem ParseAggregateItemDecl( Lexer* lexer )
{
    SourcePos pos = lexer->pos;
    AggregateItem result = {};

    if( MatchKeyword( Keyword::Enum, lexer->token ) )
    {
        Decl* subAggregate = ParseEnumDecl( lexer );
        result = NewSubAggregate( pos, subAggregate );
    }
    else if( MatchKeyword( Keyword::Struct, lexer->token ) )
    {
        Decl* subAggregate = ParseAggregateDecl( lexer, Keyword::Struct );
        result = NewSubAggregate( pos, subAggregate );
    }
    else
    {
        // Parse field(s)
        BucketArray<char const*> names( &globalTmpArena, 16 );
        
        RequireToken( TokenKind::Name, lexer );
        names.Push( lexer->token.ident );

        NextToken( lexer );
        while( MatchToken( TokenKind::Comma, lexer->token ) )
        {
            NextToken( lexer );
            RequireToken( TokenKind::Name, lexer );
            names.Push( lexer->token.ident );
            
            NextToken( lexer );
        }
        RequireToken( TokenKind::Colon, lexer );

        NextToken( lexer );
        TypeSpec* type = ParseTypeSpec( lexer );
        RequireToken( TokenKind::Semicolon, lexer );
        NextToken( lexer );

        // TODO Parse this as several separate items with the same type instead?
        if( lexer->IsValid() )
            result = NewAggregateField( pos, names, type );
    }

    return result;
}

Decl* NewDecl( SourcePos const& pos, Decl::Kind kind, char const* name )
{
    Decl* result = PUSH_STRUCT( &globalArena, Decl );
    result->pos = pos;
    result->name = name;
    result->kind = kind;

    return result;
}

Decl* NewAggregateDecl( SourcePos const& pos, int kind, char const* name, BucketArray<AggregateItem> const& items )
{
    Decl::Kind declKind = Decl::None;
    switch( kind )
    {
        case Keyword::Struct: declKind = Decl::Struct; break;
        //case Keyword::Union: declKind = Decl::Union break;
    }

    Decl* result = nullptr;
    if( declKind != Decl::None )
    {
        result = NewDecl( pos, declKind, name );
        new( &result->aggregate.items ) Array<AggregateItem>( &globalArena, items.count );
        items.CopyTo( &result->aggregate.items );
    }

    return result;
}

Decl* ParseAggregateDecl( Lexer* lexer, int kind )
{
    SourcePos pos = lexer->pos;
    
    Token token = NextToken( lexer );
    RequireToken( TokenKind::Name, lexer );
    char const* name = token.ident;

    NextToken( lexer );
    RequireToken( TokenKind::OpenBrace, lexer );
    NextToken( lexer );

    BucketArray<AggregateItem> items( &globalTmpArena, 16 );
    while( !MatchToken( TokenKind::CloseBrace, lexer->token ) )
    {
        items.Push( ParseAggregateItemDecl( lexer ) );
    }

    RequireToken( TokenKind::CloseBrace, lexer );
    NextToken( lexer );

    Decl* result = nullptr;
    if( lexer->IsValid() )
    {
        result = NewAggregateDecl( pos, kind, name, items );
        if( !result )
            PARSE_ERROR( token, "Unsupported aggregate type '%s'", Keyword::Values::names[ kind ] );
    }

    return result;
}

EnumItem ParseEnumItemDecl( Lexer* lexer )
{
    Token const& token = lexer->token;
    SourcePos pos = token.pos;
    
    RequireToken( TokenKind::Name, lexer );
    char const* name = token.ident;

    Expr* init = nullptr;
    NextToken( lexer );
    if( MatchToken( TokenKind::Assign, lexer->token ) )
    {
        NextToken( lexer );
        init = ParseExpr( lexer );
    }

    EnumItem result = {};
    if( lexer->IsValid() )
        result = { pos, name, init };

    return result;
}

Decl* NewEnumDecl( SourcePos const& pos, char const* name, TypeSpec* type, BucketArray<EnumItem> const& items )
{
    Decl* result = NewDecl( pos, Decl::Enum, name );
    new( &result->enum_.items ) Array<EnumItem>( &globalArena, items.count );
    items.CopyTo( &result->enum_.items );
    result->enum_.type = type;

    return result;
}

Decl* ParseEnumDecl( Lexer* lexer )
{
    SourcePos pos = lexer->pos;

    Token token = NextToken( lexer );
    RequireToken( TokenKind::Name, lexer );
    char const* name = token.ident;

    TypeSpec* type = nullptr;

    NextToken( lexer );
    if( MatchToken( TokenKind::Colon, lexer->token ) )
    {
        NextToken( lexer );
        type = ParseTypeSpec( lexer );
    }

    RequireToken( TokenKind::OpenBrace, lexer );
    NextToken( lexer );

    BucketArray<EnumItem> items( &globalTmpArena, 16 );
    while( !MatchToken( TokenKind::CloseBrace, lexer->token ) )
    {
        items.Push( ParseEnumItemDecl( lexer ) );
        if( !MatchToken( TokenKind::Comma, lexer->token ) )
            break;

        RequireToken( TokenKind::Comma, lexer );
        NextToken( lexer );
    }

    RequireToken( TokenKind::CloseBrace, lexer );
    NextToken( lexer );

    Decl* result = nullptr;
    if( lexer->IsValid() )
        result = NewEnumDecl( pos, name, type, items );

    return result;
}

FuncArg ParseFuncArg( Lexer* lexer )
{
    FuncArg result = {};
    SourcePos pos = lexer->pos;

    RequireToken( TokenKind::Name, lexer );
    char const* name = lexer->token.ident;

    NextToken( lexer );
    RequireToken( TokenKind::Colon, lexer );

    NextToken( lexer );
    TypeSpec* type = ParseTypeSpec( lexer );

    if( lexer->IsValid() )
        result = { pos, name, type };

    return result;
}

Decl* NewFuncDecl( SourcePos const& pos, char const* name, BucketArray<FuncArg> const& args, StmtList* body, TypeSpec* returnType )
{
    Decl* result = NewDecl( pos, Decl::Func, name );
    new( &result->func.args ) Array<FuncArg>( &globalArena, args.count );
    args.CopyTo( &result->func.args );
    result->func.body = body;
    result->func.returnType = returnType;

    return result;
}

Decl* NewVarDecl( SourcePos const& pos, char const* name, TypeSpec* type, Expr* initExpr, bool isConst )
{
    Decl* result = NewDecl( pos, Decl::Var, name );
    result->var.type = type;
    result->var.initExpr = initExpr;
    result->var.isConst = isConst;

    return result;
}

StmtList* ParseStmtBlock( Lexer* lexer );

Decl* ParseDecl( Lexer* lexer )
{
    Token const& token = lexer->token;
    SourcePos pos = token.pos;

    if( MatchToken( TokenKind::Keyword, token ) )
    {
        // TODO Unions
        if( MatchKeyword( Keyword::Struct, token ) )
            return ParseAggregateDecl( lexer, Keyword::Struct );
        else if( MatchKeyword( Keyword::Enum, token ) )
            return ParseEnumDecl( lexer );

        // TODO Directives
    }
    else
    {
        RequireToken( TokenKind::Name, lexer );
        char const* name = lexer->token.ident;
        NextToken( lexer );
        RequireToken( TokenKind::Colon, lexer );

        bool isConst = false;
        Expr* initExpr = nullptr;
        TypeSpec* type = nullptr;

        NextToken( lexer );
        if( MatchToken( TokenKind::Colon, lexer->token ) )
        {
            NextToken( lexer );
            if( MatchToken( TokenKind::OpenParen, lexer->token ) )
            {
                // Func
                BucketArray<FuncArg> args( &globalTmpArena, 16 );

                NextToken( lexer );
                if( !MatchToken( TokenKind::CloseParen, lexer->token ) )
                {
                    args.Push( ParseFuncArg( lexer ) );
                    while( MatchToken( TokenKind::Comma, lexer->token ) )
                    {
                        NextToken( lexer );
                        args.Push( ParseFuncArg( lexer ) );
                    }
                    RequireToken( TokenKind::CloseParen, lexer );
                }

                NextToken( lexer );
                TypeSpec* returnType = nullptr;
                if( MatchToken( TokenKind::RightArrow, lexer->token ) )
                {
                    NextToken( lexer );
                    returnType = ParseTypeSpec( lexer );
                }

                RequireToken( TokenKind::OpenBrace, lexer );
                StmtList* body = nullptr;
                if( lexer->IsValid() )
                    body = ParseStmtBlock( lexer );

                if( lexer->IsValid() )
                    return NewFuncDecl( pos, name, args, body, returnType );
            }
            else
            {
                // Const
                // TODO Type specifier?
                initExpr = ParseExpr( lexer );
                isConst = true;

                RequireToken( TokenKind::Semicolon, lexer );
                NextToken( lexer );
            }
        }
        else
        {
            if( !MatchToken( TokenKind::Assign, lexer->token ) )
                type = ParseTypeSpec( lexer );
            if( MatchToken( TokenKind::Assign, lexer->token ) )
            {
                NextToken( lexer );
                initExpr = ParseExpr( lexer );
            }

            RequireToken( TokenKind::Semicolon, lexer );
            NextToken( lexer );
        }

        if( lexer->IsValid() )
            return NewVarDecl( pos, name, type, initExpr, isConst );
    }

    return nullptr;
}

StmtList* ParseStmtBlock( Lexer* lexer )
{
    // TODO 
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


#define INDENT \
{ \
    len = snprintf( outBuf, maxLen, "%*s", indent * 4, "" ); \
    outBuf += len; \
    maxLen -= len; \
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

    if( expr == nullptr )
    {
        APPEND( "null" );
        return;
    }

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

void DebugPrintTypeSpec( TypeSpec* type, char*& outBuf, sz& maxLen )
{
    int len = 0;

    if( type == nullptr )
    {
        APPEND( "null" );
        return;
    }

    switch( type->kind )
    {
        case TypeSpec::Name:
        {
            for( char const* n : type->names )
            {
                if( n != type->names[0] )
                    APPEND( "." );
                APPEND( "%s", n );
            }
        } break;
        case TypeSpec::Func:
        {
            APPEND( "( " );
            for( TypeSpec* t : type->func.args )
            {
                if( t != type->func.args[0] )
                    APPEND( ", " );
                DebugPrintTypeSpec( t, outBuf, maxLen );
            }
            APPEND( " ) -> " );
            DebugPrintTypeSpec( type->func.returnType, outBuf, maxLen );
        } break;
        case TypeSpec::Array:
        {
            APPEND( "[" );
            if( type->arraySize )
                DebugPrintSExpr( type->arraySize, outBuf, maxLen );
            APPEND( "] " );
        } break;
        case TypeSpec::Pointer:
        {
            APPEND( "*" );
        } break;
    }

    if( type->base )
        DebugPrintTypeSpec( type->base, outBuf, maxLen );
}

void DebugPrintSExpr( Decl* decl, char*& outBuf, sz& maxLen, int& indent )
{
    int len = 0;
    switch( decl->kind )
    {
        case Decl::Enum:
        {
            APPEND( "(enum %s: ", decl->name );
            DebugPrintTypeSpec( decl->enum_.type, outBuf, maxLen );
            APPEND( "\n" );
            indent++;
            for( auto& it : decl->enum_.items )
            {
                INDENT APPEND( "(%s = ", it.name );
                DebugPrintSExpr( it.initValue, outBuf, maxLen );
                APPEND( "),\n" );
            }
            APPEND( ")\n" );
            indent--;
        } break;

        case Decl::Struct:
        {
            APPEND( "(struct %s\n", decl->name );
            indent++;
            for( auto& it : decl->aggregate.items )
            {
                if( it.kind == AggregateItem::Field )
                {
                    INDENT APPEND( "(" );
                    for( char const* n : it.names )
                    {
                        if( n != it.names[0] )
                            APPEND( ", " );
                        APPEND( "%s", n );
                    }
                    APPEND( ": " );
                    DebugPrintTypeSpec( it.type, outBuf, maxLen );
                    APPEND( "),\n" );
                }
                else
                    DebugPrintSExpr( it.subAggregate, outBuf, maxLen, indent );
            }
            APPEND( ")\n" );
            indent--;
        } break;

        case Decl::Union:
        {
            APPEND( "(union %s\n", decl->name );
            indent++;
            for( auto& it : decl->aggregate.items )
            {
                if( it.kind == AggregateItem::Field )
                {
                    APPEND( "(" );
                    for( char const* n : it.names )
                    {
                        if( n != it.names[0] )
                            APPEND( ", " );
                        APPEND( "%s", n );
                    }
                    APPEND( ": " );
                    DebugPrintTypeSpec( it.type, outBuf, maxLen );
                    APPEND( "),\n" );
                }
                else
                    DebugPrintSExpr( it.subAggregate, outBuf, maxLen, indent );
            }
            APPEND( ")\n" );
            indent--;
        } break;

        case Decl::Var:
        {
            APPEND( "(%s %s: ", decl->var.isConst ? "const" : "var", decl->name );
            DebugPrintTypeSpec( decl->var.type, outBuf, maxLen );
            APPEND( " = " );
            DebugPrintSExpr( decl->var.initExpr, outBuf, maxLen );
            APPEND( ")" );
        } break;
        case Decl::Func:
        {
            APPEND( "(func %s ( ", decl->name );
            for( auto& a : decl->func.args )
            {
                if( &a != decl->func.args.begin() )
                    APPEND( ", " );
                APPEND( "%s: ", a.name );
                DebugPrintTypeSpec( a.type, outBuf, maxLen );
            }
            APPEND( " ) -> " );
            DebugPrintTypeSpec( decl->func.returnType, outBuf, maxLen );
            APPEND( "\n" );

            // TODO

        } break;
        case Decl::Import:
        {

        } break;
    }
}

#undef APPEND
#undef INDENT
