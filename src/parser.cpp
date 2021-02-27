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
    result->ptr.base = ofType;

    return result;
}

TypeSpec* NewFuncTypeSpec( SourcePos const& pos, BucketArray<FuncArgSpec> const& args, TypeSpec* returnType )
{
    TypeSpec* result = NewTypeSpec( pos, TypeSpec::Func );
    new( &result->func.args ) Array<FuncArgSpec>( &globalArena, args.count );
    args.CopyTo( &result->func.args );
    result->func.returnType = returnType;

    return result;
}

TypeSpec* NewArrayTypeSpec( SourcePos const& pos, TypeSpec* ofType, Expr* size, bool isView )
{
    TypeSpec* result = NewTypeSpec( pos, TypeSpec::Array );
    result->array.base = ofType;
    result->array.count = size;
    result->array.isView = isView;

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
        BucketArray<char const*> names( &globalTmpArena, 8, Temporary() );

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
        BucketArray<FuncArgSpec> args( &globalTmpArena, 8, Temporary() );

        NextToken( lexer );
        while( !MatchToken( TokenKind::CloseParen, token ) )
        {
            TypeSpec* argType = ParseTypeSpec( lexer );

            bool vararg = false;
            if( MatchToken( TokenKind::Ellipsis, lexer->token ) )
            {
                vararg = true;
                NextToken( lexer );
            }
            args.Push( { argType, vararg } );

            if( !MatchToken( TokenKind::Comma, token ) )
                break;
            NextToken( lexer );
        }
        RequireTokenAndAdvance( TokenKind::CloseParen, lexer );

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
        bool isView = false;

        NextToken( lexer );
        if( !MatchToken( TokenKind::CloseBracket, token ) )
        {
            if( MatchToken( TokenKind::Asterisk, token ) )
            {
                isView = true;
                NextToken( lexer );
            }
            else
                size = ParseExpr( lexer );
        }
        RequireTokenAndAdvance( TokenKind::CloseBracket, lexer );

        TypeSpec* ofType = ParseTypeSpec( lexer );

        if( lexer->IsValid() )
            type = NewArrayTypeSpec( pos, ofType, size, isView );
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
    new( &result->compoundFields ) Array<CompoundField>( &globalArena, fields.count );
    fields.CopyTo( &result->compoundFields );

    return result;
}

Expr* NewNameExpr( SourcePos const& pos, char const* name )
{
    Expr* result = NewExpr( pos, Expr::Name );
    result->name.ident = name;
    result->name.symbol = nullptr;

    return result;
}

Expr* NewIntExpr( SourcePos const &pos, i64 value, Token::LiteralMod modifier )
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
    result->sizeofExpr = expr;

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

Expr* NewCastExpr( SourcePos const& pos, TypeSpec* type, Expr* castedExpr )
{
    Expr* result = NewExpr( pos, Expr::Cast );
    result->cast.type = type;
    result->cast.expr = castedExpr;

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

Expr* NewCommaExpr( SourcePos const& pos, BucketArray<Expr*> const& exprList )
{
    Expr* result = NewExpr( pos, Expr::Comma );
    new( &result->commaExprs ) Array<Expr*>( &globalArena, exprList.count );
    exprList.CopyTo( &result->commaExprs );

    return result;
}

Expr* NewRangeExpr( SourcePos const& pos, Expr* lowerBound, Expr* upperBound )
{
    Expr* result = NewExpr( pos, Expr::Range );
    result->range.lowerBound = lowerBound;
    result->range.upperBound = upperBound;

    return result;
}

CompoundField ParseCompoundFieldExpr( Lexer* lexer )
{
    SourcePos pos = lexer->token.pos;

    CompoundField result = {};
    if( MatchToken( TokenKind::OpenBracket, lexer->token ) )
    {
        NextToken( lexer );
        Expr* indexExpr = ParseExpr( lexer );
        RequireTokenAndAdvance( TokenKind::CloseBracket, lexer );
        RequireTokenAndAdvance( TokenKind::Assign, lexer );
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
    else if( MatchToken( TokenKind::Dot, lexer->token ) )
    {
        NextToken( lexer );
        RequireToken( TokenKind::Name, lexer );
        char const* fieldName = lexer->token.ident;
        NextToken( lexer );

        RequireTokenAndAdvance( TokenKind::Assign, lexer );
        Expr* valueExpr = ParseExpr( lexer );

        if( lexer->IsValid() )
            result = { pos, fieldName, valueExpr, CompoundField::Name };
    }
    else
    {
        Expr* expr = ParseExpr( lexer );
        if( lexer->IsValid() )
            result = { pos, nullptr, expr, CompoundField::Default };
    }

    return result;
}

Expr* ParseCompoundExpr( Lexer* lexer )
{
    SourcePos pos = lexer->token.pos;
    BucketArray<CompoundField> fields( &globalTmpArena, 16, Temporary() );

    RequireTokenAndAdvance( TokenKind::OpenBrace, lexer );
    while( !MatchToken( TokenKind::CloseBrace, lexer->token ) )
    {
        CompoundField field = ParseCompoundFieldExpr( lexer );
        fields.Push( field );

        if( !MatchToken( TokenKind::Comma, lexer->token ) )
            break;
        NextToken( lexer );
    }
    RequireTokenAndAdvance( TokenKind::CloseBrace, lexer );

    Expr* expr = nullptr;
    if( lexer->IsValid() )
        expr = NewCompoundExpr( pos, fields );
    return expr;
}

Expr* ParseUnaryExpr( Lexer* lexer );

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
        RequireTokenAndAdvance( TokenKind::OpenParen, lexer );
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
    else if( MatchToken( TokenKind::Range, token ) )
    {
        // This is an 'open' range, just return null which will be used as the lower bound by ParseRangeExpr on the way up
        advance = false;
    }
    else
    {
        char const* desc = TokenKind::Items::names[ token.kind ];
        if( token.kind == TokenKind::Name || token.kind == TokenKind::Keyword )
            desc = token.ident;

        PARSE_ERROR( token.pos, "Unexpected token '%s' in expression", desc );
    }

    if( advance && lexer->IsValid() )
        NextToken( lexer );

    return expr;
}

Expr* ParseAddExpr( Lexer* lexer );

Expr* ParsePostfixExpr( Lexer* lexer )
{
    Expr* expr = ParseBaseExpr( lexer );

    while( lexer->token.HasFlag( TokenFlags::PostfixOp ) )
    {
        SourcePos pos = lexer->token.pos;

        if( MatchToken( TokenKind::OpenParen, lexer->token ) )
        {
            BucketArray<Expr*> args( &globalTmpArena, 16, Temporary() );

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
            if( !(field.kind == TokenKind::Name || field.kind == TokenKind::Keyword) )
            {
                char const* desc = TokenKind::Items::names[ field.kind ];
                PARSE_ERROR( field.pos, "Unexpected token '%s' in expression", desc );
            }

            if( lexer->IsValid() )
                expr = NewFieldExpr( pos, expr, field.ident );
        }

        NextToken( lexer );
    }

    return expr;
}

Expr* ParseUnaryExpr( Lexer* lexer )
{
    Expr* expr = nullptr;
    SourcePos pos = lexer->token.pos;

    if( lexer->token.HasFlag( TokenFlags::UnaryOp ) )
    {
        TokenKind::Enum op = lexer->token.kind;

        NextToken( lexer );
        expr = NewUnaryExpr( pos, op, ParseUnaryExpr( lexer ) );
    }
    else if( MatchToken( TokenKind::LessThan, lexer->token ) )
    {
        NextToken( lexer );
        TypeSpec* type = ParseTypeSpec( lexer );
        RequireTokenAndAdvance( TokenKind::GreaterThan, lexer );

        if( lexer->IsValid() )
            expr = NewCastExpr( pos, type, ParseUnaryExpr( lexer ) );
    }
    else
    {
        expr = ParsePostfixExpr( lexer );
    }

    return expr;
}

Expr* ParseRangeExpr( Lexer* lexer )
{
    SourcePos pos = lexer->token.pos;
    Expr* expr = ParseUnaryExpr( lexer );

    Expr* upperBound = nullptr;
    if( MatchToken( TokenKind::Range, lexer->token ) )
    {
        NextToken( lexer );
        upperBound = ParseUnaryExpr( lexer );

        if( lexer->IsValid() )
            expr = NewRangeExpr( pos, expr, upperBound );
    }
    return expr;
}

Expr* ParseMulExpr( Lexer* lexer )
{
    Expr* expr = ParseRangeExpr( lexer );

    // NOTE Range expressions cannot really be combined (for now at least!)
    if( expr->kind == Expr::Range )
        return expr;

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

        // NOTE Range expressions cannot really be combined (for now at least!)
        if( expr->kind == Expr::Range )
        {
            PARSE_ERROR( expr->pos, "Range expression cannot be operated upon (complex bound expressions should be parenthesized)" );
            return nullptr;
        }

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

        RequireTokenAndAdvance( TokenKind::Colon, lexer );

        Expr* elseExpr = ParseTernaryExpr( lexer );

        if( lexer->IsValid() )
            expr = NewTernaryExpr( expr->pos, expr, thenExpr, elseExpr );
    }
    return expr;
}

// NOTE This is incompatible with some comma separated constructs involving expressions, like function call arguments and compound
// expressions, so right now we only invoke it specifically for declarations & assignments so we can support multiple return values
Expr* ParseCommaExpr( Lexer* lexer )
{
    Expr* expr = ParseTernaryExpr( lexer ); 

    if( MatchToken( TokenKind::Comma, lexer->token ) )
    {
        BucketArray<Expr*> exprList( &globalTmpArena, 8, Temporary() );
        exprList.Push( expr );

        while( MatchToken( TokenKind::Comma, lexer->token ) )
        {
            NextToken( lexer );
            exprList.Push( ParseTernaryExpr( lexer ) );
        }

        if( lexer->IsValid() )
            expr = NewCommaExpr( expr->pos, exprList );
    }
    return expr;
}

Expr* ParseExpr( Lexer* lexer )
{
    return ParseTernaryExpr( lexer );
}


Decl* NewDecl( SourcePos const& pos, Decl::Kind kind, char const* name, StmtList* parentBlock )
{
    Decl* result = PUSH_STRUCT( &globalArena, Decl );
    result->pos = pos;
    new( &result->names ) Array<char const*>( &globalArena, 1 );
    result->names.Push( name );
    result->parentBlock = parentBlock;
    result->kind = kind;

    return result;
}

Decl* NewDecl( SourcePos const& pos, Decl::Kind kind, BucketArray<char const*> const& names, StmtList* parentBlock )
{
    Decl* result = PUSH_STRUCT( &globalArena, Decl );
    result->pos = pos;
    // Copy names array to ensure it's in the global arena
    new( &result->names ) Array<char const*>( &globalArena, names.count );
    names.CopyTo( &result->names );
    result->parentBlock = parentBlock;
    result->kind = kind;

    return result;
}

Decl* NewAggregateDecl( SourcePos const& pos, int kind, char const* name, BucketArray<Decl*> const& items, StmtList* parentBlock  )
{
    Decl::Kind declKind = Decl::None;
    switch( kind )
    {
        case Keyword::Struct: declKind = Decl::Struct; break;
        case Keyword::Union: declKind = Decl::Union; break;
    }

    Decl* result = nullptr;
    if( declKind != Decl::None )
    {
        result = NewDecl( pos, declKind, name, parentBlock );
        new( &result->aggregate.items ) Array<Decl*>( &globalArena, items.count );
        items.CopyTo( &result->aggregate.items );
    }

    return result;
}

Decl* ParseDecl( Lexer* lexer, StmtList* parentBlock );

Decl* ParseAggregateBlockDecl( SourcePos const& pos, char const* name, Lexer* lexer, int kind, StmtList* parentBlock )
{
    Token token = lexer->token;

    RequireTokenAndAdvance( TokenKind::OpenBrace, lexer );

    BucketArray<Decl*> items( &globalTmpArena, 16, Temporary() );
    while( !MatchToken( TokenKind::CloseBrace, lexer->token ) )
    {
        Decl* decl = ParseDecl( lexer, parentBlock );
        items.Push( decl );
    }

    RequireTokenAndAdvance( TokenKind::CloseBrace, lexer );

    Decl* result = nullptr;
    if( lexer->IsValid() )
    {
        result = NewAggregateDecl( pos, kind, name, items, parentBlock );
        if( !result )
            PARSE_ERROR( token.pos, "Unsupported aggregate type '%s'", Keyword::Items::names[ kind ] );
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

Decl* NewEnumDecl( SourcePos const& pos, char const* name, TypeSpec* type, BucketArray<EnumItem> const& items, StmtList* parentBlock )
{
    Decl* result = NewDecl( pos, Decl::Enum, name, parentBlock );
    new( &result->enum_.items ) Array<EnumItem>( &globalArena, items.count );
    items.CopyTo( &result->enum_.items );
    result->enum_.type = type;

    return result;
}

Decl* ParseEnumBlockDecl( SourcePos const& pos, char const* name, Lexer* lexer, StmtList* parentBlock )
{
    TypeSpec* type = nullptr;

    if( MatchToken( TokenKind::Colon, lexer->token ) )
    {
        NextToken( lexer );
        type = ParseTypeSpec( lexer );
    }

    RequireTokenAndAdvance( TokenKind::OpenBrace, lexer );

    BucketArray<EnumItem> items( &globalTmpArena, 16, Temporary() );
    while( !MatchToken( TokenKind::CloseBrace, lexer->token ) )
    {
        items.Push( ParseEnumItemDecl( lexer ) );
        if( !MatchToken( TokenKind::Comma, lexer->token ) )
            break;

        RequireTokenAndAdvance( TokenKind::Comma, lexer );
    }

    RequireTokenAndAdvance( TokenKind::CloseBrace, lexer );

    Decl* result = nullptr;
    if( lexer->IsValid() )
        result = NewEnumDecl( pos, name, type, items, parentBlock );

    return result;
}

FuncArgDecl ParseFuncArgDecl( Lexer* lexer )
{
    FuncArgDecl result = {};
    SourcePos pos = lexer->pos;

    // TODO Omit arg names for foreign funcs
    RequireToken( TokenKind::Name, lexer );
    char const* name = lexer->token.ident;

    NextToken( lexer );
    RequireTokenAndAdvance( TokenKind::Colon, lexer );

    TypeSpec* type = ParseTypeSpec( lexer );

    bool vararg = false;
    if( MatchToken( TokenKind::Ellipsis, lexer->token ) )
    {
        vararg = true;
        NextToken( lexer );
    }

    if( lexer->IsValid() )
        result = { pos, name, type, vararg };

    return result;
}

Decl* NewFuncDecl( SourcePos const& pos, char const* name, BucketArray<FuncArgDecl> const& args, StmtList* body,
                   TypeSpec* returnType, char const* directive, StmtList* parentBlock )
{
    Decl* result = NewDecl( pos, Decl::Func, name, parentBlock );
    new( &result->func.args ) Array<FuncArgDecl>( &globalArena, args.count );
    args.CopyTo( &result->func.args );
    result->func.body = body;
    result->func.returnType = returnType;
    result->directive = directive;

    return result;
}

Decl* NewVarDecl( SourcePos const& pos, BucketArray<char const*> const& names, TypeSpec* type, Expr* initExpr, bool isConst, StmtList* parentBlock )
{
    Decl* result = NewDecl( pos, Decl::Var, names, parentBlock );
    result->var.type = type;
    result->var.initExpr = initExpr;
    result->var.isConst = isConst;

    return result;
}

// For synthetic Decls
Decl* NewVarDecl( SourcePos const& pos, char const* name, TypeSpec* type, Expr* initExpr, StmtList* parentBlock )
{
    BucketArray<char const*> names( &globalTmpArena, 1, Temporary() );
    names.Push( name );

    return NewVarDecl( pos, names, type, initExpr, false, parentBlock );
}

Stmt* ParseStmt( Lexer* lexer, StmtList* parent ); 

void GlobalPathBuilder( StringBuilder* path, StmtList* parent, char const* name )
{
    StmtList* p = parent;
    while( p )
    {
        GlobalPathBuilder( path, p->parent, p->name );
        p = p->parent;
    }

    if( !path->Empty() )
        path->AppendString( "_" );

    if( name )
        path->AppendString( name );
    else
    {
        ASSERT( parent );
        path->Append( "%d", parent->childCount );
    }
}

String GlobalPathString( StmtList* parent, char const* name )
{
    StringBuilder path( &globalTmpArena );
    GlobalPathBuilder( &path, parent, name );

    return path.ToString( &globalArena);
}

StmtList* NewStmtList( SourcePos const& pos, StmtList* parent, char const* name )
{
    StmtList* result = PUSH_STRUCT( &globalArena, StmtList );
    result->pos = pos;
    result->parent = parent;
    result->name = name;

    if( parent )
        parent->childCount++;

    result->globalPath = GlobalPathString( parent, name );

    return result;
}

void AddStmtListStmts( StmtList* block, BucketArray<Stmt*> const& stmts )
{
    new( &block->stmts ) Array<Stmt*>( &globalArena, stmts.count );
    stmts.CopyTo( &block->stmts );
}

StmtList* ParseStmtBlock( Lexer* lexer, StmtList* parent, char const* name )
{
    SourcePos pos = lexer->token.pos;
    BucketArray<Stmt*> stmts( &globalTmpArena, 16, Temporary() );
    
    StmtList* result = NewStmtList( pos, parent, name );

    RequireTokenAndAdvance( TokenKind::OpenBrace, lexer );
    while( !MatchToken( TokenKind::CloseBrace, lexer->token ) )
    {
        stmts.Push( ParseStmt( lexer, result ) );
    }
    RequireTokenAndAdvance( TokenKind::CloseBrace, lexer );

    if( lexer->IsValid() )
    {
        AddStmtListStmts( result, stmts );
        return result;
    }

    return nullptr;
}


Decl* ParseNamedDecl( SourcePos const& pos, Expr* namesExpr, char const* directive, Lexer* lexer, StmtList* parentBlock )
{
    bool isConst = false;
    Expr* initExpr = nullptr;
    TypeSpec* type = nullptr;

    RequireTokenAndAdvance( TokenKind::Colon, lexer );

    if( MatchToken( TokenKind::Colon, lexer->token ) )
    {
        NextToken( lexer );

        if( MatchKeyword( Keyword::Struct, lexer->token ) )
        {
            if( namesExpr->kind != Expr::Name )
            {
                PARSE_ERROR( pos, "Struct declaration cannot be given more than one name" );
                return nullptr;
            }

            NextToken( lexer );
            return ParseAggregateBlockDecl( pos, namesExpr->name.ident, lexer, Keyword::Struct, parentBlock );
        }
        else if( MatchKeyword( Keyword::Union, lexer->token ) )
        {
            if( namesExpr->kind != Expr::Name )
            {
                PARSE_ERROR( pos, "Union declaration cannot be given more than one name" );
                return nullptr;
            }

            NextToken( lexer );
            return ParseAggregateBlockDecl( pos, namesExpr->name.ident, lexer, Keyword::Union, parentBlock );
        }
        // TODO Enum struct
        // TODO Enum union (tagged union)
        else if( MatchKeyword( Keyword::Enum, lexer->token ) )
        {
            if( namesExpr->kind != Expr::Name )
            {
                PARSE_ERROR( pos, "Enum declaration cannot be given more than one name" );
                return nullptr;
            }

            NextToken( lexer );
            return ParseEnumBlockDecl( pos, namesExpr->name.ident, lexer, parentBlock );
        }

        else if( MatchToken( TokenKind::OpenParen, lexer->token ) )
        {
            if( namesExpr->kind != Expr::Name )
            {
                PARSE_ERROR( pos, "Function declaration cannot be given more than one name" );
                return nullptr;
            }

            // Func
            BucketArray<FuncArgDecl> args( &globalTmpArena, 16, Temporary() );

            NextToken( lexer );
            if( !MatchToken( TokenKind::CloseParen, lexer->token ) )
            {
                args.Push( ParseFuncArgDecl( lexer ) );
                while( MatchToken( TokenKind::Comma, lexer->token ) )
                {
                    NextToken( lexer );
                    args.Push( ParseFuncArgDecl( lexer ) );
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

            StmtList* body = nullptr;
            bool isForeign = directive == globalDirectives[ Directive::Foreign ];

            if( isForeign )
                RequireTokenAndAdvance( TokenKind::Semicolon, lexer );
            else
            {
                RequireToken( TokenKind::OpenBrace, lexer );
                if( lexer->IsValid() )
                    body = ParseStmtBlock( lexer, nullptr, namesExpr->name.ident );
            }

            if( lexer->IsValid() )
                return NewFuncDecl( pos, namesExpr->name.ident, args, body, returnType, directive, parentBlock );
        }
        else
        {
            // Const
            // TODO So const vars cannot have a type specifier?
            initExpr = ParseCommaExpr( lexer );
            isConst = true;

            RequireTokenAndAdvance( TokenKind::Semicolon, lexer );
        }
    }
    else
    {
        if( !MatchToken( TokenKind::Assign, lexer->token ) )
            // TODO We could decide to allow specifying one-time structs/enums inline here too
            // (in that case I think it would make sense to allow binding to more than one name)
            type = ParseTypeSpec( lexer );
        if( MatchToken( TokenKind::Assign, lexer->token ) )
        {
            NextToken( lexer );
            initExpr = ParseCommaExpr( lexer );
        }

        RequireTokenAndAdvance( TokenKind::Semicolon, lexer );
    }

    BucketArray<char const*> names( &globalTmpArena, 8, Temporary() );
    if( namesExpr->kind == Expr::Name )
        names.Push( namesExpr->name.ident );
    else
    {
        if( namesExpr->kind == Expr::Comma )
        {
            for( Expr* e : namesExpr->commaExprs )
            {
                if( e->kind != Expr::Name )
                {
                    PARSE_ERROR( e->pos, "Expected identifier" );
                    break;
                }
                names.Push( e->name.ident );
            }
        }
        else
            PARSE_ERROR( namesExpr->pos, "Expected one or more identifiers" );
    }

    if( lexer->IsValid() )
        return NewVarDecl( pos, names, type, initExpr, isConst, parentBlock );

    return nullptr;
}

char const* ParseDirective( Lexer* lexer )
{
    // Directives are always optional
    char const* result = nullptr;
    if( MatchToken( TokenKind::Pound, lexer->token ) )
    {
        NextToken( lexer );
        result = RequireDirective( lexer );
        NextToken( lexer );
    }

    return result;
}

Decl* ParseDecl( Lexer* lexer, StmtList* parentBlock )
{
    char const* directive = ParseDirective( lexer );

    SourcePos pos = lexer->token.pos;
    BucketArray<Token> names( &globalTmpArena, 8, Temporary() );

    RequireToken( TokenKind::Name, lexer );
    names.Push( lexer->token );

    NextToken( lexer );
    while( MatchToken( TokenKind::Comma, lexer->token ) )
    {
        NextToken( lexer );
        RequireToken( TokenKind::Name, lexer );
        names.Push( lexer->token );
        
        NextToken( lexer );
    }

    Expr* expr = nullptr;
    if( names.count > 1 )
    {
        BucketArray<Expr*> exprs( &globalTmpArena, names.count, Temporary() );
        auto idx = names.First();
        while( idx )
        {
            Token const& token = *idx;
            exprs.Push( NewNameExpr( token.pos, token.ident ) );

            idx.Next();
        }
        expr = NewCommaExpr( pos, exprs );
    }
    else
    {
        ASSERT( names.count == 1 );
        expr = NewNameExpr( pos, (*names.First()).ident );
    }

    return ParseNamedDecl( pos, expr, directive, lexer, parentBlock );
}

Stmt* NewStmt( SourcePos const& pos, Stmt::Kind kind, StmtList* parentBlock )
{
    Stmt* result = PUSH_STRUCT( &globalArena, Stmt );
    result->pos = pos;
    result->kind = kind;
    result->parentBlock = parentBlock;

    return result;
}

Stmt* NewDeclStmt( SourcePos const& pos, Decl* decl, StmtList* parentBlock )
{
    Stmt* result = NewStmt( pos, Stmt::Decl, parentBlock );
    result->decl = decl;

    return result;
}

Stmt* NewAssignStmt( SourcePos const& pos, Expr* leftExpr, TokenKind::Enum op, Expr* rightExpr, StmtList* parentBlock )
{
    Stmt* result = NewStmt( pos, Stmt::Assign, parentBlock );
    result->assign.left = leftExpr;
    result->assign.right = rightExpr;
    result->assign.op = op;

    return result;
}

Stmt* NewExprStmt( SourcePos const& pos, Expr* expr, StmtList* parentBlock )
{
    Stmt* result = NewStmt( pos, Stmt::Expr, parentBlock );
    result->expr = expr;
    
    return result;
}

//a, b, c :: 10, 1.0, "whatever";
//a, b, c := some_func();
//a, b, c = some_func(), b+c, x > 10 ? 1 : 0;
//a, b, c += d, e, f; //?

// Assignment / decl / expr
Stmt* ParseSimpleStmt( SourcePos const& pos, Lexer* lexer, StmtList* parentBlock )
{
    Stmt* result = nullptr;
    bool consumeSemicolon = true;

    Expr* expr = ParseCommaExpr( lexer );

    if( (expr->kind == Expr::Comma || expr->kind == Expr::Name) &&
        MatchToken( TokenKind::Colon, lexer->token ) )
    {
        Decl* decl = ParseNamedDecl( pos, expr, nullptr, lexer, parentBlock );

        if( lexer->IsValid() )
            result = NewDeclStmt( pos, decl, parentBlock );

        consumeSemicolon = false;
    }
    else if( lexer->token.HasFlag( TokenFlags::AssignOp ) )
    {
        TokenKind::Enum op = lexer->token.kind;
        NextToken( lexer );
        Expr* rightExpr = ParseCommaExpr( lexer );

        if( lexer->IsValid() )
            result = NewAssignStmt( pos, expr, op, rightExpr, parentBlock );
    }
    else
    {
        // TODO What is this for, exactly? Could be useful for for? x)
        result = NewExprStmt( pos, expr, parentBlock );
    }

    if( consumeSemicolon )
        RequireTokenAndAdvance( TokenKind::Semicolon, lexer );

    return result;
}

Expr* ParseParenExpr( Lexer* lexer )
{
    RequireTokenAndAdvance( TokenKind::OpenParen, lexer );
    Expr* result = ParseExpr( lexer );
    RequireTokenAndAdvance( TokenKind::CloseParen, lexer );

    return result;
}

StmtList* ParseStmtOrStmtBlock( Lexer* lexer, StmtList* parent )
{
    StmtList* result = nullptr;
    if( MatchToken( TokenKind::OpenBrace, lexer->token ) )
    {
        result = ParseStmtBlock( lexer, parent, nullptr );
    }
    else
    {
        result = NewStmtList( lexer->token.pos, parent, nullptr );

        BucketArray<Stmt*> stmts( &globalTmpArena, 1, Temporary() );
        stmts.Push( ParseStmt( lexer, result ) );

        if( lexer->IsValid() )
            AddStmtListStmts( result, stmts );
    }

    return result;
}

Stmt* NewIfStmt( SourcePos const& pos, Expr* cond, StmtList* thenBlock, BucketArray<ElseIf> const& elseIfs, StmtList* elseBlock,
                 StmtList* parentBlock )
{
    Stmt* result = NewStmt( pos, Stmt::If, parentBlock );
    result->if_.cond = cond;
    result->if_.thenBlock = thenBlock;
    result->if_.elseBlock = elseBlock;
    new( &result->if_.elseIfs ) Array<ElseIf>( &globalTmpArena, elseIfs.count, Temporary() );
    elseIfs.CopyTo( &result->if_.elseIfs );

    return result;
}

Stmt* NewWhileStmt( SourcePos const& pos, Expr* cond, StmtList* block, bool isDoWhile, StmtList* parentBlock )
{
    Stmt* result = NewStmt( pos, Stmt::While, parentBlock );
    result->while_.cond = cond;
    result->while_.block = block;
    result->while_.isDoWhile = isDoWhile;

    return result;
}

Stmt* NewForStmt( SourcePos const& pos, char const* indexName, Expr* rangeExpr, StmtList* block, StmtList* parentBlock )
{
    Stmt* result = NewStmt( pos, Stmt::For, parentBlock );
    result->for_.indexName = indexName;
    result->for_.rangeExpr = rangeExpr;
    result->for_.block = block;

    return result;
}
        
Stmt* NewSwitchStmt( SourcePos const& pos, Expr* expr, BucketArray<SwitchCase> const& cases, StmtList* parentBlock )
{
    Stmt* result = NewStmt( pos, Stmt::Switch, parentBlock );
    result->switch_.expr = expr;
    new( &result->switch_.cases ) Array<SwitchCase>( &globalTmpArena, cases.count, Temporary() );
    cases.CopyTo( &result->switch_.cases );

    return result;
}

Stmt* NewReturnStmt( SourcePos const& pos, Expr* expr, StmtList* parentBlock )
{
    Stmt* result = NewStmt( pos, Stmt::Return, parentBlock );
    result->expr = expr;

    return result;
}

Stmt* NewBlockStmt( SourcePos const& pos, StmtList* block, StmtList* parentBlock )
{
    Stmt* result = NewStmt( pos, Stmt::Block, parentBlock );
    result->block = block;

    return result;
}

Stmt* ParseIfStmt( SourcePos const& pos, Lexer* lexer, StmtList* parentBlock )
{
    RequireKeywordAndAdvance( Keyword::If, lexer );

    // TODO Assign statements?
    Expr* cond = ParseParenExpr( lexer );

    StmtList* thenBlock = ParseStmtOrStmtBlock( lexer, parentBlock );

    StmtList* elseBlock = nullptr;
    BucketArray<ElseIf> elseIfs( &globalTmpArena, 8, Temporary() );

    while( MatchKeyword( Keyword::Else, lexer->token ) )
    {
        NextToken( lexer );
        if( MatchKeyword( Keyword::If, lexer->token ) )
        {
            NextToken( lexer );
            Expr* elseIfCond = ParseParenExpr( lexer );

            StmtList* elseIfBlock = ParseStmtOrStmtBlock( lexer, parentBlock );
            elseIfs.Push( { elseIfBlock, elseIfCond } );
        }
        else
        {
            elseBlock = ParseStmtOrStmtBlock( lexer, parentBlock );
            break;
        }
    }

    if( lexer->IsValid() )
        return NewIfStmt( pos, cond, thenBlock, elseIfs, elseBlock, parentBlock );

    return nullptr;
}

Stmt* ParseWhileStmt( SourcePos const& pos, Lexer* lexer, StmtList* parentBlock )
{
    RequireKeywordAndAdvance( Keyword::While, lexer );

    Expr* cond = ParseParenExpr( lexer );
    StmtList* block = ParseStmtOrStmtBlock( lexer, parentBlock );

    if( lexer->IsValid() )
        return NewWhileStmt( pos, cond, block, false, parentBlock );

    return nullptr;
}

Stmt* ParseDoWhileStmt( SourcePos const& pos, Lexer* lexer, StmtList* parentBlock )
{
    RequireKeywordAndAdvance( Keyword::Do, lexer );
    StmtList* block = ParseStmtOrStmtBlock( lexer, parentBlock );

    RequireKeywordAndAdvance( Keyword::While, lexer );
    Expr* cond = ParseParenExpr( lexer );

    RequireTokenAndAdvance( TokenKind::Semicolon, lexer );

    if( lexer->IsValid() )
        return NewWhileStmt( pos, cond, block, true, parentBlock );

    return nullptr;
}

Stmt* ParseForStmt( SourcePos const& pos, Lexer* lexer, StmtList* parentBlock )
{
    RequireKeywordAndAdvance( Keyword::For, lexer );
    RequireTokenAndAdvance( TokenKind::OpenParen, lexer );

    RequireToken( TokenKind::Name, lexer );
    char const* indexName = lexer->token.ident;
    NextToken( lexer );
    RequireKeywordAndAdvance( Keyword::In, lexer );
    Expr* rangeExpr = ParseExpr( lexer ); //ParseRangeExpr( lexer->token.pos, lexer );

    RequireTokenAndAdvance( TokenKind::CloseParen, lexer );
    StmtList* block = ParseStmtOrStmtBlock( lexer, parentBlock );

    if( lexer->IsValid() )
        return NewForStmt( pos, indexName, rangeExpr, block, parentBlock );

    return nullptr;
}

SwitchCase ParseSwitchCase( Lexer* lexer, StmtList* parentBlock )
{
    Expr* expr = nullptr;
    bool isDefault = false;

    if( MatchKeyword( Keyword::Default, lexer->token ) )
    {
        isDefault = true;
        NextToken( lexer );
    }
    else
    {
        RequireKeywordAndAdvance( Keyword::Case, lexer );
        expr = ParseCommaExpr( lexer );
    }

    RequireTokenAndAdvance( TokenKind::Colon, lexer );

    StmtList* block = ParseStmtOrStmtBlock( lexer, parentBlock );

    return { block, expr, isDefault };
}

Stmt* ParseSwitchStmt( SourcePos const& pos, Lexer* lexer, StmtList* parentBlock )
{
    RequireKeywordAndAdvance( Keyword::Switch, lexer );
    Expr* expr = ParseParenExpr( lexer );

    // TODO Let's test without these for now
    //RequireTokenAndAdvance( TokenKind::OpenBrace, lexer );

    bool hasDefault = false;
    BucketArray<SwitchCase> cases( &globalTmpArena, 16, Temporary() );
    while( !MatchToken( TokenKind::CloseBrace, lexer->token ) )
    {
        SourcePos casePos = lexer->pos;

        SwitchCase c = ParseSwitchCase( lexer, parentBlock );
        if( c.isDefault )
        {
            if( hasDefault )
            {
                PARSE_ERROR( casePos, "Switch statement can only have one default case" );
                break;
            }
            else
                hasDefault = true;
        }
        cases.Push( c );
    }

    //RequireTokenAndAdvance( TokenKind::CloseBrace, lexer );

    if( lexer->IsValid() )
        return NewSwitchStmt( pos, expr, cases, parentBlock );

    return nullptr;
}

Stmt* ParseStmt( Lexer* lexer, StmtList* parentBlock )
{
    char const* directive = ParseDirective( lexer );

    Stmt* stmt = nullptr;
    bool needSemicolon = false;
    SourcePos pos = lexer->token.pos;

    if( MatchKeyword( Keyword::If, lexer->token ) )
        stmt = ParseIfStmt( pos, lexer, parentBlock );
    else if( MatchKeyword( Keyword::While, lexer->token ) )
        stmt = ParseWhileStmt( pos, lexer, parentBlock );
    else if( MatchKeyword( Keyword::Do, lexer->token ) )
        stmt = ParseDoWhileStmt( pos, lexer, parentBlock );
    else if( MatchKeyword( Keyword::For, lexer->token ) )
        stmt = ParseForStmt( pos, lexer, parentBlock );
    else if( MatchKeyword( Keyword::Switch, lexer->token ) )
        stmt = ParseSwitchStmt( pos, lexer, parentBlock );

    else if( MatchKeyword( Keyword::Break, lexer->token ) )
    {
        stmt = NewStmt( pos, Stmt::Break, parentBlock );
        NextToken( lexer );
        needSemicolon = true;
    }
    else if( MatchKeyword( Keyword::Continue, lexer->token ) )
    {
        stmt = NewStmt( pos, Stmt::Continue, parentBlock );
        needSemicolon = true;
        NextToken( lexer );
    }
    else if( MatchKeyword( Keyword::Return, lexer->token ) )
    {
        Expr* expr = nullptr;

        NextToken( lexer );
        if( !MatchToken( TokenKind::Semicolon, lexer->token ) )
            expr = ParseExpr( lexer );

        if( lexer->IsValid() )
            stmt = NewReturnStmt( pos, expr, parentBlock );
        needSemicolon = true;
    }

    else if( MatchToken( TokenKind::OpenBrace, lexer->token ) )
    {
        StmtList* block = ParseStmtBlock( lexer, parentBlock, nullptr );

        if( lexer->IsValid() )
            stmt = NewBlockStmt( pos, block, parentBlock );
    }

    else
    {
        stmt = ParseSimpleStmt( pos, lexer, parentBlock );
    }

    if( needSemicolon )
        RequireTokenAndAdvance( TokenKind::Semicolon, lexer );

    return stmt;
}


Array<Decl*> Parse( String const& program, char const* filename )
{
    Lexer lexer = Lexer( program, filename );

    ScopedTmpMemory tmpMemory( &globalTmpArena );
    BucketArray<Decl*> decls( &globalTmpArena, 16, Temporary() );

    Token const& token = lexer.token;
    while( lexer.IsValid() )
    {
        if( token.kind == TokenKind::EndOfStream )
            break;

        Decl* decl = ParseDecl( &lexer, nullptr );
        if( decl )
            decls.Push( decl );
    }

    Array<Decl*> result( &globalArena, decls.count );
    decls.CopyTo( &result );

    return result;
}


#define INDENT \
{ \
    len = snprintf( outBuf, Size( maxLen ), "%*s", indent * 4, "" ); \
    outBuf += len; \
    maxLen -= len; \
} 

#define APPEND(fmt, ...) \
{ \
    len = snprintf( outBuf, Size( maxLen ), fmt, ##__VA_ARGS__ ); \
    outBuf += len; \
    maxLen -= len; \
} 

void DebugPrintSExpr( Expr* expr, char*& outBuf, sz& maxLen );

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
            for( FuncArgSpec const& a : type->func.args )
            {
                if( &a != type->func.args.begin() )
                    APPEND( ", " );
                DebugPrintTypeSpec( a.type, outBuf, maxLen );
            }
            APPEND( " ) -> " );
            DebugPrintTypeSpec( type->func.returnType, outBuf, maxLen );
        } break;
        case TypeSpec::Array:
        {
            APPEND( "[" );
            if( type->array.count )
                DebugPrintSExpr( type->array.count, outBuf, maxLen );
            APPEND( "] " );
            DebugPrintTypeSpec( type->array.base, outBuf, maxLen );
        } break;
        case TypeSpec::Pointer:
        {
            APPEND( "*" );
            DebugPrintTypeSpec( type->ptr.base, outBuf, maxLen );
        } break;
    }
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
            APPEND( "%s", expr->name.ident );
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
        case Expr::Cast:
        {
            APPEND( "(" );
            DebugPrintSExpr( expr->cast.expr, outBuf, maxLen );
            APPEND( " as " );
            DebugPrintTypeSpec( expr->cast.type, outBuf, maxLen );
            APPEND( ")" );
        } break;

        case Expr::Compound:
        {
            APPEND( "({} " );
            int i = 0;
            for( CompoundField const& f : expr->compoundFields )
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
            APPEND( "(%s ", TokenKind::Items::names[ expr->unary.op ] );
            DebugPrintSExpr( expr->unary.expr, outBuf, maxLen );
            APPEND( ")" );
        } break;
        case Expr::Binary:
        {
            APPEND( "(%s ", TokenKind::Items::names[ expr->binary.op ] );
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
        case Expr::Comma:
        {
            APPEND( "(" );
            for( Expr* e : expr->commaExprs )
            {
                if( e != expr->commaExprs[0] )
                    APPEND( ", " );
                DebugPrintSExpr( e, outBuf, maxLen );
            }
            APPEND( ")" );
        } break;

        case Expr::Sizeof:
        {
            APPEND( "(sizeof " );
            DebugPrintSExpr( expr->sizeofExpr, outBuf, maxLen );
            APPEND( ")" );
        } break;
        case Expr::Range:
        {
            APPEND( "(" );
            DebugPrintSExpr( expr->range.lowerBound, outBuf, maxLen );
            if( expr->range.upperBound )
            {
                APPEND( " .. " );
                DebugPrintSExpr( expr->range.upperBound, outBuf, maxLen );
            }
            APPEND( ")" );
        } break;
    }
}

void DebugPrintSExpr( Decl* decl, char*& outBuf, sz& maxLen, int& indent );
void DebugPrintSExpr( Stmt* stmt, char*& outBuf, sz& maxLen, int& indent );

void DebugPrintStmtList( StmtList const* block, char*& outBuf, sz& maxLen, int& indent )
{
    int len = 0;

    INDENT APPEND( "(block \n" );
    indent++;
    for( Stmt* s : block->stmts )
    {
        if( s != block->stmts[0] )
            APPEND( "\n" );
        INDENT;
        DebugPrintSExpr( s, outBuf, maxLen, indent );
        APPEND( "," );
    }
    APPEND( ")\n" );
    indent--;
}

void DebugPrintSExpr( Stmt* stmt, char*& outBuf, sz& maxLen, int& indent )
{
    int len = 0;
    switch( stmt->kind )
    {
        case Stmt::Expr:
        {
            DebugPrintSExpr( stmt->expr, outBuf, maxLen );
        } break;
        case Stmt::Decl:
        {
            DebugPrintSExpr( stmt->decl, outBuf, maxLen, indent );
        } break;
        case Stmt::Assign:
        {
            APPEND( "(%s ", TokenKind::Items::names[ stmt->assign.op ] );
            DebugPrintSExpr( stmt->assign.left, outBuf, maxLen );
            APPEND( " " );
            DebugPrintSExpr( stmt->assign.right, outBuf, maxLen );
            APPEND( ")" );
        } break;

        case Stmt::If:
        {
            APPEND( "(if " );
            DebugPrintSExpr( stmt->if_.cond, outBuf, maxLen );
            APPEND( "\n" );
            DebugPrintStmtList( stmt->if_.thenBlock, outBuf, maxLen, indent );

            for( auto& elseIf : stmt->if_.elseIfs )
            {
                INDENT APPEND( ")(else if " );
                DebugPrintSExpr( elseIf.cond, outBuf, maxLen );
                APPEND( "\n" );
                DebugPrintStmtList( elseIf.block, outBuf, maxLen, indent );
            }

            if( stmt->if_.elseBlock->stmts.count )
            {
                INDENT APPEND( ")(else\n" );
                DebugPrintStmtList( stmt->if_.elseBlock, outBuf, maxLen, indent );
            }
            INDENT APPEND( ")" );
        } break;
        case Stmt::While:
        {
            APPEND( "(%swhile ", stmt->while_.isDoWhile ? "do " : "" );
            DebugPrintSExpr( stmt->while_.cond, outBuf, maxLen );
            APPEND( "\n" );
            DebugPrintStmtList( stmt->while_.block, outBuf, maxLen, indent );
            INDENT APPEND( ")" );
        } break;
        case Stmt::For:
        {
            APPEND( "(for %s in", stmt->for_.indexName );
            DebugPrintSExpr( stmt->for_.rangeExpr, outBuf, maxLen );
            APPEND( "\n" );
            DebugPrintStmtList( stmt->for_.block, outBuf, maxLen, indent );
            INDENT APPEND( ")" );
        } break;
        case Stmt::Switch:
        {
            APPEND( "(switch " );
            DebugPrintSExpr( stmt->switch_.expr, outBuf, maxLen );
            APPEND( "\n" );
            for( SwitchCase const& c : stmt->switch_.cases ) 
            {
                INDENT APPEND( "(%s ", c.isDefault ? "default" : "case" );
                if( !c.isDefault )
                    DebugPrintSExpr( c.expr, outBuf, maxLen );
                APPEND( "\n" );
                indent++;
                DebugPrintStmtList( c.block, outBuf, maxLen, indent );
                INDENT APPEND( ")\n" );
                indent--;
            }
            INDENT APPEND( ")" );
        } break;

        case Stmt::Break:
        {
            APPEND( "(break)" );
        } break;
        case Stmt::Continue:
        {
            APPEND( "(continue)" );
        } break;
        case Stmt::Return:
        {
            APPEND( "(return " );
            DebugPrintSExpr( stmt->expr, outBuf, maxLen );
            APPEND( ")" );
        } break;

        case Stmt::Block:
        {
            DebugPrintStmtList( stmt->block, outBuf, maxLen, indent );
        } break;
    }
}

void DebugPrintSExpr( Decl* decl, char*& outBuf, sz& maxLen, int& indent )
{
    int len = 0;
    switch( decl->kind )
    {
        case Decl::Enum:
        {
            APPEND( "(enum %s: ", decl->names[0] );
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
            APPEND( "(struct %s\n", decl->names[0] );
            indent++;
            for( auto& it : decl->aggregate.items )
            {
                INDENT;
                DebugPrintSExpr( it, outBuf, maxLen, indent );
                APPEND( ",\n" );
            }
            APPEND( ")\n" );
            indent--;
        } break;

        case Decl::Union:
        {
            APPEND( "(union %s\n", decl->names[0] );
            indent++;
            for( auto& it : decl->aggregate.items )
            {
                INDENT;
                DebugPrintSExpr( it, outBuf, maxLen, indent );
                APPEND( ",\n" );
            }
            APPEND( ")\n" );
            indent--;
        } break;

        case Decl::Func:
        {
            APPEND( "(func %s ( ", decl->names[0] );
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

            indent++;
            for( Stmt* s : decl->func.body->stmts )
            {
                INDENT;
                DebugPrintSExpr( s, outBuf, maxLen, indent );
                APPEND( ",\n" );
            }
            indent--;

        } break;

        case Decl::Var:
        {
            APPEND( "(%s ", decl->var.isConst ? "const" : "var" );
            for( char const*& name : decl->names )
            {
                if( &name != decl->names.begin() )
                    APPEND( ", " );
                APPEND( "%s", name );
            }
            APPEND( ": " );
            DebugPrintTypeSpec( decl->var.type, outBuf, maxLen );
            APPEND( " = " );
            DebugPrintSExpr( decl->var.initExpr, outBuf, maxLen );
            APPEND( ")" );
        } break;

        case Decl::Import:
        {

        } break;
    }
}

#undef APPEND
#undef INDENT
