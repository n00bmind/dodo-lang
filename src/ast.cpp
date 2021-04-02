
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

TypeSpec* NewFuncTypeSpec( SourcePos const& pos, BucketArray<FuncArgSpec> const& args, BucketArray<TypeSpec*> const& returnTypes )
{
    TypeSpec* result = NewTypeSpec( pos, TypeSpec::Func );
    INIT( result->func.args ) Array<FuncArgSpec>( args, &globalArena );
    INIT( result->func.returnTypes ) Array<TypeSpec*>( returnTypes, &globalArena );

    return result;
}

TypeSpec* NewArrayTypeSpec( SourcePos const& pos, TypeSpec* ofType, Expr* size, bool isView )
{
    TypeSpec* result = NewTypeSpec( pos, TypeSpec::Array );
    result->array.base = ofType;
    result->array.length = size;
    result->array.isView = isView;

    return result;
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

Expr* NewCallExpr( SourcePos const& pos, Expr* expr, BucketArray<ArgExpr> const& args )
{
    Expr* result = NewExpr( pos, Expr::Call );
    result->call.func = expr;
    INIT( result->call.args ) Array<ArgExpr>( args, &globalArena );

    return result;
}

Expr* NewIndexExpr( SourcePos const& pos, Expr* base, Expr* index )
{
    Expr* result = NewExpr( pos, Expr::Index );
    result->index.base = base;
    result->index.index = index;

    return result;
}

Expr* NewFieldExpr( SourcePos const& pos, Expr* base, char const* name, bool isMeta )
{
    Expr* result = NewExpr( pos, Expr::Field );
    result->field.base = base;
    result->field.name = name;
    result->field.isMeta = isMeta;

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

Expr* NewLambdaExpr( SourcePos const& pos, Decl* decl )
{
    Expr* result = NewExpr( pos, Expr::Lambda );
    result->lambda.decl = decl;

    return result;
}


Decl* NewDecl( SourcePos const& pos, Decl::Kind kind, char const* name, BucketArray<NodeDirective> const& directives, u32 flags,
               StmtList* parentBlock )
{
    Decl* result = PUSH_STRUCT( &globalArena, Decl );
    result->pos = pos;
    new( &result->names ) Array<char const*>( &globalArena, 1 );
    result->names.Push( name );
    result->parentBlock = parentBlock;
    result->kind = kind;
    INIT( result->directives ) Array<NodeDirective>( directives, &globalArena );
    result->flags = flags;

    return result;
}

Decl* NewDecl( SourcePos const& pos, Decl::Kind kind, BucketArray<char const*> const& names, BucketArray<NodeDirective> const& directives,
               u32 flags, StmtList* parentBlock )
{
    Decl* result = PUSH_STRUCT( &globalArena, Decl );
    result->pos = pos;
    // Copy names array to ensure it's in the global arena
    new( &result->names ) Array<char const*>( &globalArena, names.count );
    names.CopyTo( &result->names );
    result->parentBlock = parentBlock;
    result->kind = kind;
    INIT( result->directives ) Array<NodeDirective>( directives, &globalArena );
    result->flags = flags;

    return result;
}

Decl* NewAggregateDecl( SourcePos const& pos, int kind, char const* name, BucketArray<Decl*> const& items,
                        BucketArray<NodeDirective> const& directives, u32 flags, StmtList* parentBlock  )
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
        result = NewDecl( pos, declKind, name, directives, flags, parentBlock );
        new( &result->aggregate.items ) Array<Decl*>( &globalArena, items.count );
        items.CopyTo( &result->aggregate.items );
    }

    return result;
}

Decl* NewEnumDecl( SourcePos const& pos, char const* name, TypeSpec* type, BucketArray<EnumItem> const& items,
                   BucketArray<NodeDirective> const& directives, u32 flags, StmtList* parentBlock )
{
    Decl* result = NewDecl( pos, Decl::Enum, name, directives, flags, parentBlock );
    new( &result->enum_.items ) Array<EnumItem>( &globalArena, items.count );
    items.CopyTo( &result->enum_.items );
    result->enum_.type = type;

    return result;
}

Decl* NewFuncDecl( SourcePos const& pos, char const* name, BucketArray<FuncArgDecl> const& args, StmtList* body,
                   BucketArray<TypeSpec*> const& returnTypes, BucketArray<NodeDirective> const& directives, u32 flags, StmtList* parentBlock )
{
    Decl* result = NewDecl( pos, Decl::Func, name, directives, flags, parentBlock );
    INIT( result->func.args ) Array<FuncArgDecl>( args, &globalArena );
    result->func.body = body;
    INIT( result->func.returnTypes ) Array<TypeSpec*>( returnTypes, &globalArena );

    return result;
}

Decl* NewVarDecl( SourcePos const& pos, BucketArray<char const*> const& names, TypeSpec* type, Expr* initExpr, bool isConst,
                  BucketArray<NodeDirective> const& directives, u32 flags, StmtList* parentBlock )
{
    Decl* result = NewDecl( pos, Decl::Var, names, directives, flags, parentBlock );
    result->var.type = type;
    result->var.initExpr = initExpr;
    result->var.isConst = isConst;

    return result;
}

// For synthetic Decls
Decl* NewVarDecl( SourcePos const& pos, char const* name, TypeSpec* type, Expr* initExpr, BucketArray<NodeDirective> const& directives,
                  u32 flags, StmtList* parentBlock )
{
    BucketArray<char const*> names( &globalTmpArena, 1, Temporary() );
    names.Push( name );

    return NewVarDecl( pos, names, type, initExpr, false, directives, flags, parentBlock );
}


Stmt* NewStmt( SourcePos const& pos, Stmt::Kind kind, StmtList* parentBlock, BucketArray<NodeDirective> const& directives, u32 flags )
{
    Stmt* result = PUSH_STRUCT( &globalArena, Stmt );
    result->pos = pos;
    result->kind = kind;
    result->parentBlock = parentBlock;
    INIT( result->directives ) Array<NodeDirective>( directives, &globalArena );
    result->flags = flags;

    return result;
}

Stmt* NewDeclStmt( SourcePos const& pos, Decl* decl, StmtList* parentBlock, BucketArray<NodeDirective> const& directives, u32 flags )
{
    Stmt* result = NewStmt( pos, Stmt::Decl, parentBlock, directives, flags );
    result->decl = decl;

    return result;
}

Stmt* NewAssignStmt( SourcePos const& pos, Expr* leftExpr, TokenKind::Enum op, Expr* rightExpr, StmtList* parentBlock,
                     BucketArray<NodeDirective> const& directives, u32 flags )
{
    Stmt* result = NewStmt( pos, Stmt::Assign, parentBlock, directives, flags );
    result->assign.left = leftExpr;
    result->assign.right = rightExpr;
    result->assign.op = op;

    return result;
}

Stmt* NewExprStmt( SourcePos const& pos, Expr* expr, StmtList* parentBlock, BucketArray<NodeDirective> const& directives, u32 flags )
{
    Stmt* result = NewStmt( pos, Stmt::Expr, parentBlock, directives, flags );
    result->expr = expr;
    
    return result;
}

Stmt* NewIfStmt( SourcePos const& pos, Expr* cond, StmtList* thenBlock, BucketArray<ElseIf> const& elseIfs, StmtList* elseBlock,
                 StmtList* parentBlock, BucketArray<NodeDirective> const& directives, u32 flags )
{
    Stmt* result = NewStmt( pos, Stmt::If, parentBlock, directives, flags );
    result->if_.cond = cond;
    result->if_.thenBlock = thenBlock;
    result->if_.elseBlock = elseBlock;
    INIT( result->if_.elseIfs ) Array<ElseIf>( elseIfs, &globalArena );

    return result;
}

Stmt* NewWhileStmt( SourcePos const& pos, Expr* cond, StmtList* block, bool isDoWhile, StmtList* parentBlock,
                    BucketArray<NodeDirective> const& directives, u32 flags )
{
    Stmt* result = NewStmt( pos, Stmt::While, parentBlock, directives, flags );
    result->while_.cond = cond;
    result->while_.block = block;
    result->while_.isDoWhile = isDoWhile;

    return result;
}

Stmt* NewForStmt( SourcePos const& pos, char const* indexName, Expr* rangeExpr, StmtList* block, StmtList* parentBlock,
                  BucketArray<NodeDirective> const& directives, u32 flags )
{
    Stmt* result = NewStmt( pos, Stmt::For, parentBlock, directives, flags );
    result->for_.indexName = indexName;
    result->for_.rangeExpr = rangeExpr;
    result->for_.block = block;

    return result;
}
        
Stmt* NewSwitchStmt( SourcePos const& pos, Expr* expr, BucketArray<SwitchCase> const& cases, StmtList* parentBlock,
                     BucketArray<NodeDirective> const& directives, u32 flags )
{
    Stmt* result = NewStmt( pos, Stmt::Switch, parentBlock, directives, flags );
    result->switch_.expr = expr;
    INIT( result->switch_.cases ) Array<SwitchCase>( cases, &globalTmpArena );

    return result;
}

Stmt* NewReturnStmt( SourcePos const& pos, Expr* expr, StmtList* parentBlock, BucketArray<NodeDirective> const& directives, u32 flags )
{
    Stmt* result = NewStmt( pos, Stmt::Return, parentBlock, directives, flags );
    result->expr = expr;

    return result;
}

Stmt* NewBlockStmt( SourcePos const& pos, StmtList* block, StmtList* parentBlock, BucketArray<NodeDirective> const& directives, u32 flags )
{
    Stmt* result = NewStmt( pos, Stmt::Block, parentBlock, directives, flags );
    result->block = block;

    return result;
}

void GlobalPathBuilder( StringBuilder* path, StmtList* parent, char const* name )
{
    StmtList* p = parent;
    while( p )
    {
        GlobalPathBuilder( path, p->parent, p->name );
        p = p->parent;
    }

    if( !path->Empty() )
        path->Append( "_" );

    if( name )
        path->Append( name );
    else
    {
        ASSERT( parent );
        path->AppendFmt( "%d", parent->childCount );
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

