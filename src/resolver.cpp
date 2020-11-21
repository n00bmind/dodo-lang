
struct Symbol
{
    enum Kind
    {
        None = 0,
        Var,
        Const,
        Func,
        //Type,     // This is just for typedefs apparently
        //Module,
    };

    enum State
    {
        Unresolved = 0,
        Resolving,
        Resolved,
    };

    char const* name;
    Decl* decl;
    Type* type;
    Kind kind;
    State state;

    union
    {
        i64 intValue;
    } value;
};

// TODO Hashtable!
BucketArray<Symbol> globalSymbolList;


struct Type;

struct TypeField
{
    char const* name;
    Type* type;
    sz offset;
};

struct Type
{
    enum Kind
    {
        None = 0,
        Incomplete,
        Completing,
        Void,
        Bool,
        Int,
        Float,
        Pointer,
        Array,
        Struct,
        Union,
        Enum,
        Func,
    };

    // TODO Should be decl?
    //Symbol* symbol;   // NOTE Will be null for all primitive types
    Decl* decl;
    Kind kind;

    union
    {
        struct
        {
            ::Array<Type*> args;
            // TODO Varargs
            Type* returnType;
        } func;
        struct
        {
            ::Array<TypeField> fields;
        } aggregate;
        struct
        {
            Type* base;
            sz count;
        } array;
        struct
        {
            Type* base;
        } ptr;
    };
};

Type intTypeVal = { nullptr, Type::Int };
Type floatTypeVal = { nullptr, Type::Float };

Type* intType = &intTypeVal;
Type* floatType = &floatTypeVal;


#if 0
struct CachedType
{
    Type* type;
    union
    {
        struct
        {
            ::Array<Type*> args;
            // TODO Varargs
            Type* returnType;
        } func;
        struct
        {
            Type* base;
            sz count;
        } array;
        struct
        {
            Type* base;
        } ptr;
    };
};
#endif

// List of unique interned types (a.k.a hash consing)
// TODO Hashtable!
//BucketArray<CachedType> globalCachedTypes;
BucketArray<Type*> globalCachedTypes;


struct ResolvedExpr
{
    Type* type;
    bool isLvalue;
    bool isConst;
    // TODO 
    u64 intValue;
};

ResolvedExpr resolvedNull = {};

ResolvedExpr NewResolvedRvalue( Type* type )
{
    ResolvedExpr result = {};
    result.type = type;
    return result;
}

ResolvedExpr NewResolvedLvalue( Type* type )
{
    ResolvedExpr result = {};
    result.type = type;
    result.isLvalue = true;
    return result;
}

ResolvedExpr ResolveNameExpr( Expr* expr )
{
    ASSERT( expr->kind == Expr::Name );

    Symbol* sym = ResolveName( expr->name );
    if( symbol->kind == Symbol::Var )
        return NewResolvedLvalue( sym->type );
    else if( symbol->kind == Symbol::Const )
        // TODO 
        return NewResolvedConst( sym->value.intValue );
    // TODO Functions
    else
    {
        RSLV_ERROR( expr->pos, "Named expression can only refer to variable or constant" );
        return resolvedNull;
    }
}

ResolvedExpr ResolveUnaryExpr( Expr* expr )
{
    ASSERT( expr->kind == Expr::Unary );

    ResolvedExpr operand = ResolveExpr( expr->unary.expr );
    Type* type = operand.type;

    switch( expr->unary.op )
    {
        case TokenKind::Asterisk:
        {
            if( type->kind != Type::Pointer )
            {
                RSLV_ERROR( expr->pos, "Cannot dereference non-pointer type" );
                return resolvedNull;
            }
            return NewResolvedLvalue( type->ptr.base );
        } break;
        case TokenKind::Ampersand:
        {
            if( !operand.isLvalue )
            {
                RSLV_ERROR( expr->pos, "Cannot take address of non-lvalue" );
                return resolvedNull;
            }
            return NewResolvedRvalue( NewPtrType( type ) );
        } break;
        default:
        {
            NOT_IMPLEMENTED;
            return resolvedNull;
        } break;
    }
}

ResolvedExpr ResolveBinaryExpr( Expr* expr )
{
    ASSERT( expr->kind == Expr::Binary );

    ResolvedExpr left = ResolveExpr( expr->binary.left );
    ResolvedExpr right = ResolveExpr( expr->binary.right );

    // TODO Check left and right types

    switch( expr->binary.op )
    {
        case TokenKind::Plus:
        {
            if( left.isConst && right.isConst )
                // TODO 
                return NewResolvedConst( left.value.intValue + right.value.intValue );
            else
                return NewResolvedRvalue( left.type );
        } break;
        default:
        {
            NOT_IMPLEMENTED;
            return resolvedNull;
        } break;
    }
}

ResolvedExpr ResolveExpr( Expr* expr )
{
    switch( expr->kind )
    {
        case Expr::Int:
        {
            return NewResolvedConst( expr->value->intValue );
        } break;
        case Expr::Name:
        {
            return ResolveNameExpr( expr );
        } break;
        case Expr::Field:
        {
            return ResolveFieldExpr( expr );
        } break;
        case Expr::Unary:
        {
            return ResolveUnaryExpr( expr );
        } break;
        case Expr::Binary:
        {
            return ResolveBinaryExpr( expr );
        } break;
        case Expr::Sizeof:
        {
            ResolvedExpr result = ResolveExpr( expr->sizeofExpr );
            Type* type = result.type;
            CompleteType( type );

            return NewResolvedConst( NewSizeofType( type ) );
        } break;

        default:
        {
            NOT_IMPLEMENTED;
            return resolvedNull;
        } break;
    }
}

Type* NewType( Type::Kind kind )
{
    Type* result = PUSH_STRUCT( &globalArena, Type );
    result->kind = kind;
    
    return result;
}

Type* NewPtrType( Type* base )
{
    auto idx = globalCachedTypes.First(); 
    while( idx )
    {
        Type* t = *idx;
        if( t->kind == Type::Pointer )
        {
            if( t->ptr.base == base )
                return t;
        }
        idx.Next();
    }

    Type* result = NewType( Type::Pointer );
    result->ptr.base = base;

#if 0
    CachedType cached = {};
    cached.type = result;
    cached.ptr.base = base;
#endif
    globalCachedTypes.Push( result );

    return result;
}

Type* NewArrayType( Type* base, u64 count )
{
    auto idx = globalCachedTypes.First(); 
    while( idx )
    {
        Type* t = *idx;
        if( t->kind == Type::Array )
        {
            if( t->array.base == base && t->array.count == count )
                return t;
        }
        idx.Next();
    }

    Type* result = NewType( Type::Array );
    result->array.base = base;
    result->array.count = count;

#if 0
    CachedType cached = {};
    cached.type = result;
    cached.array.base = base;
    cached.array.count = count;
#endif
    globalCachedTypes.Push( result );

    return result;
}

Type* NewFuncType( Array<Type*> const& args, Type* returnType )
{
    auto idx = globalCachedTypes.First(); 
    while( idx )
    {
        Type* t = *idx;
        if( t->kind == Type::Func )
        {
            if( t->func.args == args && t->func.returnType == returnType )
                return t;
        }
        idx.Next();
    }

    Type* result = NewType( Type::Func );
    result->func.args = args;
    result->func.returnType = returnType;

#if 0
    CachedType cached = {};
    cached.type = result;
    cached.array.base = base;
    cached.array.count = count;
#endif
    globalCachedTypes.Push( result );

    return result;
}

Type* NewIncompleteType( Symbol* sym )
{
    Type* result = NewType( Type::Incomplete );
    result->symbol = sym;
    return result;
}

void CompleteStructType( Type* type, Array<TypeField> const& fields )
{
    ASSERT( type->kind == Type::Completing );
    type->kind = Type::Struct;
    type->aggregate.fields = fields;

    type->size = 0;
    for( TypeField const& f : type->aggregate.fields )
    {
        ASSERT( f->type->kind > Type::Completing );
        // TODO Alignment, field offset, etc.
        type->size += SizeOfType( f->type );
    }
}

void CompleteUnionType( Type* type, Array<TypeField> const& fields )
{
    ASSERT( type->kind == Type::Completing );
    type->kind = Type::Union;
    type->aggregate.fields = fields;

    type->size = 0;
    for( TypeField const& f : type->aggregate.fields )
    {
        ASSERT( f->type->kind > Type::Completing );
        // TODO Alignment, field offset, etc.
        type->size = Max( type->size, SizeOfType( f->type ) );
    }
}

void CompleteType( Type* type )
{
    ASSERT( type->kind );

    Decl* decl = type->symbol->decl;
    SourcePos const& pos = decl->pos;
    if( type->kind == Type::Completing )
    {
        // TODO
        RSLV_ERROR( pos, "Circular dependency detected" );
        return;
    }
    else if( type->kind != Type::Incomplete )
        // Nothing to do
        return;

    type->kind = Type::Completing;
    ASSERT( decl->kind == Decl::Struct || decl->kind == Decl::Union );

    Array<TypeField> fields( &globalArena, decl->aggregate.items.count );
    for( Decl* d : decl->aggregate.items )
    {
        Symbol* sym = ResolveDecl( d );
        CompleteType( sym->type );
        // If there's several names, create a separate field for each
        // TODO Should have returned several symbols above?
        for( char const* name : d->names )
        {
            // TODO Offset
            field.Push( { name, sym->type } );
        }
    }

    if( decl->kind == Decl::Struct )
        CompleteStructType( type, fields );
    else
        CompleteUnionType( type, fields );

    globalOrderedSymbols.Push( type->symbol );
}

Type* ResolveTypeSpec( TypeSpec* spec )
{
    Type* result = nullptr;
    SourcePos const& pos = spec->pos;

    switch( spec->kind )
    {
        case TypeSpec::Name:
        {
            ASSERT( spec->names.count );

            // TODO Resolve namespaces
            if( spec->names.count > 1 )
            {
                RSLV_ERROR( pos, "Namespaces not yet supported" );
                break;
            }

            char const* name = spec->names.Back();
            Symbol* sym = ResolveName( spec->name, pos );
            if( sym->kind != Symbol::Type )
            {
                RSLV_ERROR( pos, "'%s' must denote a type", name );
                break;
            }

            result = sym->type;
        } break;
        case TypeSpec::Pointer:
        {
            Type* base = ResolveTypeSpec( spec->ptr.base );
            result = NewPtrType( base );
        } break;
        case TypeSpec::Array:
        {
            u64 count = ResolveConstIntExpr( spec->array.count );
            Type* base = ResolveTypeSpec( spec->array.base );
            result = NewArrayType( base, count );
        } break;
        case TypeSpec::Func:
        {
            Array<Type*> args( &globalArena, spec->func.args.count );
            for( TypeSpec* argSpec : spec->func.args )
            {
                Type* arg = ResolveTypeSpec( argSpec );
                args.Push( arg );
            }
            Type* ret = ResolveTypeSpec( spec->func.returnType );
            result = NewFuncType( args, ret );
        } break;

        INVALID_DEFAULT_CASE
    }
}


Symbol* GetSymbol( char const* name )
{
    Symbol* result = nullptr;

    auto idx = globalSymbolList.First();
    while( idx )
    {
        if( (*idx).name == name )
            break;

        idx.Next();
    }

    if( idx )
        result = &*idx;

    return result;
}

Symbol* PushSymbol( Decl* decl )
{
    ASSERT( decl->name );
    ASSERT( GetSymbol( decl->name ) == nullptr );

    Symbol result = {};
    result->decl = decl;
    result->name = decl->name;
    result->state = Symbol::Unresolved;

    return globalSymbolList.Push( result );
}

void ResolveSymbol( Symbol* sym )
{
    if( sym->state == Symbol::Resolved )
        // Nothing to do
        return;

    SourcePos const& pos = sym->decl->pos;
    if( sym->state == Symbol::Resolving )
    {
        // TODO Log dependency graph nodes to aid debugging?
        RSLV_ERROR( pos, "Circular dependency detected" );
        return;
    }

    ASSERT( sym->state == Symbol::Unresolved );
    sym->state == Symbol::Resolving;

    switch( sym->kind )
    {
        case Symbol::Var:
        {
            ASSERT( decl->kind == Decl::Var );
            
            Type* type = nullptr;
            if( decl->var.type )
                type = ResolveTypeSpec( decl->var.type );

            // TODO Multiple names/inits?
            auto& names decl->var.names;
            if( decl->var.initExpr )
            {
                ResolvedExpr init = ResolveExpr( decl->var.initExpr );
                if( type && init.type != type )
                    RSLV_ERROR( pos, "Declared type does not match inferred type" );

                type = init.type;
            }

            CompleteType( type );
            sym->type = type;
        } break;

        case Symbol::Const:
        {
        } break;
        //case Symbol::Type:
        //{
        //} break;
        case Symbol::Func:
        {
        } break;
        case Symbol::Module:
        {
        } break;

        INVALID_DEFAULT_CASE
    }

    sym->state = Symbol::Resolved;
    globalOrderedSymbols.Push( sym );
}

void CompleteSymbol( Symbol* sym )
{
    ResolveSymbol( sym );

    if( sym->kind == Symbol::Type )
        CompleteType( sym->type );
}

Symbol* ResolveName( char const* name, SourcePos const& pos )
{
    Symbol* sym = GetSymbol( name );
    if( sym )
        ResolveSymbol( sym, pos );
    else
        RSLV_ERROR( pos, "Undefined symbol '%s'", name );

    return sym;
}

void ResolveSymbols()
{
    // TODO
    SourcePos pos = {};

    auto idx = globalSymbolList.First();
    while( idx )
    {
        ResolveSymbol( &*idx, pos );
        idx.Next();
    }
}

Symbol* CreateTypeSymbol( char const* name, Type* type )
{
    Symbol result = {};
    result->name = name;
    result->type = type;
    result->kind = Symbol::Type;
    result->state = Symbol::Resolved;

    return globalSymbolList.Push( result );
}


void InitResolver()
{
    new( &globalSymbolList ) BucketArray<Symbol>( &globalArena, 256 );
    new( &globalCachedTypes ) BucketArray<Type*>( &globalArena, 256 );

    CreateTypeSymbol( "int", intType );
    CreateTypeSymbol( "float", floatType );
}
