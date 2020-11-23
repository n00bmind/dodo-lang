struct Type;

struct Value
{
    union
    {
        void* ptrValue;
        u64 intValue;
        bool boolValue;
    };
};

struct Symbol
{
    enum Kind
    {
        None = 0,
        Var,
        Const,
        Func,
        Type,
        Module,
    };

    enum State
    {
        Unresolved = 0,
        Resolving,
        Resolved,
    };

    char const* name;
    Decl* decl;
    ::Type* type;
    Value value;
    Kind kind;
    State state;
};

// TODO Hashtable!
BucketArray<Symbol> globalSymbolList;
BucketArray<Symbol*> globalOrderedSymbols;


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

    // TODO Fill up!
    char const* name;
    // TODO Should be decl?
    Symbol* symbol;   // NOTE Only aggregates and enums will have one
    // TODO Remove?
    Decl* decl;
    sz size;
    sz align;
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

// TODO Builtins
// TODO Type metrics
Type intTypeVal = { "int", nullptr, nullptr, 4, 4, Type::Int };
Type boolTypeVal = { "bool", nullptr, nullptr, 1, 1, Type::Bool };
Type floatTypeVal = { "float", nullptr, nullptr, 4, 4, Type::Float };

Type* intType = &intTypeVal;
Type* boolType = &boolTypeVal;
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


sz TypeSize( Type* type )
{
    ASSERT( type->kind > Type::Completing );
    ASSERT( type->size );
    return type->size;
}


struct ResolvedExpr
{
    Type* type;
    Value value;
    bool isLvalue;
    bool isConst;
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

ResolvedExpr NewResolvedConst( Type* type, u64 intValue )
{
    ResolvedExpr result = {};
    result.type = type;
    result.isConst = true;
    result.value.intValue = intValue;
    return result;
}


Symbol* ResolveName( char const* name, SourcePos const& pos );
ResolvedExpr ResolveExpr( Expr* expr, Type* expectedType = nullptr );

ResolvedExpr ResolveConstExpr( Expr* expr, Type* expectedType = nullptr )
{
    ResolvedExpr result = ResolveExpr( expr );
    if( !result.isConst )
    {
        RSLV_ERROR( expr->pos, "Expected constant expression" );
        result = resolvedNull;
    }
    if( expectedType && result.type != expectedType )
    {
        RSLV_ERROR( expr->pos, "Expected constant expression of type '%s'", expectedType );
        result = resolvedNull;
    }

    return result;
}

u64 ResolveConstExprInt( Expr* expr )
{
    ResolvedExpr result = ResolveConstExpr( expr, intType );
    return result.value.intValue;
}

ResolvedExpr ResolveNameExpr( Expr* expr )
{
    ASSERT( expr->kind == Expr::Name );

    Symbol* sym = ResolveName( expr->name, expr->pos );

    if( sym->kind == Symbol::Var )
        return NewResolvedLvalue( sym->type );
    else if( sym->kind == Symbol::Const )
        // TODO 
        return NewResolvedConst( intType, sym->value.intValue );
    else if( sym->kind == Symbol::Func )
        return NewResolvedRvalue( sym->type );
    else
    {
        RSLV_ERROR( expr->pos, "Named expression can only refer to variable or constant" );
        return resolvedNull;
    }
}

void CompleteType( Type* type, SourcePos const& pos );

ResolvedExpr ResolveFieldExpr( Expr* expr )
{
    ASSERT( expr->kind == Expr::Field );

    // TODO Modules
    ResolvedExpr left = ResolveExpr( expr->field.base );
    Type* type = left.type;
    CompleteType( type, expr->pos );

    if( type && type->kind == Type::Pointer )
    {
        left = NewResolvedLvalue( type->ptr.base );
        type = left.type;
        CompleteType( type, expr->pos );
    }
    if( !type || !(type->kind == Type::Struct || type->kind == Type::Union) )
    {
        RSLV_ERROR( expr->pos, "Can only reference fields on aggregates or pointers to aggregates" );
        return resolvedNull;
    }

    for( TypeField const& f : type->aggregate.fields )
    {
        if( f.name == expr->field.name )
        {
            ResolvedExpr resolvedField = left.isLvalue
                ? NewResolvedLvalue( f.type )
                : NewResolvedRvalue( f.type );
            return resolvedField;
        }
    }

    RSLV_ERROR( expr->pos, "No field named '%s' in type '%s'", expr->field.name, type->name );
    return resolvedNull;
}

Type* NewPtrType( Type* base );

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
                return NewResolvedConst( intType, left.value.intValue + right.value.intValue );
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

ResolvedExpr ResolveCompoundExpr( Expr* expr, Type* expectedType )
{
    ASSERT( expr->kind == Expr::Compound );
    if( !expectedType )
    {
        RSLV_ERROR( expr->pos, "Compound literal requires explicit type" );
        return resolvedNull;
    }

    CompleteType( expectedType, expr->pos );

    sz slotCount = 0;
    // TODO Unions?
    if( expectedType->kind == Type::Array )
        slotCount = expectedType->array.count;
    else if( expectedType->kind == Type::Struct )
        slotCount = Sz( expectedType->aggregate.fields.count );
    else
    {
        RSLV_ERROR( expr->pos, "Compound literals can only be used to initialize struct or array types" );
        return resolvedNull;
    }

    if( Sz( expr->compoundFields.count ) > slotCount )
    {
        RSLV_ERROR( expr->pos, "Too many fields in compound literal" );
        return resolvedNull;
    }

    Type* expectedFieldType = nullptr;
    if( expectedType->kind == Type::Array )
        expectedFieldType = expectedType->array.base;

    int i = 0;
    for( CompoundField const& f : expr->compoundFields )
    {
        if( expectedType->kind == Type::Struct )
            expectedFieldType = expectedType->aggregate.fields[i++].type;

        ResolvedExpr field = ResolveExpr( f.initValue, expectedFieldType );
        if( field.type != expectedFieldType )
        {
            RSLV_ERROR( expr->pos, "Type mismatch in compound literal field (wanted '%s', got '%s')",
                        expectedFieldType->name, field.type->name );
        }
    }

    return NewResolvedRvalue( expectedType );
}

ResolvedExpr ResolveCallExpr( Expr* expr )
{
    ASSERT( expr->kind == Expr::Call );
    ResolvedExpr result = ResolveExpr( expr->call.func );
    CompleteType( result.type, expr->pos );

    if( result.type->kind != Type::Func )
    {
        RSLV_ERROR( expr->pos, "Cannot call non-function value" );
        return resolvedNull;
    }
    if( expr->call.args.count != result.type->func.args.count )
    {
        RSLV_ERROR( expr->pos, "Missing arguments in function call" );
        return resolvedNull;
    }

    int i = 0;
    Array<Type*> const& argTypes = result.type->func.args;
    for( Expr* arg : expr->call.args )
    {
        Type* argType = argTypes[i++];
        ResolvedExpr resolvedArg = ResolveExpr( arg, argType );
        if( resolvedArg.type != argType )
        {
            RSLV_ERROR( arg->pos, "Type mismatch in function call (wanted '%s', got '%s')",
                        argType->name, resolvedArg.type->name );
            return resolvedNull;
        }
    }

    return NewResolvedRvalue( result.type->func.returnType );
}

ResolvedExpr ResolveTernaryExpr( Expr* expr, Type* expectedType )
{
    ASSERT( expr->kind == Expr::Ternary );

    ResolvedExpr cond = ResolveExpr( expr->ternary.cond );
    // TODO Is this equivalent to C semantics for all cases?
    if( false ) //cond.type != boolType )
    {
        RSLV_ERROR( expr->ternary.cond->pos, "Conditional expression must have type bool" );
        return resolvedNull;
    }

    ResolvedExpr thenExpr = ResolveExpr( expr->ternary.thenExpr, expectedType );
    ResolvedExpr elseExpr = ResolveExpr( expr->ternary.elseExpr, expectedType );
    if( thenExpr.type != elseExpr.type )
    {
        RSLV_ERROR( expr->ternary.elseExpr->pos, "Ternary expression branches must have matching types" );
        return resolvedNull;
    }

    if( cond.isConst && thenExpr.isConst && elseExpr.isConst )
        return cond.value.boolValue ? thenExpr : elseExpr;
    else
        return NewResolvedRvalue( thenExpr.type );
}

ResolvedExpr ResolveIndexExpr( Expr* expr )
{
    ASSERT( expr->kind == Expr::Index );

    ResolvedExpr base = ResolveExpr( expr->index.base );
    ResolvedExpr index = ResolveExpr( expr->index.index );
    if( index.type->kind != Type::Int )
        RSLV_ERROR( expr->pos, "Index expression must have integer type" );

    if( base.type->kind == Type::Pointer )
        return NewResolvedLvalue( base.type->ptr.base );
    else if( base.type->kind == Type::Array )
        return NewResolvedLvalue( base.type->array.base );
    else
    {
        RSLV_ERROR( expr->pos, "Can only index arrays or pointers" );
        return resolvedNull;
    }
}

ResolvedExpr ResolveExpr( Expr* expr, Type* expectedType /*= nullptr*/ )
{
    switch( expr->kind )
    {
        case Expr::Int:
        {
            return NewResolvedConst( intType, expr->literal.intValue );
        } break;
        case Expr::Name:
        {
            return ResolveNameExpr( expr );
        } break;
        case Expr::Field:
        {
            return ResolveFieldExpr( expr );
        } break;
        case Expr::Index:
        {
            return ResolveIndexExpr( expr );
        } break;
        case Expr::Compound:
        {
            return ResolveCompoundExpr( expr, expectedType );
        } break;
        case Expr::Call:
        {
            return ResolveCallExpr( expr );
        } break;

        case Expr::Unary:
        {
            return ResolveUnaryExpr( expr );
        } break;
        case Expr::Binary:
        {
            return ResolveBinaryExpr( expr );
        } break;
        case Expr::Ternary:
        {
            return ResolveTernaryExpr( expr, expectedType );
        } break;
        case Expr::Sizeof:
        {
            ResolvedExpr result = ResolveExpr( expr->sizeofExpr );
            Type* type = result.type;
            CompleteType( type, expr->pos );

            // TODO Unsigned 64 bit?
            return NewResolvedConst( intType, TypeSize( type ) );
        } break;

        default:
        {
            NOT_IMPLEMENTED;
            return resolvedNull;
        } break;
    }
}

Type* NewType( Type::Kind kind, sz size )
{
    Type* result = PUSH_STRUCT( &globalArena, Type );
    result->kind = kind;
    result->size = size;
    
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

    Type* result = NewType( Type::Pointer, globalPlatform.PointerSize );
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

    // TODO Dynamic/any size arrays
    Type* result = NewType( Type::Array, count * TypeSize( base ) );
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

    Type* result = NewType( Type::Func, globalPlatform.PointerSize );
    // NOTE Assume passed args are allocated in permanent memory
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
    Type* result = NewType( Type::Incomplete, 0 );
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
        ASSERT( f.type->kind > Type::Completing );
        // TODO Alignment, field offset, etc.
        type->size += TypeSize( f.type );
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
        ASSERT( f.type->kind > Type::Completing );
        // TODO Alignment, field offset, etc.
        type->size = Max( type->size, TypeSize( f.type ) );
    }
}

Array<Symbol*> CreateDeclSymbols( Decl* decl );

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

            char const* name = spec->names.Last();
            Symbol* sym = ResolveName( name, pos );
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
            // TODO Dynamic/unknown array size
            u64 count = ResolveConstExprInt( spec->array.count );
            // TODO Signed/unsigned
            // TODO Check positive
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

    return result;
}

int CountAggregateItems( Array<Decl*> const& items )
{
    int result = 0;
    for( Decl* d : items )
    {
        for( char const* n : d->names )
            result++;
    }

    return result;
}

void CompleteType( Type* type, SourcePos const& pos )
{
    ASSERT( type->kind );

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
    ASSERT( type->symbol && type->symbol->decl );

    // TODO Enums?
    Decl* decl = type->symbol->decl;
    ASSERT( decl->kind == Decl::Struct || decl->kind == Decl::Union );

    Array<TypeField> fields( &globalArena, CountAggregateItems( decl->aggregate.items ) );
    for( Decl* d : decl->aggregate.items )
    {
        // Only care about variable & const fields
        if( d->kind == Decl::Var )
        {
            Type* fieldType = ResolveTypeSpec( d->var.type );
            CompleteType( fieldType, d->pos );

            // If there are several names, create a separate field for each one
            for( char const* name : d->names )
            {
                // TODO Offset
                fields.Push( { name, fieldType } );
            }
        }
    }

    if( decl->kind == Decl::Struct )
        CompleteStructType( type, fields );
    else
        CompleteUnionType( type, fields );

    globalOrderedSymbols.Push( type->symbol );
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

void ResolveSymbol( Symbol* sym )
{
    if( sym->state == Symbol::Resolved )
        // Nothing to do
        return;

    Decl* decl = sym->decl;
    SourcePos const& pos = decl->pos;
    if( sym->state == Symbol::Resolving )
    {
        // TODO Log dependency graph nodes to aid debugging?
        RSLV_ERROR( pos, "Circular dependency detected" );
        return;
    }

    ASSERT( sym->state == Symbol::Unresolved );
    sym->state = Symbol::Resolving;

    switch( sym->kind )
    {
        case Symbol::Var:
        {
            ASSERT( decl->kind == Decl::Var );
            
            Type* type = nullptr;
            if( decl->var.type )
                type = ResolveTypeSpec( decl->var.type );

            // TODO Multiple names/inits?
            // (Should maybe separate any comma expressions when separating the names above)
            //auto& names = decl->var.names;
            if( decl->var.initExpr )
            {
                ResolvedExpr init = ResolveExpr( decl->var.initExpr, type );
                if( type && init.type != type )
                    RSLV_ERROR( pos, "Declared type for '%s' does not match type of initializer expression", sym->name );

                type = init.type;
            }

            CompleteType( type, decl->pos );
            sym->type = type;
        } break;

        case Symbol::Const:
        {
            ASSERT( decl->var.isConst );

            // NOTE Type is always inferred for constants right now
            ResolvedExpr init = ResolveExpr( decl->var.initExpr );
            if( !init.isConst )
                RSLV_ERROR( pos, "Initializer for constant '%s' is not a constant expression", sym->name );

            sym->value = init.value;
            sym->type = init.type;
        } break;

        case Symbol::Type:
        {
            // TODO This would only be for typedefs now, as aggregates start out already resolved
        } break;

        case Symbol::Func:
        {
            Array<Type*> args( &globalArena, decl->func.args.count );
            for( FuncArg const& a : decl->func.args )
            {
                Type* argType = ResolveTypeSpec( a.type );
                args.Push( argType );
            }

            Type* retType = ResolveTypeSpec( decl->func.returnType );
            sym->type = NewFuncType( args, retType );
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
        CompleteType( sym->type, sym->decl->pos );
}

Symbol* ResolveName( char const* name, SourcePos const& pos )
{
    Symbol* sym = GetSymbol( name );
    if( sym )
        ResolveSymbol( sym );
    else
        RSLV_ERROR( pos, "Undefined symbol '%s'", name );

    return sym;
}

void ResolveSymbols()
{
    auto idx = globalSymbolList.First();
    while( idx )
    {
        ResolveSymbol( &*idx );
        idx.Next();
    }
}

Array<Symbol*> CreateDeclSymbols( Decl* decl )
{
    ASSERT( decl->names );

    Symbol::Kind kind = Symbol::None;
    switch( decl->kind )
    {
        case Decl::Var:
            kind = decl->var.isConst ? Symbol::Const : Symbol::Var;
            break;
        case Decl::Func:
            kind = Symbol::Func;
            break;
        // TODO Don't we need to add all syb-types and contained symbols for these?
        case Decl::Struct:
        case Decl::Union:
        case Decl::Enum:
            kind = Symbol::Type;
            break;

        INVALID_DEFAULT_CASE;
    }

    // TODO Temp?
    Array<Symbol*> result( &globalTmpArena, decl->names.count );

    for( char const* name : decl->names )
    {
        if( GetSymbol( name ) != nullptr )
        {
            RSLV_ERROR( decl->pos, "Symbol '%s' already declared", name );
            continue;
        }

        Symbol* sym = globalSymbolList.PushEmpty( true );
        sym->decl = decl;
        sym->name = name;
        sym->kind = kind;
        sym->state = Symbol::Unresolved;

        if( decl->kind == Decl::Struct || decl->kind == Decl::Union )
        {
            sym->state = Symbol::Resolved;
            sym->type = NewIncompleteType( sym );
        }

        result.Push( sym );
    }

    return result;
}

Symbol* CreateTypeSymbol( char const* name, Type* type )
{
    InternString* internName = Intern( String( name ) );

    Symbol result = {};
    result.name = internName->data;
    result.type = type;
    result.kind = Symbol::Type;
    result.state = Symbol::Resolved;

    return globalSymbolList.Push( result );
}


void InitResolver()
{
    INIT( globalSymbolList ) BucketArray<Symbol>( &globalArena, 256 );
    INIT( globalOrderedSymbols ) BucketArray<Symbol*>( &globalArena, 256 );
    INIT( globalCachedTypes ) BucketArray<Type*>( &globalArena, 256 );

    CreateTypeSymbol( "int", intType );
    CreateTypeSymbol( "float", floatType );
}
