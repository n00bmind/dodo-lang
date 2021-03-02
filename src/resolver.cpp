// TODO Inline any single-use functions!
// TODO Inline any single-use functions!
// TODO Inline any single-use functions!
// TODO Inline any single-use functions!

struct Type;

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
        Tainted,            // Failed to resolve. Stop emitting errors about it
    };

    char const* name;
    Decl* decl;
    ::Type* type;
    // TODO Store constants separately?
    ConstValue constValue = {};
    u32 kind : 16;
    u32 state : 15;
    // TODO Namespaces?
    u32 isLocal : 1;
};


struct FuncArg
{
    Type* type;
    bool isVararg;
};

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
        Bool,       // TODO Is this needed at all?
        Bits,
        Int,
        Enum,       // NOTE This is just the scalar enum (non-struct). Can be int or bits!
        Float,
        Pointer,
        Func,
        Array,
        Buffer,     // a.k.a. array view
        String,
        Struct,
        Union,
        // TODO 
        SBuffer,
        // TODO 
        TypeVar,
        Any,
    };

    char const* name;
    // NOTE Only user types (aggregates and enums) will have one
    // (only user created types are biunivocally associated with a symbol)
    Symbol* symbol;
    sz size;
    sz align;
    Kind kind;

    union
    {
        struct
        {
            // TODO Varargs
            ::Array<FuncArg> args;
            Type* returnType;
        } func;
        struct
        {
            ::Array<TypeField> fields;
        } aggregate;
        struct
        {
            // TODO 
            Type* base;
        } enum_;
        struct
        {
            Type* base;
            // 0 = unknown (needs initializer)
            // Unused for buffers, as their size is dynamic
            sz count;
        } array;
        struct
        {
            Type* base;
        } ptr;
    };

    Type()
    {}
};


internal Hashtable<char const*, Symbol, LazyAllocator> globalSymbols;
internal BucketArray<Symbol*> globalSymbolsList;
// Ordered global symbols for linearized codegen
internal BucketArray<Symbol*> globalOrderedSymbols;

// Local lexical scopes for functions
internal BucketArray<Symbol> globalScopeStack;
internal BucketArray<Symbol>::Idx<false> globalCurrentScopeStart;

BucketArray<Stmt*> globalNodeStack;

// List of unique interned types (a.k.a hash consing)
// TODO Hashtable!
internal BucketArray<Type*> globalCachedTypes;

// Builtins
struct BuiltinType
{
    char const* symbolName;
    bool isAlias;

    union
    {
        Type type = {};
        Type* aliasType;
    };

    BuiltinType()
    {}
};

internal BuiltinType globalBuiltinTypes[64];
internal int globalNextBuiltinIndex = 0;

Type* NewBuiltinType( char const* name, Type::Kind kind, sz size, sz alignment )
{
    ASSERT( globalNextBuiltinIndex < ARRAYCOUNT( globalBuiltinTypes ) );
    BuiltinType* result = globalBuiltinTypes + globalNextBuiltinIndex++;
    result->symbolName = name;
    result->type.name = name;
    result->type.kind = kind;
    result->type.size = size;
    result->type.align = alignment;
    result->isAlias = false;
    
    return &result->type;
}

Type* NewBuiltinType( char const* name, Type* aliasType )
{
    BuiltinType const* alias = nullptr;
    for( BuiltinType const& b : globalBuiltinTypes )
    {
        if( &b.type == aliasType )
        {
            alias = &b;
            break;
        }
    }
    
    ASSERT( alias );
    ASSERT( globalNextBuiltinIndex < ARRAYCOUNT( globalBuiltinTypes ) );
    BuiltinType* result = globalBuiltinTypes + globalNextBuiltinIndex++;
    result->symbolName = name;
    result->aliasType = aliasType;
    result->isAlias = true;

    return aliasType;
}

Type* voidType = NewBuiltinType( "void", Type::Void, 0, 0 );
Type* boolType = NewBuiltinType( "bool", Type::Bool, 1, 1 );
Type* i8Type = NewBuiltinType( "i8", Type::Int, 1, 1 );
Type* i16Type = NewBuiltinType( "i16", Type::Int, 2, 2 );
Type* i32Type = NewBuiltinType( "i32", Type::Int, 4, 4 );
Type* i64Type = NewBuiltinType( "i64", Type::Int, 8, 8 );
Type* intType = NewBuiltinType( "int", i64Type );
Type* b8Type = NewBuiltinType( "b8", Type::Bits, 1, 1 );
Type* b16Type = NewBuiltinType( "b16", Type::Bits, 2, 2 );
Type* b32Type = NewBuiltinType( "b32", Type::Bits, 4, 4 );
Type* b64Type = NewBuiltinType( "b64", Type::Bits, 8, 8 );
Type* bitsType = NewBuiltinType( "bits", b64Type );
Type* f32Type = NewBuiltinType( "f32", Type::Float, 4, 4 );
Type* f64Type = NewBuiltinType( "f64", Type::Float, 8, 8 );
Type* floatType = NewBuiltinType( "float", f32Type );
// TODO This stuff needs to be in sync with the structs in the preamble!
Type* stringType = NewBuiltinType( "string", Type::String, globalPlatform.PointerSize, globalPlatform.PointerSize );
// TODO How big is this guy? How does any even work?
Type* anyType = NewBuiltinType( "any", Type::Any, globalPlatform.PointerSize, globalPlatform.PointerSize );

// Error marker
Type* nullType = NewBuiltinType( "", Type::None, 0, 0 );
ResolvedExpr resolvedNull = { nullType, };

bool IsValid( ResolvedExpr const& resolved )
{
    return !resolved.type || resolved.type->kind != Type::None;
}



Type* NewType( char const* name, Type::Kind kind, sz size, sz alignment )
{
    Type* result = PUSH_STRUCT( &globalArena, Type );
    result->name = name;
    result->kind = kind;
    result->size = size;
    result->align = alignment;
    
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

    Type* result = NewType( "pointer", Type::Pointer, globalPlatform.PointerSize, globalPlatform.PointerSize );
    result->ptr.base = base;

    globalCachedTypes.Push( result );

    return result;
}

sz TypeSize( Type* type )
{
    ASSERT( type->kind > Type::Completing );
    ASSERT( type->size );
    return type->size;
}

sz TypeAlignment( Type* type )
{
    ASSERT( type->kind > Type::Completing );
    // FIXME 
    //ASSERT( type->align );
    return type->align;
}

Type* NewArrayType( Type* base, sz count )
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

    // NOTE We allow no-size arrays to pass through, as they'll be checked and substituted during ResolveSymbol
    Type* result = NewType( "array", Type::Array, count * TypeSize( base ), TypeAlignment( base ) );
    result->array.base = base;
    result->array.count = count;

    globalCachedTypes.Push( result );

    return result;
}

Type* NewBufferType( Type* base )
{
    auto idx = globalCachedTypes.First(); 
    while( idx )
    {
        Type* t = *idx;
        if( t->kind == Type::Buffer )
        {
            if( t->array.base == base )
                return t;
        }
        idx.Next();
    }

    Type* result = NewType( "buffer view", Type::Buffer, globalPlatform.PointerSize + 4, globalPlatform.PointerSize );
    result->array.base = base;

    globalCachedTypes.Push( result );

    return result;
}

Type* NewFuncType( Array<FuncArg> const& args, Type* returnType )
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

    Type* result = NewType( "function", Type::Func, globalPlatform.PointerSize, globalPlatform.PointerSize );
    // NOTE Assume passed args are allocated in permanent memory
    result->func.args = args;
    result->func.returnType = returnType;

    globalCachedTypes.Push( result );

    return result;
}

Type* NewIncompleteType( char const* name, Symbol* sym )
{
    Type* result = NewType( name, Type::Incomplete, 0, 0 );
    result->symbol = sym;
    return result;
}


bool IsTainted( Type* type )
{
    return type->symbol && type->symbol->state == Symbol::Tainted;
}


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

ResolvedExpr NewResolvedConst( Type* type, i64 intValue, SourcePos const& pos )
{
    ASSERT( type->kind == Type::Int );

    i64 min = 0, max = 0;
    switch( type->size )
    {
        case 1:
            min = I8MIN;
            max = I8MAX;
            break;
        case 2:
            min = I16MIN;
            max = I16MAX;
            break;
        case 4:
            min = I32MIN;
            max = I32MAX;
            break;
        case 8:
            min = I64MIN;
            max = I64MAX;
            break;
        INVALID_DEFAULT_CASE;
    }

    if( intValue < min )
    {
        RSLV_ERROR( pos, "Integer constant expression overflow (minimum representable value is %lld)", min );
        return resolvedNull;
    }
    if( intValue > max )
    {
        RSLV_ERROR( pos, "Integer constant expression overflow (maximum representable value is %lld)", max );
        return resolvedNull;
    }

    ResolvedExpr result = {};
    result.type = type;
    result.isConst = true;
    result.constValue.intValue = intValue;
    return result;
}

ResolvedExpr NewResolvedConst( Type* type, u64 bitsValue, SourcePos const& pos )
{
    ASSERT( type->kind == Type::Bits );

    u64 max = 0;
    switch( type->size )
    {
        case 1:
            max = U8MAX;
            break;
        case 2:
            max = U16MAX;
            break;
        case 4:
            max = U32MAX;
            break;
        case 8:
            max = U64MAX;
            break;
        INVALID_DEFAULT_CASE;
    }

    if( bitsValue > max )
    {
        RSLV_ERROR( pos, "Integer constant expression overflow (maximum representable value is %lld)", max );
        return resolvedNull;
    }

    ResolvedExpr result = {};
    result.type = type;
    result.isConst = true;
    result.constValue.bitsValue = bitsValue;
    return result;
}

ResolvedExpr NewResolvedConst( Type* type, bool boolValue, SourcePos const& pos )
{
    ASSERT( type->kind == Type::Bool );

    ResolvedExpr result = {};
    result.type = type;
    result.isConst = true;
    result.constValue.bitsValue = boolValue ? 1ull : 0ull;
    return result;
}

ResolvedExpr NewResolvedConst( Type* type, f64 floatValue, SourcePos const& pos )
{
    ASSERT( type->kind == Type::Float );

    f64 min = 0, max = 0;
    switch( type->size )
    {
        case 4:
            min = -F32MAX;
            max = F32MAX;
            break;
        case 8:
            min = -F64MAX;
            max = F64MAX;
            break;
        INVALID_DEFAULT_CASE;
    }

    if( floatValue < min )
    {
        RSLV_ERROR( pos, "Floating point constant expression overflow (minimum representable value is %.*e)", DECIMAL_DIG, min );
        return resolvedNull;
    }
    if( floatValue > max )
    {
        RSLV_ERROR( pos, "Floating point constant expression overflow (maximum representable value is %.*e)", DECIMAL_DIG, max );
        return resolvedNull;
    }

    ResolvedExpr result = {};
    result.type = type;
    result.isConst = true;
    result.constValue.floatValue = floatValue;
    return result;
}

ResolvedExpr NewResolvedConst( Type* type, String const& strValue, SourcePos const& pos )
{
    ASSERT( type->kind == Type::String );

    ResolvedExpr result = {};
    result.type = type;
    result.isConst = true;
    result.constValue.strValue = strValue;
    return result;
}

bool IsPointerType( Type* type )
{
    return type->kind == Type::Pointer || type->kind == Type::Func;
}

bool IsIntegralType( Type* type )
{
    return type->kind >= Type::Bool && type->kind <= Type::Enum;
}

bool IsIntegerType( Type* type )
{
    return type->kind == Type::Int || (type->kind == Type::Enum && type->enum_.base->kind == Type::Int);
}

bool IsArithmeticType( Type* type )
{
    return type->kind >= Type::Int && type->kind <= Type::Float;
}

bool IsScalarType( Type* type )
{
    return type->kind >= Type::Bool && type->kind <= Type::Func;
}

bool IsNullPtrConst( ResolvedExpr const& resolved )
{
    Type* type = resolved.type;
    if( resolved.isConst && (type->kind == Type::Pointer || IsIntegralType( type )) )
        return resolved.constValue.bitsValue == 0;
    else
        return false;
}

bool IsIntegerConst( ResolvedExpr const& resolved )
{
    return resolved.isConst && IsIntegerType( resolved.type );
}

bool IsCharacterConst( ResolvedExpr const& resolved )
{
    Type* type = resolved.type;
    // TODO Unicode
    return resolved.isConst && type->kind == Type::String && resolved.constValue.strValue.length == 1;
}

bool IsConvertible( ResolvedExpr const& resolved, Type* dest )
{
    Type* src = resolved.type;

    if( dest == src )
        return true;
    else if( dest == voidType || dest == anyType )
        return true;
    else if( IsIntegralType( dest ) && IsIntegerConst( resolved ) )          // TODO Have a 'literal' flag and restrict this to literals?
        return true;
    else if( dest->kind == Type::Float && IsArithmeticType( src ) )
        return true;
    else if( dest->kind == Type::Bits && IsCharacterConst( resolved ) )
        return true;
    //else if( src->kind == Type::Func && src->func.isIntrinsic )
        //return false;
    else if( IsPointerType( dest ) && IsNullPtrConst( resolved ) )
        return true;
    else if( dest->kind == Type::Pointer && src->kind == Type::Pointer )
    {
        if( dest->ptr.base == src->ptr.base )
            return true;
        else if( dest->ptr.base == voidType || src->ptr.base == voidType )
            return true;
        else
            return false;
    }
    else if( dest->kind == Type::Pointer && src->kind == Type::Buffer )
        return true;
    else if( (dest->kind == Type::Pointer || dest->kind == Type::Buffer) && src->kind == Type::Array )
        return true;
    else
        return false;
}

bool IsCastable( ResolvedExpr const& resolved, Type* dest )
{
    Type* src = resolved.type;
    if( IsConvertible( resolved, dest ) )
        return true;
    else if( (IsIntegralType( dest ) || IsPointerType( dest )) && (IsIntegralType( src ) || IsPointerType( src )) )
        return true;
    else
        return false;
}

#define CASE(srcKind, srcSlot) \
    case srcKind: \
        switch (dest->kind) { \
        case Type::Bool: \
            resolved->constValue.bitsValue = (bool)resolved->constValue.srcSlot; \
            break; \
        case Type::Bits: \
            resolved->constValue.bitsValue = (u64)resolved->constValue.srcSlot; \
            break; \
        case Type::Int: \
            resolved->constValue.intValue = (i64)resolved->constValue.srcSlot; \
            break; \
        case Type::Float: \
            resolved->constValue.floatValue = (f64)resolved->constValue.srcSlot; \
            break; \
        case Type::Pointer: \
            resolved->constValue.ptrValue = (uintptr_t)resolved->constValue.srcSlot; \
            break; \
        default: \
            resolved->isConst = false; \
            break; \
        } \
        break;

bool CastType( ResolvedExpr* resolved, Type* type )
{
    Type* dest = type;
    if( resolved->type != dest )
    {
        if( !IsCastable( *resolved, dest ) )
            return false;

        if( resolved->isConst )
        {
            if( dest->kind == Type::Enum )
                dest = dest->enum_.base;

            Type* srcType = resolved->type;
            if( srcType->kind == Type::Enum )
                srcType = srcType->enum_.base;

            // TODO Cast to/from string?
            switch( srcType->kind )
            {
                CASE(Type::Bool, bitsValue)
                CASE(Type::Bits, bitsValue)
                CASE(Type::Int, intValue)
                CASE(Type::Float, floatValue)
                CASE(Type::Pointer, ptrValue)

                default:
                    resolved->isConst = false;
                    break;
            }
        }
    }

    resolved->type = type;
    return true;
}

bool ConvertType( ResolvedExpr* resolved, Type* type )
{
    if( IsConvertible( *resolved, type ) )
    {
        CastType( resolved, type );
        resolved->isLvalue = false;
        return true;
    }

    return false;
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
    if( expectedType && !ConvertType( &result, expectedType ) )
    {
        RSLV_ERROR( expr->pos, "Expected constant expression of type '%s'", expectedType );
        result = resolvedNull;
    }

    return result;
}

i64 ResolveConstExprInt( Expr* expr )
{
    ResolvedExpr result = ResolveConstExpr( expr, intType );
    return result.constValue.intValue;
}

ResolvedExpr ResolveNameExpr( Expr* expr )
{
    ASSERT( expr->kind == Expr::Name );

    Symbol* sym = ResolveName( expr->name.ident, expr->pos );
    if( !sym )
        return resolvedNull;

    expr->name.symbol = sym;

    if( sym->kind == Symbol::Var )
        return NewResolvedLvalue( sym->type );
    else if( sym->kind == Symbol::Const )
        return NewResolvedConst( intType, sym->constValue.intValue, expr->pos );
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
    
    if( !type || IsTainted( type ) )
        return resolvedNull;

    CompleteType( type, expr->pos );

    if( MatchKeyword( Keyword::Length, expr->field.name ) )
    {
        if( !(type->kind == Type::Array || type->kind == Type::Buffer || type->kind == Type::String) )
        {
            RSLV_ERROR( expr->pos, "Only arrays, buffer views and strings have a 'length' attribute" );
            return resolvedNull;
        }

        ResolvedExpr result = NewResolvedRvalue( intType );
        if( type->kind == Type::Array )
        {
            result.isConst = true;
            result.constValue.intValue = type->array.count;
        }

        return result;
    }

    // 'Normal' field access of aggregates or pointers to such
    if( type->kind == Type::Pointer )
    {
        left = NewResolvedLvalue( type->ptr.base );
        type = left.type;
        CompleteType( type, expr->pos );
    }
    if( !(type->kind == Type::Struct || type->kind == Type::Union) )
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

ResolvedExpr EvalConstUnaryExpr( TokenKind::Enum op, ResolvedExpr const& operand, SourcePos const& pos )
{
    Type* type = operand.type;

    if( type->kind == Type::Int )
    {
        i64 value = operand.constValue.intValue;
        switch( op )
        {
            case TokenKind::Plus:
                value = +value;
                break;
            case TokenKind::Minus:
                value = -value;
                break;
            case TokenKind::Tilde:
                value = ~value;
                break;
            case TokenKind::Exclamation:
                value = !value;
                break;

            INVALID_DEFAULT_CASE
        }
        return NewResolvedConst( type, value, pos );
    }
    else if( type->kind == Type::Float )
    {
        // TODO Not seen any real examples of how to do this properly
        // I assume the proper way is to do all calculations in a much higher precission (80 bits or what have you)
        f64 value = operand.constValue.floatValue;
        switch( op )
        {
            case TokenKind::Plus:
                value = +value;
                break;
            case TokenKind::Minus:
                value = -value;
                break;

            INVALID_DEFAULT_CASE
        }
        return NewResolvedConst( type, value, pos );
    }
    else if( type->kind == Type::Bits )
    {
        u64 value = operand.constValue.bitsValue;
        switch( op )
        {
            case TokenKind::Tilde:
                value = ~value;
                break;
            case TokenKind::Exclamation:
                value = !value;
                break;

            INVALID_DEFAULT_CASE
        }
        return NewResolvedConst( type, value, pos );
    }
    else if( type->kind == Type::Bool )
    {
        bool value = operand.constValue.BoolValue();
        switch( op )
        {
            case TokenKind::Exclamation:
                value = !value;
                break;

            INVALID_DEFAULT_CASE
        }
        return NewResolvedConst( type, value, pos );
    }
    else
    {
        INVALID_CODE_PATH;
    }

    return resolvedNull;
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

        // NOTE These all continue below the switch!!
        case TokenKind::Plus:
        case TokenKind::Minus:
            if( !IsArithmeticType( type ) )
            {
                RSLV_ERROR( expr->pos, "Can only use unary '%s' with arithmetic types",
                            TokenKind::Items::names[ expr->unary.op ] );
                return resolvedNull;
            }
            break;
        case TokenKind::Tilde:
            if( !IsIntegralType( type ) )
            {
                RSLV_ERROR( expr->pos, "Can only use unary '%s' with integral types",
                            TokenKind::Items::names[ expr->unary.op ] );
                return resolvedNull;
            }
            break;
        case TokenKind::Exclamation:
            if( !IsScalarType( type ) )
            {
                RSLV_ERROR( expr->pos, "Can only use unary '%s' with scalar types",
                            TokenKind::Items::names[ expr->unary.op ] );
                return resolvedNull;
            }
            break;

        INVALID_DEFAULT_CASE;
    }

    if( operand.isConst )
        return EvalConstUnaryExpr( expr->unary.op, operand, expr->pos );
    else
        return NewResolvedRvalue( type );

}

i64 EvalIntBinaryExpr( TokenKind::Enum op, i64 leftValue, i64 rightValue, SourcePos const& pos )
{
    switch( op )
    {
        case TokenKind::Asterisk:
            return leftValue * rightValue;
        case TokenKind::Slash:
        {
            if( rightValue == 0 )
            {
                RSLV_ERROR( pos, "Divide by zero in constant expression" );
                return 0;
            }
            else
                return leftValue / rightValue;
        }
        case TokenKind::Percent:
        {
            if( rightValue == 0 )
            {
                RSLV_ERROR( pos, "Divide by zero in constant expression" );
                return 0;
            }
            else
                return leftValue % rightValue;
        }
        // TODO Detect over/underflow
        case TokenKind::LeftShift:
            return leftValue << rightValue;
        case TokenKind::RightShift:
            return leftValue >> rightValue;
        case TokenKind::Ampersand:
            return leftValue & rightValue;

        case TokenKind::Plus:
            return leftValue + rightValue;
        case TokenKind::Minus:
            return leftValue - rightValue;
        case TokenKind::Pipe:
            return leftValue | rightValue;
        case TokenKind::Caret:
            return leftValue ^ rightValue;

        case TokenKind::Equal:
            return leftValue == rightValue;
        case TokenKind::NotEqual:
            return leftValue != rightValue;
        case TokenKind::LessThan:
            return leftValue < rightValue;
        case TokenKind::GreaterThan:
            return leftValue > rightValue;
        case TokenKind::LTEqual:
            return leftValue <= rightValue;
        case TokenKind::GTEqual:
            return leftValue >= rightValue;

        INVALID_DEFAULT_CASE
    }

    return 0;
}

u64 EvalBitsBinaryExpr( TokenKind::Enum op, u64 leftValue, u64 rightValue, SourcePos const& pos )
{
    switch( op )
    {
        // TODO Detect over/underflow
        case TokenKind::LeftShift:
            return leftValue << rightValue;
        case TokenKind::RightShift:
            return leftValue >> rightValue;
        case TokenKind::Ampersand:
            return leftValue & rightValue;
        case TokenKind::Pipe:
            return leftValue | rightValue;
        case TokenKind::Caret:
            return leftValue ^ rightValue;

        case TokenKind::Equal:
            return leftValue == rightValue;
        case TokenKind::NotEqual:
            return leftValue != rightValue;
        case TokenKind::LessThan:
            return leftValue < rightValue;
        case TokenKind::GreaterThan:
            return leftValue > rightValue;
        case TokenKind::LTEqual:
            return leftValue <= rightValue;
        case TokenKind::GTEqual:
            return leftValue >= rightValue;

        INVALID_DEFAULT_CASE
    }

    return 0;
}

// FIXME 
f64 EvalFloatBinaryExpr( TokenKind::Enum op, f64 leftValue, f64 rightValue, SourcePos const& pos )
{
    switch( op )
    {
        case TokenKind::Asterisk:
            return leftValue * rightValue;
        case TokenKind::Slash:
        {
            if( rightValue == 0 )
            {
                RSLV_ERROR( pos, "Divide by zero in constant expression" );
                return 0;
            }
            else
                return leftValue / rightValue;
        }
        case TokenKind::Plus:
            return leftValue + rightValue;
        case TokenKind::Minus:
            return leftValue - rightValue;

        case TokenKind::Equal:
            return leftValue == rightValue;
        case TokenKind::NotEqual:
            return leftValue != rightValue;
        case TokenKind::LessThan:
            return leftValue < rightValue;
        case TokenKind::GreaterThan:
            return leftValue > rightValue;
        case TokenKind::LTEqual:
            return leftValue <= rightValue;
        case TokenKind::GTEqual:
            return leftValue >= rightValue;

        INVALID_DEFAULT_CASE
    }

    return 0;
}

void UnifyArithmeticOperands( ResolvedExpr* left, ResolvedExpr* right )
{
    Type* leftType = left->type;
    Type* rightType = right->type;

    if( leftType == f64Type )
        CastType( right, f64Type );
    else if( rightType == f64Type )
        CastType( left, f64Type );
    else if( leftType  == f32Type )
        CastType( right, f32Type );
    else if( rightType == f32Type )
        CastType( left, f32Type );
    else
    {
        // NOTE Promote scalar enums to their base types
        if( leftType->kind == Type::Enum )
            left->type = left->type->enum_.base;
        if( rightType->kind == Type::Enum )
            right->type = right->type->enum_.base;

        if( leftType->kind == Type::Bits && rightType->kind == Type::Int )
            CastType( right, leftType );
        else if( leftType->kind == Type::Int && rightType->kind == Type::Bits )
            CastType( left, rightType );
        else
        {
            ASSERT( (leftType->kind == Type::Int && rightType->kind == Type::Int ) ||
                    (leftType->kind == Type::Bits && rightType->kind == Type::Bits) );

            if( leftType->size > rightType->size )
                CastType( right, leftType );
            else
                CastType( left, rightType );
        }
    }

    ASSERT( left->type == right->type );
}

ResolvedExpr EvalConstBinaryExpr( TokenKind::Enum op, ResolvedExpr* left, ResolvedExpr* right, SourcePos const& pos )
{
    Type* type = left->type;
    ASSERT( type == right->type );

    if( type->kind == Type::Int )
    {
        i64 result = EvalIntBinaryExpr( op, left->constValue.intValue, right->constValue.intValue, pos );
        return NewResolvedConst( type, result, pos );
    }
    else if( type->kind == Type::Bits )
    {
        u64 result = EvalBitsBinaryExpr( op, left->constValue.bitsValue, right->constValue.bitsValue, pos );
        return NewResolvedConst( type, result, pos );
    }
    else if( type->kind == Type::Float )
    {
        f64 result = EvalFloatBinaryExpr( op, left->constValue.floatValue, right->constValue.floatValue, pos );
        return NewResolvedConst( type, result, pos );
    }
    else
        INVALID_CODE_PATH;

    return resolvedNull;
}

ResolvedExpr ResolveBinaryArithmeticExpr( TokenKind::Enum op, ResolvedExpr* left, ResolvedExpr* right, SourcePos const& pos )
{
    UnifyArithmeticOperands( left, right );

    ResolvedExpr result = {};
    if( left->isConst && right->isConst )
        result =  EvalConstBinaryExpr( op, left, right, pos );
    else
        result =  NewResolvedRvalue( left->type );

    if( TokenKind::Items::entries[ op ].value.flags == TokenFlags::CmpOp )
        CastType( &result, boolType );

    return result;
}

ResolvedExpr ResolveBinaryExpr( TokenKind::Enum op, char const* opName, ResolvedExpr* left, ResolvedExpr* right, SourcePos const& pos )
{
    switch( op )
    {
        case TokenKind::Asterisk:
        case TokenKind::Slash:
        {
            if( !IsArithmeticType( left->type ) || !IsArithmeticType( right->type ) )
            {
                RSLV_ERROR( pos, "Operands of '%s' expression must have arithmetic type", opName );
                return resolvedNull;
            }

            return ResolveBinaryArithmeticExpr( op, left, right, pos );
        } break;
        case TokenKind::Percent:
        {
            if( IsIntegerType( left->type ) && IsIntegerType( right->type ) )
                return ResolveBinaryArithmeticExpr( op, left, right, pos );
            else
            {
                RSLV_ERROR( pos, "Operands of '%s' expression must both have integer type", opName );
                return resolvedNull;
            }
        } break;

        case TokenKind::Plus:
        {
            if( IsArithmeticType( left->type ) && IsArithmeticType( right->type ) )
                return ResolveBinaryArithmeticExpr( op, left, right, pos );
            else
            {
                ResolvedExpr* ptrOperand = nullptr;
                ResolvedExpr* intOperand = nullptr;
                if( IsPointerType( left->type ) && IsIntegerType( right->type ) )
                {
                    ptrOperand = left;
                    intOperand = right;
                }
                else if( IsPointerType( right->type ) && IsIntegerType( left->type ) )
                {
                    ptrOperand = right;
                    intOperand = left;
                }

                if( ptrOperand && intOperand )
                {
                    CompleteType( ptrOperand->type, pos );
                    // TODO Promote void pointers to b8 ??
#if 0
                    if( ptrOperand->type->base == voidType )
                        ptrOperand->type = b8Type;
#endif
                    if( ptrOperand->type->ptr.base->size == 0 )
                    {
                        RSLV_ERROR( pos, "Cannot do pointer arithmetic with size 0 base type" );
                        return resolvedNull;
                    }

                    return NewResolvedRvalue( ptrOperand->type );
                }
                else
                {
                    RSLV_ERROR( pos, "Operands of '+' expression must both have arithmetic type, or pointer and integer type" );
                    return resolvedNull;
                }
            }
        } break;

        case TokenKind::Minus:
        {
            if( IsArithmeticType( left->type ) && IsArithmeticType( right->type ) )
                return ResolveBinaryArithmeticExpr( op, left, right, pos );
            else if( IsPointerType( left->type ) && IsIntegerType( right->type ) )
            {
                // TODO Promote void pointers to b8 ??
#if 0
                if( ptrOperand->type->base == voidType )
                    ptrOperand->type = b8Type;
#endif
                return NewResolvedRvalue( left->type );
            }
            else if( IsPointerType( left->type ) && IsPointerType( right->type ) )
            {
                // TODO Promote left and right from void* if the other is pointer to size 1? (or both are void)
                if( left->type != right->type )
                {
                    RSLV_ERROR( pos, "Cannot subtract pointers to different types" );
                    return resolvedNull;
                }

                return NewResolvedRvalue( i64Type );
            }
            else
            {
                RSLV_ERROR( pos, "Operands of '-' expression must both have arithmetic type, pointer and integer type, or compatible pointer types" );
                return resolvedNull;
            }
        } break;

        case TokenKind::LeftShift:
        case TokenKind::RightShift:
        {
            if( IsIntegralType( left->type ) && IsIntegerType( right->type ) )
            {
                ResolvedExpr result = ResolveBinaryArithmeticExpr( op, left, right, pos );
                ASSERT( result.type->kind == left->type->kind );
                return result;
            }
            else
            {
                RSLV_ERROR( pos, "Operands of '%s' expression must both have integer type, or bits and integer type", opName );
                return resolvedNull;
            }
        } break;
        case TokenKind::Ampersand:
        case TokenKind::Caret:
        case TokenKind::Pipe:
        {
            if( IsIntegralType( left->type ) && IsIntegralType( right->type ) )
            {
                ResolvedExpr result = ResolveBinaryArithmeticExpr( op, left, right, pos );
                ASSERT( result.type->kind == Type::Bits );
                return result;
            }
            else
            {
                RSLV_ERROR( pos, "Operands of '%s' expression must both have integral type", opName );
                return resolvedNull;
            }
        } break;

        case TokenKind::Equal:
        case TokenKind::NotEqual:
        {
            if( IsArithmeticType( left->type ) && IsArithmeticType( right->type ) )
            {
                ResolvedExpr result = ResolveBinaryArithmeticExpr( op, left, right, pos );
                ASSERT( result.type->kind == Type::Bool );
                return result;
            }
            else if( IsPointerType( left->type ) && IsPointerType( right->type ) )
            {
                // TODO Do we care about comparing pointers to different types?
                return NewResolvedRvalue( boolType );
            }
            else if( (IsPointerType( left->type ) && IsNullPtrConst( *right )) || (IsNullPtrConst( *left ) && IsPointerType( right->type )) )
            {
                return NewResolvedRvalue( boolType );
            }
            else
            {
                RSLV_ERROR( pos, "Operands of '%s' expression must both be arithmetic types or compatible pointer types", opName );
                return resolvedNull;
            }
        } break;
        case TokenKind::LessThan:
        case TokenKind::LTEqual:
        case TokenKind::GreaterThan:
        case TokenKind::GTEqual:
        {
            if( IsArithmeticType( left->type ) && IsArithmeticType( right->type ) )
            {
                ResolvedExpr result = ResolveBinaryArithmeticExpr( op, left, right, pos );
                ASSERT( result.type->kind == Type::Bool );
                return result;
            }
            else if( IsPointerType( left->type ) && IsPointerType( right->type ) )
            {
                // TODO Do we care about comparing pointers to different types?
                // NOTE Apparently C follows slightly different rules here compared to the previous case. Check.
                return NewResolvedRvalue( boolType );
            }
            else if( (IsPointerType( left->type ) && IsNullPtrConst( *right )) || (IsNullPtrConst( *left ) && IsPointerType( right->type )) )
            {
                return NewResolvedRvalue( boolType );
            }
            else
            {
                RSLV_ERROR( pos, "Operands of '%s' expression must both be arithmetic types or compatible pointer types", opName );
                return resolvedNull;
            }
        } break;

        case TokenKind::LogicAnd:
        case TokenKind::LogicOr:
        {
            // TODO Shortcircuting !?
            if( IsScalarType( left->type ) && IsScalarType( right->type ) )
            {
                if( left->isConst && right->isConst )
                {
                    CastType( left, boolType );
                    CastType( right, boolType );

                    bool result;
                    if( op == TokenKind::LogicAnd )
                        result = left->constValue.BoolValue() && right->constValue.BoolValue();
                    else 
                        result = left->constValue.BoolValue() || right->constValue.BoolValue();

                    return NewResolvedConst( boolType, result, pos );
                }
                else
                    return NewResolvedRvalue( boolType );
            }
            else
            {
                RSLV_ERROR( pos, "Operands of '%s' expression must both be scalar types", opName );
                return resolvedNull;
            }
        } break;

        INVALID_DEFAULT_CASE;
    }

    return resolvedNull;
}

ResolvedExpr ResolveBinaryExpr( Expr* expr )
{
    ASSERT( expr->kind == Expr::Binary );

    ResolvedExpr left = ResolveExpr( expr->binary.left );
    ResolvedExpr right = ResolveExpr( expr->binary.right );

    TokenKind::Enum op = expr->binary.op;
    char const* opName = TokenKind::Items::names[op];

    return ResolveBinaryExpr( op, opName, &left, &right, expr->pos );
}

ResolvedExpr ResolveCompoundExpr( Expr* expr, Type* expectedType )
{
    ASSERT( expr->kind == Expr::Compound );
    ASSERT( expectedType );

    if( expectedType )
        CompleteType( expectedType, expr->pos );

    Type* type = expectedType;

    sz maxSlotCount = 0;
    if( type->kind == Type::Union )
        maxSlotCount = 1;
    else if( type->kind == Type::Struct )
        maxSlotCount = type->aggregate.fields.count;
    else if( type->kind == Type::Array )
    {
        if( type->array.count )
            maxSlotCount = type->array.count;
    }
    else if( type->kind == Type::Buffer )
    {
        // We only know for sure if this is a buffer compound after we have resolved the first field
    }
    else
    {
        RSLV_ERROR( expr->pos, "Compound literals can only be used with struct or array types" );
        return resolvedNull;
    }

    if( maxSlotCount && expr->compoundFields.count > maxSlotCount )
    {
        RSLV_ERROR( expr->pos, "Too many fields in compound literal" );
        return resolvedNull;
    }

    i64 index = 0, maxIndex = 0;
    Type* expectedFieldType = nullptr;

    for( CompoundField const& f : expr->compoundFields )
    {
        if( type->kind == Type::Struct || type->kind == Type::Union )
        {
            if( f.kind != CompoundField::Name )
                RSLV_ERROR( f.pos, "Only named initializers allowed in compound literals for struct or union types" );
            else
            {
                for( TypeField const& tf : type->aggregate.fields )
                {
                    if( tf.name == f.name )
                    {
                        expectedFieldType = tf.type; 
                        break;
                    }
                }

                if( !expectedFieldType )
                    RSLV_ERROR( f.pos, "Unknown field '%s' in named initializer for type '%s'", f.name, type->name );
            }
        }
        else if( type->kind == Type::Array )
        {
            expectedFieldType = type->array.base;

            if( f.kind == CompoundField::Name )
                RSLV_ERROR( f.pos, "Named initializers not allowed in compound literals for array types" );
            else if( f.kind == CompoundField::Index )
            {
                index = ResolveConstExprInt( f.index );
                if( index < 0 )
                    RSLV_ERROR( f.pos, "Indexed initializer in compound literal cannot be negative" );
            }

            if( type->array.count && index >= type->array.count )
                RSLV_ERROR( f.pos, "Field initializer in compound literal out of range" );

            // TODO Remember which indices have been defined, and show an error in case a slot is specified more than once
            maxIndex = Max( maxIndex, index );
        }
        else if( type->kind == Type::Buffer )
        {
            expectedFieldType = index == 0 ? NewPtrType( type->array.base ) : intType;
        }

        ResolvedExpr field = ResolveExpr( f.initValue, expectedFieldType );

        if( !ConvertType( &field, expectedFieldType ) )
        {
            bool error = true;
            // HACK For buffers, if the first field is not a pointer, try continuing as an array literal instead
            if( type->kind == Type::Buffer && index == 0 )
            {
                expectedFieldType = type->array.base;
                if( ConvertType( &field, expectedFieldType ) )
                {
                    // Continue using an unknown size array
                    // FIXME We need to do the checks for arrays for this slot too!
                    type = NewArrayType( type->array.base, 0 );
                    error = false;
                }
            }

            if( error )
                RSLV_ERROR( f.pos, "Invalid type in compound literal field (expected '%s', got '%s')",
                            expectedFieldType->name, field.type->name );
        }

        index++;
    }

    // If we were given an unknown sized array type
    if( type->kind == Type::Array && type->array.count == 0 )
    {
        // Substitute type by a new one with correct sizing info (need to fetch any cached types again)
        // This leaks the old type, but who cares!
        type = NewArrayType( type->array.base, maxIndex );
    }

    return NewResolvedRvalue( type );
}

ResolvedExpr ResolveCallExpr( Expr* expr )
{
    ASSERT( expr->kind == Expr::Call );
    ResolvedExpr result = ResolveExpr( expr->call.func );
    if( !IsValid( result ) )
        return resolvedNull;

    Decl* funcDecl = nullptr;
    if( expr->call.func->kind == Expr::Name )
        funcDecl = expr->call.func->name.symbol->decl;  // Nais

    CompleteType( result.type, expr->pos );

    if( result.type->kind != Type::Func )
    {
        RSLV_ERROR( expr->pos, "Cannot call non-function value" );
        return resolvedNull;
    }

    // TODO Optionals
    int givenIndex = 0, wantedIndex = 0;
    Array<FuncArg> const& wantedArgs = result.type->func.args;
    Array<Expr*> const& givenArgs = expr->call.args;

    while( givenIndex < givenArgs.count && wantedIndex < wantedArgs.count )
    {
        FuncArg const& wantedArg = wantedArgs[wantedIndex];
        Type* wantedArgType = wantedArg.type;
        Expr* givenArg = givenArgs[givenIndex];

        ResolvedExpr resolvedArg = ResolveExpr( givenArg, wantedArgType );
        if( !ConvertType( &resolvedArg, wantedArgType ) )
        {
            // Try with the next one
            if( wantedArg.isVararg )
            {
                wantedIndex++;
                continue;
            }

            RSLV_ERROR( givenArg->pos, "Invalid type in function call argument (expected '%s', got '%s')",
                        wantedArgType->name, resolvedArg.type->name );
            return resolvedNull;
        }

        givenIndex++;
        if( !wantedArg.isVararg )
            wantedIndex++;
    }
    // If the last expected argument was a vararg, consider that one done
    if( wantedIndex < wantedArgs.count && wantedArgs[wantedIndex].isVararg )
        wantedIndex++;

    if( wantedIndex < wantedArgs.count )
    {
        // TODO Improve this
        RSLV_ERROR( givenArgs.Last()->pos, "Missing arguments in function call" ); // (expected %d, got %d)",
                    //result.type->func.args.count, expr->call.args.count );
        if( funcDecl )
            RSLV_INFO( givenArgs.Last()->pos, "First unmatched argument was '%s'", funcDecl->func.args[wantedIndex].name );
        return resolvedNull;
    }
    if( givenIndex < givenArgs.count )
    {
        // TODO Improve this
        RSLV_ERROR( givenArgs[givenIndex]->pos, "Too many arguments in function call" ); // (expected %d, got %d)",
        //result.type->func.args.count, expr->call.args.count );
        return resolvedNull;
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

    if( cond.isConst )
        return cond.constValue.BoolValue() ? thenExpr : elseExpr;
    else
        return NewResolvedRvalue( thenExpr.type );
}

ResolvedExpr ResolveIndexExpr( Expr* expr )
{
    ASSERT( expr->kind == Expr::Index );

    ResolvedExpr base = ResolveExpr( expr->index.base );
    ResolvedExpr index = ResolveExpr( expr->index.index );
    if( !ConvertType( &index, intType ) )
        RSLV_ERROR( expr->pos, "Index expression must have integer type" );

    // Disallow pointers for now
#if 0
    if( base.type->kind == Type::Pointer )
        return NewResolvedLvalue( base.type->ptr.base );
#endif
    if( base.type->kind == Type::Array || base.type->kind == Type::Buffer )
        return NewResolvedLvalue( base.type->array.base );
    else if( base.type->kind == Type::String )
        return NewResolvedRvalue( b8Type );
    else
    {
        RSLV_ERROR( expr->pos, "Can only index arrays, buffer views or strings" );
        return resolvedNull;
    }
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

            char const* name = spec->names.Last();
            Symbol* sym = ResolveName( name, pos );
            if( !sym )
                break;
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
            i64 count = 0;
            //bool dynamic = false;

            Expr* countExpr = spec->array.count;
            if( countExpr )
            {
                //if( countExpr->kind == Expr::Range )
                //{
                    //dynamic = true;
                    //if( countExpr->range.lowerBound || countExpr->range.upperBound )
                        //RSLV_ERROR( pos, "Unexpected expression in array type" );
                //}
                //else
                {
                    count = ResolveConstExprInt( countExpr );
                    // NOTE Zero-sized arrays not supported.
                    // We use count == 0 to indicate an initializer is expected to figure out the actual size
                    if( count <= 0 )
                    {
                        RSLV_ERROR( pos, "Array size must be a positive integer" );
                        //return nullptr;
                    }
                }
            }
            Type* base = ResolveTypeSpec( spec->array.base );
            if( spec->array.isView )
                result = NewBufferType( base );
            else
                result = NewArrayType( base, count );
        } break;
        case TypeSpec::Func:
        {
            Array<FuncArg> args( &globalArena, spec->func.args.count );
            for( FuncArgSpec const& argSpec : spec->func.args )
            {
                Type* arg = ResolveTypeSpec( argSpec.type );
                args.Push( { arg, argSpec.isVararg } );
            }
            Type* ret = ResolveTypeSpec( spec->func.returnType );
            result = NewFuncType( args, ret );
        } break;

        INVALID_DEFAULT_CASE
    }

    return result;
}

ResolvedExpr ResolveCastExpr( Expr* expr )
{
    ASSERT( expr->kind == Expr::Cast );
    
    Type* type = ResolveTypeSpec( expr->cast.type );
    ResolvedExpr operand = ResolveExpr( expr->cast.expr );

    if( !CastType( &operand, type ) )
    {
        RSLV_ERROR( expr->pos, "Invalid type cast (source '%s', target '%s')", operand.type->name, type->name );
    }

    return NewResolvedRvalue( type );
}

ResolvedExpr ResolveRangeExpr( Expr* expr )
{
    ASSERT( expr->kind == Expr::Range );

    if( expr->range.lowerBound )
    {
        ResolvedExpr lowerBound = ResolveExpr( expr->range.lowerBound );
        if( !ConvertType( &lowerBound, intType ) )
            RSLV_ERROR( expr->pos, "Lower bound in range expression must have integer type" );
    }

    ResolvedExpr upperBound = ResolveExpr( expr->range.upperBound );
    if( upperBound.type->kind == Type::Array || upperBound.type->kind == Type::Buffer )
    {
        if( expr->range.lowerBound )
            RSLV_ERROR( expr->pos, "Ranged buffer expression cannot have a lower bound" );
    }
    else
    {
        if( !expr->range.lowerBound )
            RSLV_ERROR( expr->pos, "Integer range expression must have a lower bound" );

        if( !ConvertType( &upperBound, intType ) )
            RSLV_ERROR( expr->pos, "Upper bound in range expression must have integer type" );
    }

    // TODO What type is this thing? Do we need a Range type?
    return NewResolvedRvalue( intType );
}

ResolvedExpr ResolveExpr( Expr* expr, Type* expectedType /*= nullptr*/ )
{
    ResolvedExpr result = {};
    switch( expr->kind )
    {
        case Expr::Int:
            result = NewResolvedConst( intType, expr->literal.intValue, expr->pos );
            break;
        case Expr::Float:
            result = NewResolvedConst( floatType, expr->literal.floatValue, expr->pos );
            break;
        case Expr::Str:
            result = NewResolvedConst( stringType, expr->literal.strValue, expr->pos );
            break;

        case Expr::Name:
            result = ResolveNameExpr( expr );
            break;
        case Expr::Field:
            result = ResolveFieldExpr( expr );
            break;
        case Expr::Index:
            result = ResolveIndexExpr( expr );
            break;
        case Expr::Compound:
            result = ResolveCompoundExpr( expr, expectedType );
            break;
        case Expr::Call:
            result = ResolveCallExpr( expr );
            break;
        case Expr::Range:
            result = ResolveRangeExpr( expr );
            break;

        case Expr::Cast:
            result = ResolveCastExpr( expr );
            break;
        case Expr::Unary:
            result = ResolveUnaryExpr( expr );
            break;
        case Expr::Binary:
            result = ResolveBinaryExpr( expr );
            break;
        case Expr::Ternary:
            result = ResolveTernaryExpr( expr, expectedType );
            break;
        case Expr::Sizeof:
        {
            ResolvedExpr size = ResolveExpr( expr->sizeofExpr );
            Type* type = size.type;
            CompleteType( type, expr->pos );

            result = NewResolvedConst( intType, TypeSize( type ), expr->pos );
        } break;
        // TODO Typeof

        case Expr::Comma:
        {
#if 0
            bool isLvalue = true;
            if( expectedType )
                result = NewResolvedRvalue( expectedType );

            int i =  0;
            bool typeMismatch = false;
            Array<ResolvedExpr> resolvedExprs( &globalTmpArena, expr->commaExprs.count );
            for( Expr* e : expr->commaExprs )
            {
                ResolvedExpr resolved = ResolveExpr( e, expectedType );
                resolvedExprs.Push( resolved );

                // NOTE We force the same type for all here. This doesn't match what we do for vars initilization
                if( !expectedType && !result.type )
                    result = resolved;

                if( !ConvertType( &resolved, result.type ) )
                {
                    if( expectedType )
                    {
                        RSLV_ERROR( e->pos, "Invalid type in expression %d of comma expression (expected '%s', got '%s')",
                                    i + 1, expectedType->name, resolved.type->name );
                    }
                    else if( !typeMismatch )
                    {
                        RSLV_ERROR( e->pos, "Cannot deduce type of comma expression (current '%s', found '%s')", result.type->name,
                                    resolved.type->name );
                        typeMismatch = true;
                    }
                }

                isLvalue = isLvalue && resolved.isLvalue;
                i++;
            }
            result.isLvalue = isLvalue;

            if( typeMismatch )
            {
                // TODO Show list of all types that were found
            }
#endif
            // NOTE Special-cased for now wherever relevant
            NOT_IMPLEMENTED;
        } break;

        INVALID_DEFAULT_CASE
    }

    ASSERT( result.type );
    expr->resolvedExpr = result;

    return result;
}

void CompleteStructType( Type* type, Array<TypeField> const& fields )
{
    ASSERT( type->kind == Type::Completing );
    type->kind = Type::Struct;
    type->aggregate.fields = fields;

    // TODO Alignment, field offsets, etc.
    type->size = 0;
    for( TypeField const& f : type->aggregate.fields )
    {
        ASSERT( f.type->kind > Type::Completing );
        type->size += TypeSize( f.type );
    }
}

void CompleteUnionType( Type* type, Array<TypeField> const& fields )
{
    ASSERT( type->kind == Type::Completing );
    type->kind = Type::Union;
    type->aggregate.fields = fields;

    // TODO Alignment, field offsets, etc.
    type->size = 0;
    for( TypeField const& f : type->aggregate.fields )
    {
        ASSERT( f.type->kind > Type::Completing );
        type->size = Max( type->size, TypeSize( f.type ) );
    }
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
    if( !type )
        return;

    ASSERT( type->kind );

    if( type->kind == Type::Completing )
    {
        // TODO
        RSLV_ERROR( pos, "Circular dependency detected" );
        return;
    }
    else if( type->kind != Type::Incomplete )
    {
        // Nothing to do
        return;
    }

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

            d->resolvedType = fieldType;
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

    // First look in the local scope stack
    {
        for( auto idx = globalScopeStack.Last(); idx; --idx )
        {
            if( (*idx).name == name )
            {
                result = &*idx;
                break;
            }
        }
    }

    // Otherwise search in the global symbols
    if( !result )
    {
        result = globalSymbols.Get( name );
    }

    return result;
}

Symbol* PushSymbol( char const* name, bool isLocal, SourcePos const& pos )
{
    Symbol* prevSymbol = GetSymbol( name );

    Symbol* sym = nullptr;
    if( prevSymbol == nullptr )
    {
        if( isLocal )
            sym = globalScopeStack.PushEmpty();
        else
            sym = globalSymbols.PutEmpty( name );
    }
    else
    {
        if( prevSymbol->decl )
        {
            RSLV_ERROR( pos, "Symbol '%s' already declared", name );
            RSLV_INFO( prevSymbol->decl->pos, "Symbol '%s' was declared here", name );
        }
        else
            RSLV_ERROR( pos, "Name '%s' is reserved", name );

        prevSymbol->state = Symbol::Tainted;
    }

    return sym;
}

BucketArray<Symbol>::Idx<false> EnterScope()
{
    return globalScopeStack.End();
}

void LeaveScope( BucketArray<Symbol>::Idx<false> const& scopeStartIdx )
{
    globalScopeStack.PopUntil( scopeStartIdx );
    ASSERT( globalScopeStack.End() == scopeStartIdx );
}

void ResolveSymbol( Symbol* sym )
{
    if( sym->state == Symbol::Resolved || sym->state == Symbol::Tainted )
        // Nothing to do
        return;

    Decl* decl = sym->decl;
    if( decl->directives.Contains( globalDirectives[Directive::DebugBreak] ) )
        __debugbreak();

    SourcePos const& pos = decl->pos;
    if( sym->state == Symbol::Resolving )
    {
        // TODO Log dependency graph nodes to aid debugging?
        RSLV_ERROR( pos, "Circular dependency detected" );
        return;
    }

    ASSERT( sym->state == Symbol::Unresolved );
    sym->state = Symbol::Resolving;

    Type* result = nullptr;
    switch( sym->kind )
    {
        case Symbol::Var:
        {
            ASSERT( decl->kind == Decl::Var );
            
            if( decl->var.type )
            {
                result = ResolveTypeSpec( decl->var.type );
                CompleteType( result, decl->pos );
            }

            // Deal with comma exprs
            // TODO Should we generalize comma expressions better? (would need some kind of compound ResolvedExpr)
            // NOTE When we get here we're resolving _one_ of the symbols in the multiple decl, so search for it by name
            // and compare with the correponding slot in the comma expression initializer (if any)
            Expr* initExpr = decl->var.initExpr;
            if( initExpr && decl->names.count > 1 )
            {
                int declIndex = 0;
                for( char const* name : decl->names )
                {
                    if( name == sym->name )
                        break;
                    declIndex++;
                }
                ASSERT( declIndex < decl->names.count );

                if( initExpr->kind == Expr::Comma )
                {
                    if( declIndex < initExpr->commaExprs.count )
                        initExpr = initExpr->commaExprs[declIndex];
                    else 
                    {
                        if( result )
                        {
                            // Use given type and zero-initialize
                            initExpr = nullptr;
                        }
                        else
                        {
                            RSLV_ERROR( pos, "Missing initializer for implicitly typed symbol declaration '%s'", sym->name );
                            result = nullType;
                            break;
                        }
                    }
                }
            }

            if( initExpr )
            {
                ResolvedExpr init = ResolveExpr( initExpr, result );
                if( result && init.type && !ConvertType( &init, result ) )
                {
                    // Make an exception for empty-size arrays
                    if( result->kind != Type::Array || result->array.count != 0 )
                    {
                        RSLV_ERROR( pos, "Invalid type in initializer expression. Expected '%s', got '%s'", result->name, init.type->name );
                    }
                }

                result = init.type;
            }
        } break;

        case Symbol::Const:
        {
            ASSERT( decl->kind == Decl::Var );
            ASSERT( decl->var.isConst );

            // TODO Multiple decl constants
            // NOTE Type is always inferred for constants right now
            ResolvedExpr init = ResolveExpr( decl->var.initExpr );
            if( !init.isConst )
                RSLV_ERROR( pos, "Initializer for constant '%s' is not a constant expression", sym->name );

            sym->constValue = init.constValue;
            result = init.type;
        } break;

        case Symbol::Type:
        {
            // TODO This would only be for typedefs now, as aggregates start out already resolved
            INVALID_CODE_PATH
        } break;

        case Symbol::Func:
        {
            Array<FuncArg> args( &globalArena, decl->func.args.count );
            for( FuncArgDecl const& a : decl->func.args )
            {
                Type* argType = ResolveTypeSpec( a.type );
                CompleteType( argType, a.pos );
                args.Push( { argType, a.isVararg } );
            }

            Type* retType = voidType;
            if( decl->func.returnType )
            {
                retType = ResolveTypeSpec( decl->func.returnType );
                CompleteType( retType, decl->func.returnType->pos );
            }

            result = NewFuncType( args, retType );
        } break;

        case Symbol::Module:
        {
        } break;

        INVALID_DEFAULT_CASE
    }

    decl->resolvedType = sym->type = result;

    if( sym->type && sym->type->kind != Type::None )
        sym->state = Symbol::Resolved;
    else
        sym->state = Symbol::Tainted;

    if( !sym->isLocal )
        globalOrderedSymbols.Push( sym );
}

ResolvedExpr ResolveConditionalExpr( Expr* expr )
{
    ResolvedExpr cond = ResolveExpr( expr );
    // TODO
    if( cond.type != intType )
        RSLV_ERROR( expr->pos, "Condition must have integer type" );

    return cond;
}

INLINE bool IsForeign( Decl* decl )
{
    return decl->directives.Contains( globalDirectives[ Directive::Foreign ] );
}

bool ResolveStmtBlock( StmtList const* block, Type* returnType );
Array<Symbol*> CreateDeclSymbols( Decl* decl, bool isLocal );

void ResolveFuncBody( Symbol* sym )
{
    ASSERT( sym->state == Symbol::Resolved );

    Decl* decl = sym->decl;
    ASSERT( decl->kind == Decl::Func );

    Type* type = sym->type;
    ASSERT( type->kind == Type::Func );

    auto scopeIdx = EnterScope();

    int i = 0;
    for( FuncArgDecl const& a : decl->func.args )
    {
        Type* argType = type->func.args[i].type;
        CompleteType( argType, a.pos );

        // Create a synthetic Decl
        Decl* varDecl = NewVarDecl( a.pos, a.name, a.type, nullptr, decl->func.body );
        CreateDeclSymbols( varDecl, true );
    }

    if( type->func.returnType )
        CompleteType( type->func.returnType, decl->func.returnType->pos );
    else
        type->func.returnType = voidType;

    if( !IsForeign( decl ) )
    {
        bool returns = ResolveStmtBlock( decl->func.body, type->func.returnType );
        if( !returns && type->func.returnType != voidType )
            RSLV_ERROR( decl->pos, "Not all control paths return a value" );
    }

    LeaveScope( scopeIdx );
}

void CompleteSymbol( Symbol* sym )
{
    ResolveSymbol( sym );

    if( sym->kind == Symbol::Type )
        CompleteType( sym->type, sym->decl->pos );
    else if( sym->kind == Symbol::Func )
        ResolveFuncBody( sym );
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

void PushNode( Stmt* node )
{
    globalNodeStack.Push( node );
}

void PopNode()
{
    auto top = globalNodeStack.Last();
    ASSERT( top.IsValid() );

    Stmt* node = *top;
    globalNodeStack.Pop();

    // Check expected errors
    if( node->directives.Contains( globalDirectives[Directive::ExpectError] ) )
        RSLV_ERROR( node->pos, "Expected error but none was generated" );
}

bool ResolveStmt( Stmt* stmt, Type* returnType )
{
    bool returns = false;
    PushNode( stmt );

    // TODO Check all control paths return a value
    switch( stmt->kind )
    {
        case Stmt::Block:
            returns = ResolveStmtBlock( stmt->block, returnType );
            break;
        case Stmt::Expr:
            ResolveExpr( stmt->expr );
            break;
        case Stmt::Decl:
        {
            Array<Symbol*> symbols = CreateDeclSymbols( stmt->decl, true );
            for( Symbol* sym : symbols )
                CompleteSymbol( sym );
        } break;
        case Stmt::Assign:
        {
            ResolvedExpr left = ResolveExpr( stmt->assign.left );
            ASSERT( left.type );
            if( !left.isLvalue )
                RSLV_ERROR( stmt->assign.left->pos, "Expression cannot be assigned to (not an l-value)" );
            //if( left.isConst )
                //RSLV_ERROR( stmt->assign.left->pos, "Cannot assign to constant" );

            ResolvedExpr right = ResolveExpr( stmt->assign.right, left.type );
            TokenKind::Enum op = stmt->assign.op;
            TokenKind::Enum binaryOp = assignOpToBinaryOp[ stmt->assign.op ];

            ResolvedExpr result = {};
            if( op == TokenKind::Assign )
                result = right;
            else
                result = ResolveBinaryExpr( binaryOp, TokenKind::Items::names[ op ], &left, &right, stmt->pos );

            if( !ConvertType( &result, left.type ) )
            {
                RSLV_ERROR( stmt->pos, "Invalid types in assignment (expected '%s', got '%s')",
                            left.type->name, right.type->name );
            }

        } break;

        case Stmt::If:
        {
            ResolvedExpr cond = ResolveConditionalExpr( stmt->if_.cond );
            returns = ResolveStmtBlock( stmt->if_.thenBlock, returnType );

            for( ElseIf const& ei : stmt->if_.elseIfs )
            {
                ResolvedExpr elseIfCond = ResolveConditionalExpr( ei.cond );
                returns = ResolveStmtBlock( ei.block, returnType ) && returns;
            }

            if( stmt->if_.elseBlock->stmts.count )
                returns = ResolveStmtBlock( stmt->if_.elseBlock, returnType ) && returns;
            else
                returns = false;
        } break;

        case Stmt::While:
        {
            ResolvedExpr cond = ResolveConditionalExpr( stmt->while_.cond );
            ResolveStmtBlock( stmt->while_.block, returnType );
        } break;
        case Stmt::For:
        {
            auto forScopeIdx = EnterScope();

            Expr* rangeExpr = stmt->for_.rangeExpr;
            SourcePos const& pos = rangeExpr->pos;

            ResolveExpr( rangeExpr );

            // Create a synthetic Decl
            // TODO Support type specifiers? (make this more Decl-like)
            Decl* varDecl = NewVarDecl( stmt->pos, stmt->for_.indexName, nullptr, rangeExpr, stmt->for_.block );
            CreateDeclSymbols( varDecl, true );

            ResolveStmtBlock( stmt->for_.block, returnType );

            LeaveScope( forScopeIdx );
        } break;

        case Stmt::Switch:
        {
            bool hasDefault = false;

            returns = true;
            ResolvedExpr switchExpr = ResolveExpr( stmt->switch_.expr );
            for( SwitchCase const& c : stmt->switch_.cases )
            {
                if( c.isDefault )
                {
                    if( hasDefault )
                        RSLV_ERROR( c.block->pos, "Multiple default clauses in switch statement" );
                    hasDefault = true;
                }
                else
                {
                    // TODO Ranges
                    // TODO Comma expressions?
                    ResolvedExpr caseResult = ResolveExpr( c.expr );
                    if( !ConvertType( &caseResult, switchExpr.type ) )
                        RSLV_ERROR( c.expr->pos, "Type mismatch in case expression" );
                }

                returns = ResolveStmtBlock( c.block, returnType ) && returns;
            }

            returns = returns && hasDefault;
        } break;

        case Stmt::Break:
        case Stmt::Continue:
        // TODO Check there is an enclosing loop (or switch)
        break;
        case Stmt::Return:
        {
            if( stmt->expr )
            {
                // TODO Return multiple values
                ResolvedExpr result = ResolveExpr( stmt->expr, returnType );
                if( result.type && !ConvertType( &result, returnType ) )
                {
                    RSLV_ERROR( stmt->expr->pos, "Invalid type in return expression (expected '%s', got '%s')",
                                returnType->name, result.type->name );
                }
            }
            else
            {
                if( returnType != voidType )
                {
                    RSLV_ERROR( stmt->pos, "Empty return expression for function with non-void return type" );
                }
            }
            
            returns = true;
        } break;

        case Stmt::None:    // Empty
        break;

        default:
        NOT_IMPLEMENTED;
    }

    PopNode();
    return returns;
}

bool ResolveStmtBlock( StmtList const* block, Type* returnType )
{
    auto scopeIdx = EnterScope();

    bool returns = false;
    for( Stmt* stmt : block->stmts )
    {
        returns = ResolveStmt( stmt, returnType ) || returns;
    }

    LeaveScope( scopeIdx );
    return returns;
}

Array<Symbol*> CreateDeclSymbols( Decl* decl, bool isLocal )
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
        // TODO Don't we need to add all sub-types and contained symbols for these?
        case Decl::Struct:
        case Decl::Union:
        case Decl::Enum:
            kind = Symbol::Type;
            break;

        INVALID_DEFAULT_CASE;
    }

    Array<Symbol*> result( &globalTmpArena, decl->names.count );
    for( char const* name : decl->names )
    {
        Symbol* sym = PushSymbol( name, isLocal, decl->pos );
        if( sym )
        {
            sym->decl = decl;
            sym->name = name;
            sym->kind = kind;
            sym->isLocal = isLocal;
            sym->state = Symbol::Unresolved;

            if( decl->kind == Decl::Struct || decl->kind == Decl::Union )
            {
                sym->state = Symbol::Resolved;
                sym->type = NewIncompleteType( name, sym );
            }

            result.Push( sym );
        }
    }

    return result;
}


void PushGlobalDeclSymbols( Decl* decl )
{
    Array<Symbol*> symbols = CreateDeclSymbols( decl, false );
    for( Symbol* s : symbols )
        globalSymbolsList.Push( s );
}

Symbol* PushGlobalTypeSymbol( char const* name, Type* type )
{
    InternString* internName = Intern( String( name ) );
    // Use interned string for both key and value!
    name = internName->data;

    Symbol sym = {};
    sym.name = name;
    sym.type = type;
    sym.kind = Symbol::Type;
    sym.state = Symbol::Resolved;

    Symbol* result = globalSymbols.Put( name, sym );
    //globalSymbolsList.Push( result );

    return result;
}

Symbol* PushGlobalConstSymbol( char const* name, Type* type, bool value )
{
    InternString* internName = Intern( String( name ) );
    // Use interned string for both key and value!
    name = internName->data;

    Symbol sym = {};
    sym.name = name;
    sym.type = type;
    sym.kind = Symbol::Const;
    sym.state = Symbol::Resolved;
    sym.constValue.bitsValue = value;

    Symbol* result = globalSymbols.Put( name, sym );
    //globalSymbolsList.Push( result );

    return result;
}

Symbol* PushGlobalFuncSymbol( char const* name, Buffer<FuncArg> args, Buffer<Type*> returnTypes )
{
    InternString* internName = Intern( String( name ) );
    // Use interned string for both key and value!
    name = internName->data;

    Symbol sym = {};
    sym.name = name;
    // TODO 
    Array<FuncArg> argsArray = Array<FuncArg>::Clone( args, &globalArena );
    sym.type = NewFuncType( argsArray, returnTypes ? returnTypes[0] : voidType );
    sym.kind = Symbol::Func;
    sym.state = Symbol::Resolved;

    Symbol* result = globalSymbols.Put( name, sym );
    //globalSymbolsList.Push( result );

    return result;
}

void InitResolver( int globalSymbolsCount )
{
    INIT( globalScopeStack ) BucketArray<Symbol>( &globalArena, 1024 );
    INIT( globalSymbolsList ) BucketArray<Symbol*>( &globalArena, 256 );
    INIT( globalOrderedSymbols ) BucketArray<Symbol*>( &globalArena, 256 );
    INIT( globalNodeStack ) BucketArray<Stmt*>( &globalArena, 16 );

    // TODO Make a generic type comparator so we can turn this into a hashtable
    INIT( globalCachedTypes ) BucketArray<Type*>( &globalArena, 256 );

    INIT( globalSymbols ) Hashtable<char const*, Symbol, MemoryArena>( &globalArena,
        globalSymbolsCount + I32( ARRAYCOUNT(globalBuiltinTypes) ), HTF_FixedSize );

    InitErrorBuffer();

    // Push symbols for builtin types
    globalCurrentScopeStart = globalScopeStack.First();
    for( BuiltinType& b : globalBuiltinTypes )
    {
        if( IsNullOrEmpty( b.symbolName ) )
            break;

        if( b.isAlias )
            PushGlobalTypeSymbol( b.symbolName, b.aliasType );
        else
            PushGlobalTypeSymbol( b.symbolName, &b.type );
    }

    PushGlobalConstSymbol( "true", boolType, true );
    PushGlobalConstSymbol( "false", boolType, false );

    // TODO Add varargs of type any
    PushGlobalFuncSymbol( "expect", BUFFER( FuncArg, { boolType } ), {} );
}

void ResolveAll( Array<Decl*> const& globalDecls )
{
    for( Decl* decl : globalDecls )
    {
        PushGlobalDeclSymbols( decl );
    }

    for( auto it = globalSymbolsList.First(); it; ++it )
    {
        CompleteSymbol( *it );
    }

    // Print all errors
    for( auto it = globalErrorBuffer.Iterator(); *it; ++it )
    {
        String msg = it->msg;
        globalPlatform.Error( msg.data );

        if( it->infoCount >= 0 )
            globalErrorCount++;
    }

#if !RELEASE
    if( globalErrorCount )
        __debugbreak();
#endif
}
