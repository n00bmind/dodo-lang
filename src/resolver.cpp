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
    Symbol* parent;
    ::Type* type;
    // TODO Don't store this here
    ConstValue constValue = {};
    u32 kind : 16;
    u32 state : 15;
    // TODO Namespaces?
    u32 isLocal : 1;
};


struct FuncArg
{
    Type* type;
    Expr* defaultValue;     // Just point to the (resolved) expr, if any
    bool isVararg;
};

struct TypeField
{
    char const* name;
    Type* type;
    ResolvedExpr init;
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
        Bits,
        Int,
        // TODO This is NOT just the scalar enum (non-struct). Expression resolution rn assumes this can only be int or bits!
        // Also, expression resolution treats this as the underlying value, but we've now moved to store the index instead
        // Review all use cases
        Enum,
        Float,
        Pointer,
        Func,
        Array,
        Buffer,     // a.k.a. array view
        String,
        Struct,
        Union,
        Multi,      // The type for multiple decls/assignments, and probably for multiple return values too
        TypeVal,    // TODO This should probably link to the type id somehow

        //SBuffer,        // TODO ?
        // TODO 
        Any,
    };

    enum Flags
    {
        Emitted     = 0x1,
    };

    char const* name;
    // NOTE Only user types (aggregates and enums) will have one
    // (only user created types are biunivocally associated with a symbol)
    Symbol* symbol;
    // TODO I guess we could make these two 32 bits without real harm?
    sz size;
    sz align;
    Kind kind;
    u32 flags;

    union
    {
        struct
        {
            ::Array<FuncArg> args;
            // NOTE This will be a Multi type when there are multiple return values
            Type* returnType;
            // Annotated source decl (if available) to access arg names
            Decl* resolvedDecl;
        } func;
        struct
        {
            ::Array<TypeField> fields;
        } aggregate;
        struct
        {
            // List of the (resolved) items, including None
            // TODO This should probably contain ConstValues instead, when that supports complex types
            ::Array<EnumItem> items;
            Type* base;
        } enum_;
        struct
        {
            Type* base;
            // 0 = unknown (needs initializer)
            // Unused for buffers, as their size is dynamic
            sz length;
        } array;
        struct
        {
            Type* base;
        } ptr;
        struct
        {
            ::Array<Type*> types;
        } multi;
        struct
        {
            Type* type;
        } value;
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
// TODO Remove?
//internal BucketArray<Symbol>::Idx<false> globalCurrentScopeStart;

BucketArray<Stmt*> globalNodeStack;

// List of unique interned types (a.k.a hash consing)
// TODO Hashtable!
internal BucketArray<Type*> globalCachedTypes;

// Special decls that will have to be emitted in the global scope in the C codegen
internal BucketArray<Decl*> globalForwardDecls;
// Multi types also need to be emitted before any function body that uses them
internal BucketArray<Type*> globalForwardTypes;

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
Type* intType = NewBuiltinType( "int", i32Type );
Type* b8Type = NewBuiltinType( "b8", Type::Bits, 1, 1 );
Type* b16Type = NewBuiltinType( "b16", Type::Bits, 2, 2 );
Type* b32Type = NewBuiltinType( "b32", Type::Bits, 4, 4 );
Type* b64Type = NewBuiltinType( "b64", Type::Bits, 8, 8 );
Type* bitsType = NewBuiltinType( "bits", b32Type );
Type* f32Type = NewBuiltinType( "f32", Type::Float, 4, 4 );
Type* f64Type = NewBuiltinType( "f64", Type::Float, 8, 8 );
Type* floatType = NewBuiltinType( "float", f32Type );
// NOTE This stuff needs to be in sync with the structs in the preamble!
Type* stringType = NewBuiltinType( "string", Type::String, globalTgtPlatform.PointerSize, globalTgtPlatform.PointerSize );
// TODO How big is this guy? How does any even work?
Type* anyType = NewBuiltinType( "any", Type::Any, globalTgtPlatform.PointerSize, globalTgtPlatform.PointerSize );

// Error marker
Type* nullType = NewBuiltinType( "", Type::None, 0, 0 );
ResolvedExpr resolvedNull = { nullType, };

bool IsValid( Type* type )
{
    return type && type->kind != Type::None;
}

bool IsValid( ResolvedExpr const& resolved )
{
    return IsValid( resolved.type );
}



Type* NewType( char const* name, Type::Kind kind, sz size, sz alignment )
{
    Type* result = PUSH_STRUCT( &globalArena, Type );
    result->name = name;
    result->kind = kind;
    result->size = size;
    result->align = alignment;
    result->flags = 0;

    globalCachedTypes.Push( result );

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

    Type* result = NewType( "pointer", Type::Pointer, globalTgtPlatform.PointerSize, globalTgtPlatform.PointerSize );
    result->ptr.base = base;

    return result;
}

sz TypeSize( Type* type )
{
    ASSERT( type->kind > Type::Completing );
    return type->size;
}

sz TypeAlignment( Type* type )
{
    ASSERT( type->kind > Type::Completing );
    return type->align;
}

Type* NewArrayType( Type* base, sz length )
{
    auto idx = globalCachedTypes.First(); 
    while( idx )
    {
        Type* t = *idx;
        if( t->kind == Type::Array )
        {
            if( t->array.base == base && t->array.length == length )
                return t;
        }
        idx.Next();
    }

    // NOTE We allow no-size arrays to pass through, as they'll be checked and substituted during ResolveSymbol
    Type* result = NewType( "array", Type::Array, length * TypeSize( base ), TypeAlignment( base ) );
    result->array.base = base;
    result->array.length = length;

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

    Type* result = NewType( "buffer view", Type::Buffer, globalTgtPlatform.PointerSize + 4, globalTgtPlatform.PointerSize );
    result->array.base = base;

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

    Type* result = NewType( "function", Type::Func, globalTgtPlatform.PointerSize, globalTgtPlatform.PointerSize );
    // NOTE Assume passed args are allocated in permanent memory
    result->func.args = args;
    result->func.returnType = returnType;

    return result;
}

Type* NewMultiType( Array<Type*> const& types )
{
    auto idx = globalCachedTypes.First(); 
    while( idx )
    {
        Type* t = *idx;
        if( t->kind == Type::Multi )
        {
            if( t->multi.types.count != types.count )
                continue;

            bool match = true;
            for( int i = 0; i < types.count; ++i )
            {
                if( t->multi.types[i] != types[i] )
                {
                    match = false;
                    break;
                }
            }

            if( match )
                return t;
        }
        idx.Next();
    }

    // TODO Do we care about the size & alignment of this guy?
    Type* result = NewType( "multi", Type::Multi, 0, 0 );
    // NOTE Assume given array is allocated in permanent memory
    result->multi.types = types;

    return result;
}

Type* NewTypeValType( Type* value, Symbol* symbol = nullptr )
{
    auto idx = globalCachedTypes.First(); 
    while( idx )
    {
        Type* t = *idx;
        if( t->kind == Type::TypeVal )
        {
            if( t->value.type == value )
                return t;
        }
        idx.Next();
    }

    // TODO Assume this will just store a type id or similar
    Type* result = NewType( "type", Type::TypeVal, 4, 4 );
    result->value.type = value;
    result->symbol = symbol;

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
    ASSERT( type->kind == Type::Bits || type->kind == Type::Bool );

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

ResolvedExpr NewResolvedConst( Type* type, bool boolValue )
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

ResolvedExpr NewResolvedConst( Type* type, uintptr_t ptrValue )
{
    ASSERT( type->kind == Type::Pointer );

    ResolvedExpr result = {};
    result.type = type;
    result.isConst = true;
    result.constValue.ptrValue = ptrValue;
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
    else if( dest->kind == Type::Bool && IsScalarType( src ) )
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
    else if( IsArithmeticType( dest ) && IsArithmeticType( src ) )
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


Symbol* ResolveName( char const* name, SourcePos const& pos, Symbol* parentSymbol = nullptr );
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
    ResolvedExpr result = ResolveConstExpr( expr, i64Type );
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
    {
        Type::Kind kind = sym->type->kind;
        if( kind == Type::Enum )
        {
            if( sym->type->enum_.base->kind == Type::Int )
                kind = Type::Int;
            else if( sym->type->enum_.base->kind == Type::Bits )
                kind = Type::Bits;
        }

        switch( kind )
        {
            case Type::String:
                return NewResolvedConst( sym->type, sym->constValue.strValue, expr->pos );
            case Type::Pointer:
                return NewResolvedConst( sym->type, sym->constValue.ptrValue );
            case Type::Float:
                return NewResolvedConst( sym->type, sym->constValue.floatValue, expr->pos );
            case Type::Int:
                return NewResolvedConst( sym->type, sym->constValue.intValue, expr->pos );
            case Type::Bits:
                return NewResolvedConst( sym->type, sym->constValue.bitsValue, expr->pos );
            case Type::Bool:
                return NewResolvedConst( sym->type, sym->constValue.BoolValue() );

            default:
                INVALID_CODE_PATH;
                return resolvedNull;
        }
    }
    else if( sym->kind == Symbol::Func )
        return NewResolvedRvalue( sym->type );
    else if( sym->kind == Symbol::Type )
        //return NewResolvedRvalue( typeType );
        return NewResolvedRvalue( NewTypeValType( sym->type, sym ) );
    else
    {
        ASSERT( sym->kind == Symbol::Module );
        NOT_IMPLEMENTED;
        return resolvedNull;
    }
}

TypeField const* FindTypeField( Type* baseType, char const* name )
{
    ASSERT( baseType->kind == Type::Struct || baseType->kind == Type::Union );

    TypeField const* result = nullptr;
    for( TypeField const& f : baseType->aggregate.fields )
    {
        if( f.name == name )
        {
            result = &f;
            break;
        }
    }

    return result;
}

int FindMetaAttr( char const* name )
{
    int index = -1;
    for( int i = 0; i < ARRAYCOUNT(globalMetaAttrs); ++i )
        if( globalMetaAttrs[i] == name )
        {
            index = i;
            break;
        }

    return index;
}

ResolvedExpr ResolveMetaFieldExpr( Expr* base, char const* name, SourcePos const& pos )
{
    int index = FindMetaAttr( name );
    ASSERT( index >= 0 );

    ResolvedExpr result = resolvedNull;
    Type* baseType = base->resolvedExpr.type;

    switch( index )
    {
        case MetaAttr::Size:
            result = NewResolvedConst( i64Type, baseType->size, pos );
            break;

        case MetaAttr::Offset:
        {
            Expr* fieldBase = (base->kind == Expr::Field && !base->field.isMeta) ? base->field.base : nullptr;
            Type* fieldBaseType = fieldBase ? fieldBase->resolvedExpr.type : nullptr;

            if( fieldBaseType && fieldBaseType->kind == Type::TypeVal )
                fieldBaseType = fieldBaseType->value.type;

            if( !fieldBase || !(fieldBaseType->kind == Type::Struct || fieldBaseType->kind == Type::Union) )
            {
                RSLV_ERROR( pos, "'%s' attribute is only valid for aggregate fields", name );
                return resolvedNull;
            }

            TypeField const* field = FindTypeField( fieldBaseType, base->field.name );
            // If the field doesn't exist, it should already have failed resolving above
            ASSERT( field );
            result = NewResolvedConst( i64Type, field->offset, pos );
        } break;

    // TODO This is different for arrays and buffers, as this could be considered an actual "field" in buffers (and it's not a constant)
        case MetaAttr::Length:
        {
            if( !(baseType->kind == Type::Array || baseType->kind == Type::Buffer || baseType->kind == Type::String) )
            {
                RSLV_ERROR( pos, "'%' attribute is only valid for arrays, buffer views and strings", name );
                return resolvedNull;
            }

            result = NewResolvedRvalue( i64Type );
            if( baseType->kind == Type::Array )
            {
                result.isConst = true;
                result.constValue.intValue = baseType->array.length;
            }
        } break;

        case MetaAttr::Index:
        {
            if( baseType->kind != Type::Enum )
            {
                RSLV_ERROR( pos, "'%' attribute is only valid for enum values", name );
                return resolvedNull;
            }

            result = NewResolvedRvalue( i32Type );
        } break;
        case MetaAttr::Name:
        {
            if( baseType->kind != Type::Enum )
            {
                RSLV_ERROR( pos, "'%' attribute is only valid for enum values", name );
                return resolvedNull;
            }

            result = NewResolvedRvalue( stringType );
        } break;
    }

    return result;
}

bool CompleteType( Type* type, SourcePos const& pos );

ResolvedExpr ResolveFieldExpr( Expr* expr )
{
    ASSERT( expr->kind == Expr::Field );

    // TODO Modules
    ResolvedExpr left = ResolveExpr( expr->field.base );
    Type* srcBaseType = left.type;
    Type* baseType = srcBaseType;
    
    if( !baseType || IsTainted( baseType ) )
        return resolvedNull;
    CompleteType( baseType, expr->pos );

    if( expr->field.isMeta )
        return ResolveMetaFieldExpr( expr->field.base, expr->field.name, expr->pos );

#if 0
    // TODO This is different for arrays and buffers, as this could be considered an actual "field" in buffers (and it's not a constant)
    if( MatchKeyword( Keyword::Length, expr->field.name ) )
    {
        if( !(baseType->kind == Type::Array || baseType->kind == Type::Buffer || baseType->kind == Type::String) )
        {
            RSLV_ERROR( expr->pos, "Only arrays, buffer views and strings have a 'length' attribute" );
            return resolvedNull;
        }

        ResolvedExpr result = NewResolvedRvalue( i64Type );
        if( baseType->kind == Type::Array )
        {
            result.isConst = true;
            result.constValue.intValue = baseType->array.length;
        }

        return result;
    }
#endif

    // 'Normal' field access of aggregates or pointers to such
    if( baseType->kind == Type::Pointer )
    {
        left = NewResolvedLvalue( baseType->ptr.base );
        baseType = left.type;
        CompleteType( baseType, expr->pos );
    }
    else if( baseType->kind == Type::TypeVal )
    {
        left = NewResolvedRvalue( baseType->value.type );
        baseType = left.type;
        CompleteType( baseType, expr->pos );
    }

    if( baseType->kind == Type::Enum )
    {
        for( EnumItem const& it : baseType->enum_.items )
        {
            if( it.name == expr->field.name )
            {
                ResolvedExpr resolvedField = it.initExpr->resolvedExpr;
                return resolvedField;
            }
        }
        RSLV_ERROR( expr->pos, "No item named '%s' in enum type '%s'", expr->field.name, baseType->name );
        return resolvedNull;
    }

    if( !(baseType->kind == Type::Struct || baseType->kind == Type::Union) )
    {
        RSLV_ERROR( expr->pos, "Unknown attribute '%s' in expression of type '%s'", expr->field.name, srcBaseType->name );
        return resolvedNull;
    }

    for( TypeField const& f : baseType->aggregate.fields )
    {
        if( f.name == expr->field.name )
        {
            ResolvedExpr resolvedField = left.isLvalue
                ? NewResolvedLvalue( f.type )
                : NewResolvedRvalue( f.type );
            return resolvedField;
        }
    }

    if( srcBaseType->kind == Type::TypeVal && srcBaseType->symbol )
    {
        // If this is a user type we may be trying to access a subtype
        Symbol* sym = ResolveName( expr->field.name, expr->pos, srcBaseType->symbol );
        return NewResolvedRvalue( NewTypeValType( sym->type, sym ) );
    }

    RSLV_ERROR( expr->pos, "No field named '%s' in type '%s'", expr->field.name, baseType->name );
    return resolvedNull;
}

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
        return NewResolvedConst( type, value );
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
                            TokenKind::names[ expr->unary.op ] );
                return resolvedNull;
            }
            break;
        case TokenKind::Tilde:
            if( !IsIntegralType( type ) )
            {
                RSLV_ERROR( expr->pos, "Can only use unary '%s' with integral types",
                            TokenKind::names[ expr->unary.op ] );
                return resolvedNull;
            }
            break;
        case TokenKind::Exclamation:
            if( !IsScalarType( type ) )
            {
                RSLV_ERROR( expr->pos, "Can only use unary '%s' with scalar types",
                            TokenKind::names[ expr->unary.op ] );
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

    if( TokenKind::items[ op ].value.flags == TokenFlags::CmpOp )
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
                    if( !CompleteType( ptrOperand->type, pos ) )
                        break;

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
                // TODO Do we wanna also enforce the same type on both for ints vs. bits?
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
                // TODO Do we care about comparing pointers to different types?
                return NewResolvedRvalue( boolType );
            else if( (IsPointerType( left->type ) && IsNullPtrConst( *right )) || (IsNullPtrConst( *left ) && IsPointerType( right->type )) )
                return NewResolvedRvalue( boolType );
            else if( left->type == right->type )
                return NewResolvedRvalue( boolType );
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

                    return NewResolvedConst( boolType, result );
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
    char const* opName = TokenKind::names[op];

    return ResolveBinaryExpr( op, opName, &left, &right, expr->pos );
}

ResolvedExpr ResolveCompoundExpr( Expr* expr, Type* expectedType )
{
    ASSERT( expr->kind == Expr::Compound );
    ASSERT( expectedType );

    if( expectedType && !CompleteType( expectedType, expr->pos ) )
        return resolvedNull;

    Type* type = expectedType;

    sz maxSlotCount = 0;
    if( type->kind == Type::Union )
        maxSlotCount = 1;
    else if( type->kind == Type::Struct )
        maxSlotCount = type->aggregate.fields.count;
    else if( type->kind == Type::Array )
        maxSlotCount = type->array.length;
    else if( type->kind == Type::Buffer )
        // We only know for sure if this is a buffer compound after we have resolved the first field
        ;
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

    bool flexibleArray = type->kind == Type::Array && type->array.length == 0;
    bool parseArrayLiteral = false;

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
        else if( type->kind == Type::Array || type->kind == Type::Buffer )
        {
            expectedFieldType = type->array.base;

            if( type->kind == Type::Buffer && !parseArrayLiteral )
            {
                expectedFieldType = index == 0 ? NewPtrType( type->array.base ) : intType;
                if( index >= 2 )
                    RSLV_ERROR( f.pos, "Buffer initializer must have only pointer and length fields" );
            }

            if( f.kind == CompoundField::Name )
                RSLV_ERROR( f.pos, "Named initializers not allowed in compound literals for array types" );
            else if( f.kind == CompoundField::Index )
            {
                ResolvedExpr indexExpr = ResolveExpr( f.index );

                if( indexExpr.type->kind == Type::Int )
                {
                    if( !indexExpr.isConst )
                        RSLV_ERROR( f.pos, "Indexed initializer must be a constant expression" );

                    index = indexExpr.constValue.intValue;
                    if( index < 0 )
                        RSLV_ERROR( f.pos, "Indexed initializer in compound literal cannot be negative" );
                }

                if( f.index->kind == Expr::Range )
                {
                    if( !indexExpr.isConst )
                        RSLV_ERROR( f.pos, "Ranged initializer must be a constant expression" );

                    if( f.index->range.lowerBound == nullptr && f.index->range.upperBound == nullptr )
                    {
                        if( flexibleArray )
                            RSLV_ERROR( f.pos, "Arrays with unknown size cannot be given a full range initializer" );
                        else if( type->kind == Type::Buffer )
                            RSLV_ERROR( f.pos, "Buffer views cannot be given a full range initializer" );
                    }
                }
            }

            if( type->array.length && index >= type->array.length )
                RSLV_ERROR( f.pos, "Field initializer in compound literal out of range" );

            // TODO Remember which indices have been defined, and show an error in case a slot is specified more than once
            maxIndex = Max( maxIndex, index );
        }

        ResolvedExpr field = ResolveExpr( f.initValue, expectedFieldType );

        if( !ConvertType( &field, expectedFieldType ) )
        {
            // For buffers, if the first field is not a pointer, try again as an array literal instead
            if( type->kind == Type::Buffer && !parseArrayLiteral )
            {
                parseArrayLiteral = true;
                continue;
            }

            RSLV_ERROR( f.pos, "No implicit conversion available in compound literal field (expected '%s', got '%s')",
                        expectedFieldType->name, field.type->name );
        }

        index++;
    }

    if( flexibleArray || parseArrayLiteral )
    {
        // Substitute type by a new one with correct sizing info (need to fetch any cached types again)
        type = NewArrayType( type->array.base, maxIndex );
    }

    return NewResolvedRvalue( type );
}

FuncArg* ResolveArgNameExpr( Type* funcType, Array<FuncArg>& wantedArgs, Expr* nameExpr )
{
    ASSERT( funcType->kind == Type::Func );

    Decl* funcDecl = funcType->func.resolvedDecl;
    /*
    // TODO There's an issue here with named args which is not quite solved
    Func types don't contain arg names, as we don't consider them part of a func type definition (two different functions providing
    different names but matching types for their arguments should have the same type as they can both be called with the same call
    expression in most situations). We could break this equivalence and introduce arg names as part of the type, but it just doesn't seem
    to make sense (we would get unresolved type errors for functions that seem like they should match?).

    We do have some cases where args can only be called by name though, so we _require_ knowing the names in those situations.
    So for now we annotate the resolved func type with the source declaration in those places where it is available, but this obviously
    doesn't solve all situations. For the remaining situations where the func decl is not available, we just show the normal
    "unknown argument" error.
    */
    if( funcDecl )
    {
        Array<FuncArg>& args = funcType->func.args;
        ASSERT( wantedArgs.count == args.count );
        Array<FuncArgDecl> const& declArgs = funcDecl->func.args;
        ASSERT( declArgs.count == args.count );

        for( int i = 0; i < wantedArgs.count; ++i )
        {
            if( declArgs[i].name == nameExpr->name.ident )
                return &wantedArgs[i];
        }
    }

    return nullptr;
}

void PrintMissingArgsInfo( Type* funcType, Array<FuncArg> const& wantedArgs, SourcePos const& pos )
{
    ASSERT( funcType->kind == Type::Func );

    Decl* funcDecl = funcType->func.resolvedDecl;
    if( funcDecl )
    {
        Array<FuncArgDecl> const& declArgs = funcDecl->func.args;
        ASSERT( declArgs.count == wantedArgs.count );

        for( int i = 0; i < wantedArgs.count; ++i )
        {
            FuncArg const& arg = wantedArgs[i];

            // Hasn't been yet crossed out
            if( arg.type != nullptr && arg.defaultValue == nullptr && !arg.isVararg )
            {
                char const* name = declArgs[i].name;
                RSLV_INFO( pos, "Argument '%s' is not optional", name );
            }
        }
    }
}

ResolvedExpr ResolveCallExpr( Expr* expr )
{
    ASSERT( expr->kind == Expr::Call );
    ResolvedExpr resolvedFunc = ResolveExpr( expr->call.func );
    if( !IsValid( resolvedFunc ) )
        return resolvedNull;

    Type* funcType = resolvedFunc.type;
    CompleteType( funcType, expr->pos );

    if( funcType->kind != Type::Func )
    {
        RSLV_ERROR( expr->pos, "Cannot call non-function value" );
        return resolvedNull;
    }

    /*
       Rules for function arguments:
       - Only one vararg for now. Any args after the vararg can only be passed by name
       - Any number of optionals, but if any is ommitted in the call, args after that one can only be passed by name
       - After a named arg appears in the call, all args after it must be named too
       // TODO Test all these combinations thoroughly
       // TODO Add any corresponding checks during parsing/resolving of lambda decls
    */
    int givenIndex = 0, wantedIndex = 0;
    bool requireNamed = false;

    Array<ArgExpr> const& givenArgs = expr->call.args;
    // Copy target func type args array and cross out args as they're found in the call
    Array<FuncArg> wantedArgs = funcType->func.args.Clone( &globalTmpArena );

    FuncArg* wantedArg = nullptr;
    while( givenIndex < givenArgs.count && wantedIndex < wantedArgs.count )
    {
        wantedArg = &wantedArgs[wantedIndex];

        ArgExpr const& givenArg = givenArgs[givenIndex];
        if( givenArg.nameExpr )
        {
            wantedArg = ResolveArgNameExpr( funcType, wantedArgs, givenArg.nameExpr );
            if( wantedArg == nullptr )
            {
                RSLV_ERROR( givenArg.nameExpr->pos, "Unknown argument '%s' in function call", givenArg.nameExpr->name.ident );
                return resolvedNull;
            }
            else if( wantedArg->type == nullptr )
            {
                RSLV_ERROR( givenArg.nameExpr->pos, "Passing argument '%s' twice in function call", givenArg.nameExpr->name.ident );
                return resolvedNull;
            }
            requireNamed = true;
        }
        else if( requireNamed )
        {
            RSLV_ERROR( givenArg.expr->pos, "Only named arguments allowed after a named argument is given" );
            return resolvedNull;
        }

        Type* wantedArgType = wantedArg->type;
        ASSERT( wantedArgType, "This argument has already been given!" );

        // Use the base type for varargs
        if( wantedArg->isVararg && wantedArgType->kind == Type::Buffer )
            wantedArgType = wantedArgType->array.base;

        ResolvedExpr resolvedArg = ResolveExpr( givenArg.expr, wantedArgType );
        if( !ConvertType( &resolvedArg, wantedArgType ) )
        {
            RSLV_ERROR( givenArg.expr->pos, "No implicit conversion available in function call argument (expected '%s', got '%s')",
                        wantedArgType->name, resolvedArg.type->name );
            return resolvedNull;
        }

        givenIndex++;
        // If it's a vararg, keep swallowing the next arg
        if( !wantedArg->isVararg )
        {
            // Otherwise mark it as done
            wantedArg->type = nullptr;
            wantedIndex++;
        }
    }
    // If the last expected argument was a vararg, consider that one done
    if( wantedArg && wantedArg->isVararg )
    {
        wantedArg->type = nullptr;
        wantedIndex++;
    }

    if( wantedIndex < wantedArgs.count )
    {
        bool argsMissing = false;

        // Check if all remaining args are optional
        for( FuncArg const& arg : wantedArgs )
        {
            if( arg.type != nullptr && arg.defaultValue == nullptr && !arg.isVararg )
            {
                argsMissing = true;
                break;
            }
        }

        if( argsMissing )
        {
            SourcePos const& pos = givenArgs.Last().expr->pos;
            RSLV_ERROR( pos, "Missing arguments in function call (expected %d, got %d)", wantedArgs.count, givenArgs.count );
            PrintMissingArgsInfo( funcType, wantedArgs, pos );

            return resolvedNull;
        }
    }
    if( givenIndex < givenArgs.count )
    {
        RSLV_ERROR( givenArgs[givenIndex].expr->pos, "Too many arguments in function call (expected %d, got %d)",
                    wantedArgs.count, givenArgs.count );
        return resolvedNull;
    }

    return NewResolvedRvalue( funcType->func.returnType );
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

    if( base.type->kind == Type::Array && index.isConst )
    {
        i64 idxValue = index.constValue.intValue;
        if( idxValue < 0 || idxValue >= base.type->array.length )
            RSLV_ERROR( expr->pos, "Array index out of bounds" );
    }

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

char const* BuildAndInternQualifiedName( Array<char const*> const& names )
{
    StringBuilder sb( &globalTmpArena );
    for( char const* n : names )
    {
        if( n != names[0] )
            sb.Append( "." );

        sb.Append( n );
    }

    InternString* internName = Intern( sb.ToString( &globalTmpArena ) );
    return internName->data;
}

Type* ResolveTypeSpec( TypeSpec* spec, Symbol* parentSymbol = nullptr )
{
    Type* result = nullptr;
    SourcePos const& pos = spec->pos;

    switch( spec->kind )
    {
        case TypeSpec::Name:
        {
            ASSERT( spec->names.count );

            char const* name = spec->names.First();
            // TODO This should probably just come like this from the parser
            if( spec->names.count > 1 )
                name = BuildAndInternQualifiedName( spec->names );

            Symbol* sym = ResolveName( name, pos, parentSymbol );
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
            Type* base = ResolveTypeSpec( spec->ptr.base, parentSymbol );
            result = NewPtrType( base );
        } break;
        case TypeSpec::Array:
        {
            i64 length = 0;

            Expr* lengthExpr = spec->array.length;
            if( lengthExpr )
            {
                length = ResolveConstExprInt( lengthExpr );
                // NOTE Zero-sized arrays not supported.
                // We use length == 0 to indicate an initializer is expected to figure out the actual size
                // TODO Add support for unknown size arrays without an initializer at the end of structs
                if( length <= 0 )
                {
                    RSLV_ERROR( pos, "Array size must be a positive integer" );
                    //return nullptr;
                }
            }
            Type* base = ResolveTypeSpec( spec->array.base, parentSymbol );
            if( spec->array.isView )
                result = NewBufferType( base );
            else
                result = NewArrayType( base, length );
        } break;
        case TypeSpec::Func:
        {
            Array<FuncArg> args( &globalArena, spec->func.args.count );
            for( FuncArgSpec const& argSpec : spec->func.args )
            {
                Type* argType = ResolveTypeSpec( argSpec.type );
                if( argSpec.isVararg )
                    argType = NewBufferType( argType );
                if( argSpec.defaultValue )
                    ResolveExpr( argSpec.defaultValue, argType );
                args.Push( { argType, argSpec.defaultValue, argSpec.isVararg } );
            }
            Array<Type*> returnTypes( &globalArena, spec->func.returnTypes.count );
            for( TypeSpec* ts : spec->func.returnTypes )
            {
                Type* ret = ResolveTypeSpec( ts );
                returnTypes.Push( ret );
            }

            Type* returnType = voidType;
            if( returnTypes.count == 1 )
                returnType = returnTypes[0];
            else if( returnTypes.count > 1 )
            {
                returnType = NewMultiType( returnTypes );
                globalForwardTypes.Push( returnType );
            }

            result = NewFuncType( args, returnType );
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

    Expr* lowerBoundExpr = expr->range.lowerBound;
    Expr* upperBoundExpr = expr->range.upperBound;
    ResolvedExpr lowerBound = {}, upperBound = {};

    // Resolve as the type of the range's elements
    Type* resolvedType = nullptr;

    if( lowerBoundExpr )
    {
        lowerBound = ResolveExpr( lowerBoundExpr );
        // TODO Shouldn't allow bits here?
        if( !IsScalarType( lowerBound.type ) )
            RSLV_ERROR( expr->pos, "Lower bound in range expression must be a scalar type" );
    }

    if( upperBoundExpr )
    {
        upperBound = ResolveExpr( upperBoundExpr );
        if( upperBound.type->kind == Type::Array || upperBound.type->kind == Type::Buffer )
        {
            if( lowerBoundExpr )
                RSLV_ERROR( expr->pos, "Ranged buffer expression cannot have a lower bound" );

            resolvedType = upperBound.type->array.base;
        }
        else
        {
            if( !lowerBoundExpr )
                RSLV_ERROR( expr->pos, "Non-iterator range expression must have a lower bound" );

            if( !ConvertType( &upperBound, lowerBound.type ) )
                RSLV_ERROR( expr->pos, "Upper bound and lower bound in scalar range expression must have the same type" );

            resolvedType = upperBound.type;
        }
    }

    ResolvedExpr result = NewResolvedRvalue( resolvedType );
    result.isConst = (lowerBoundExpr == nullptr && upperBoundExpr == nullptr) || (lowerBound.isConst && upperBound.isConst);

    return result;
}

INLINE bool IsForeign( Decl* decl )
{
    return ContainsDirective( decl, Directive::Foreign );
}

Type* ResolveFuncDecl( Decl* decl )
{
    ASSERT( decl->kind == Decl::Func );

    Array<FuncArg> args( &globalArena, decl->func.args.count );
    for( FuncArgDecl const& a : decl->func.args )
    {
        Type* argType = ResolveTypeSpec( a.type );
        if( a.isVararg && !IsForeign( decl ) )
            argType = NewBufferType( argType );
        CompleteType( argType, a.pos );
        if( a.defaultValue )
            ResolveExpr( a.defaultValue, argType );

        args.Push( { argType, a.defaultValue, a.isVararg } );
    }

    Array<Type*> returnTypes( &globalArena, decl->func.returnTypes.count );
    for( TypeSpec* ts : decl->func.returnTypes )
    {
        Type* retType = voidType;
        if( ts )
        {
            retType = ResolveTypeSpec( ts );
            CompleteType( retType, ts->pos );
        }

        returnTypes.Push( retType );
    }

    Type* returnType = voidType;
    if( returnTypes.count == 1 )
        returnType = returnTypes[0];
    else if( returnTypes.count > 1 )
    {
        returnType = NewMultiType( returnTypes );
        globalForwardTypes.Push( returnType );
    }

    Type* funcType = NewFuncType( args, returnType );
    return funcType;
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

bool ResolveStmtBlock( StmtList const* block, Type* returnType );
Array<Symbol*> CreateDeclSymbols( Decl* decl, bool isLocal, Symbol* parentSymbol = nullptr );

void ResolveFuncBody( Decl* decl )
{
    // FIXME How do we mark this body as fully resolved so we don't ever do this more than once?

    ASSERT( decl->kind == Decl::Func );
    Type* type = decl->resolvedType;

    auto scopeIdx = EnterScope();

    int i = 0;
    for( FuncArgDecl const& a : decl->func.args )
    {
        Type* argType = type->func.args[i].type;
        CompleteType( argType, a.pos );

        if( a.name )
        {
            // Create a synthetic Decl
            TypeSpec* varType = a.type;
            if( a.isVararg )
                varType = NewArrayTypeSpec( a.pos, varType, nullptr, true );
            Decl* varDecl = NewVarDecl( a.pos, a.name, varType, nullptr, BucketArray<NodeDirective>::Empty, 0, decl->func.body );
            CreateDeclSymbols( varDecl, true );
        }
    }

    ASSERT( type->func.returnType && type->func.returnType->kind > Type::Completing );

    if( !IsForeign( decl ) )
    {
        bool returns = ResolveStmtBlock( decl->func.body, type->func.returnType );
        if( !returns && type->func.returnType != voidType )
            RSLV_ERROR( decl->pos, "Not all control paths return a value" );
    }

    LeaveScope( scopeIdx );
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
            if( !CompleteType( type, expr->pos ) )
                return resolvedNull;

            result = NewResolvedConst( intType, TypeSize( type ), expr->pos );
        } break;
        // TODO Typeof

        case Expr::Comma:
        {
            ASSERT( expr->kind == Expr::Comma );

            Array<Type*> expectedTypes( &globalArena, expr->commaExprs.count );
            expectedTypes.ResizeToCapacity();

            if( expectedType )
            {
                ASSERT( expectedType->kind == Type::Multi );
                for( int i = 0; i < expectedType->multi.types.count; ++i )
                    expectedTypes[i] = expectedType->multi.types[i];
            }

            int i =  0;
            bool isLvalue = true, typeMismatch = false;
            Array<ResolvedExpr> resolvedExprs( &globalTmpArena, expr->commaExprs.count );

            for( Expr* e : expr->commaExprs )
            {
                // NOTE Can be null
                expectedType = expectedTypes[i];

                ResolvedExpr resolved = ResolveExpr( e, expectedType );
                resolvedExprs.Push( resolved );

                if( expectedType && !ConvertType( &resolved, expectedType ) )
                {
                    RSLV_ERROR( e->pos, "No implicit conversion available for expression %d in comma expression (expected '%s', got '%s')",
                                i + 1, expectedType->name, resolved.type->name );
                }
                e->resolvedExpr = resolved;
                expectedTypes[i] = resolved.type;

                isLvalue = isLvalue && resolved.isLvalue;
                i++;
            }

            // NOTE Any ConstValues are not correctly reflected here (they're available in the individual resolvedExprs though)
            result = NewResolvedRvalue( NewMultiType( expectedTypes ) );
            // It's an l-value if all sub-expressions were too
            result.isLvalue = isLvalue;
        } break;

        case Expr::Lambda:
        {
            Decl* decl = expr->lambda.decl;
            ASSERT( decl->kind == Decl::Func );

            Type* funcType = ResolveFuncDecl( decl );
            // Remember the decl so we can access arg names
            funcType->func.resolvedDecl = decl;

            decl->resolvedType = funcType;
            ResolveFuncBody( decl );

            result = NewResolvedRvalue( funcType );
        } break;

        INVALID_DEFAULT_CASE
    }

    ASSERT( result.type );
    expr->resolvedExpr = result;

    return result;
}

void CompleteStructType( Type* type, Array<TypeField>& fields )
{
    if( !IsValid( type ) )
        return;

    ASSERT( type->kind == Type::Completing );
    type->kind = Type::Struct;
    type->size = 0;
    type->align = 0;

    sz totalFieldsSize = 0;
    for( TypeField& f : fields )
    {
        if( !IsValid( f.type ) )
            continue;

        ASSERT( f.type->kind > Type::Completing );
        ASSERT( IsPowerOf2( f.type->align ) );

        // TODO Unnamed aggregate members
        f.offset = type->size;

        totalFieldsSize += f.type->size;
        type->align = Max( type->align, f.type->align );
        type->size = f.type->size + AlignUp( type->size, f.type->align );
    }
    type->size = AlignUp( type->size, type->align );
    //type->padding = type->size - totalFieldsSize;

    type->aggregate.fields = fields;
}

void CompleteUnionType( Type* type, Array<TypeField>& fields )
{
    if( !IsValid( type ) )
        return;

    ASSERT( type->kind == Type::Completing );
    type->kind = Type::Union;
    type->size = 0;
    type->align = 0;
    
    for( TypeField& f : fields )
    {
        if( !IsValid( f.type ) )
            continue;

        ASSERT( f.type->kind > Type::Completing );
        ASSERT( IsPowerOf2( f.type->align ) );

        // TODO Unnamed aggregate members

        type->size = Max( type->size, f.type->size );
        type->align = Max( type->align, f.type->align );
    }
    type->size = AlignUp( type->size, type->align );

    type->aggregate.fields = fields;
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

ResolvedExpr ZeroValue( Type* type, SourcePos const& pos )
{
    switch( type->kind )
    {
        case Type::Int:
        return NewResolvedConst( type, (i64)0, pos );
        case Type::Bits:
        return NewResolvedConst( type, 0ull, pos );
        case Type::Bool:
        return NewResolvedConst( type, false );

        default:
        INVALID_CODE_PATH;
        return resolvedNull;
    }
}

void IncValue( ResolvedExpr* value )
{
    // TODO Handle overflow
    switch( value->type->kind )
    {
        case Type::Int:
        case Type::Bool:
        value->constValue.bitsValue++;
        break;
        case Type::Bits:
        value->constValue.bitsValue = value->constValue.bitsValue
            ? value->constValue.bitsValue * 2
            : 1;
        break;

        default:
        INVALID_CODE_PATH;
    }
}

Expr* NewInitExpr( ResolvedExpr const& value, SourcePos const& pos )
{
    Expr* result = NewExpr( pos, Expr::Int );
    switch( value.type->kind )
    {
        case Type::Int:
        {
            result->literal.intValue = value.constValue.intValue;
        } break;
        case Type::Bits:
            // TODO 
            result->literal.modifier = Token::Hexadecimal;
            // Fallthrough
        case Type::Bool:
        {
            result->literal.intValue = (i64)value.constValue.bitsValue;
        } break;

        default:
        INVALID_CODE_PATH;
    }

    result->resolvedExpr = value;
    return result;
}

EnumItem NewEnumItem( SourcePos const& pos, char const* name, Expr* initExpr )
{
    EnumItem item = {};
    item.pos = pos;
    item.name = name;
    item.initExpr = initExpr;

    return item;
}

bool CompleteType( Type* type, SourcePos const& pos )
{
    if( !type || type->kind == Type::None )
        return false;

    ASSERT( type->kind );

    if( type->kind == Type::Completing )
    {
        // TODO Better message
        RSLV_ERROR( pos, "Circular dependency detected" );
        type->kind = Type::None;
        return false;
    }
    else if( type->kind != Type::Incomplete )
    {
        // Nothing to do
        return true;
    }

    type->kind = Type::Completing;
    ASSERT( type->symbol && type->symbol->decl );

    Decl* decl = type->symbol->decl;
    if( decl->kind == Decl::Enum )
    {
        type->kind = Type::Enum;
        // Underlying data is just the index
        type->size = 4;
        type->align = 4;

        Type* valueType = i32Type;
        if( decl->enum_.type )
        {
            valueType = ResolveTypeSpec( decl->enum_.type );
            CompleteType( valueType, pos );
        }
        type->enum_.base = valueType;

        if( valueType == voidType )
        {
            RSLV_ERROR( decl->pos, "Cannot declare enum with a void base type" );
            type->kind = Type::None;
            return false;
        }

        // TODO Enum bools?
        bool isBits = valueType->kind == Type::Bits;
        bool isInteger = IsIntegerType( valueType );
        bool isIntegral = IsIntegralType( valueType );

        ResolvedExpr currentValue = { nullptr };
        if( isIntegral )
            currentValue = ZeroValue( valueType, decl->pos );
        Expr* initExpr = NewInitExpr( currentValue, decl->pos );

        INIT( type->enum_.items ) Array<EnumItem>( &globalArena, decl->enum_.items.count + 1 );
        type->enum_.items.Push( NewEnumItem( decl->pos, "None", initExpr ) );

        if( isIntegral )
            IncValue( &currentValue );

        for( EnumItem const& it : decl->enum_.items )
        {
            if( StringsEqualNoCase( it.name, "none" ) )
            {
                RSLV_ERROR( it.pos, "'None' item is reserved" );
                continue;
            }

            initExpr = it.initExpr;
            if( !isIntegral && !initExpr )
                RSLV_ERROR( it.pos, "Non integral enum item must be given a value" );

            if( initExpr )
            {
                // TODO Self-referring items
                ResolvedExpr init = ResolveExpr( initExpr, valueType );

                if( IsValid( init ) )
                {
                    if( !ConvertType( &init, valueType ) )
                        RSLV_ERROR( it.pos, "No implicit conversion available in initializer expression. Expected '%s', got '%s'",
                                    valueType->name, init.type->name );

                    if( IsScalarType( valueType ) )
                    {
                        if( !init.isConst )
                            RSLV_ERROR( it.pos, "Initializer expression for scalar enum item must be constant" );
                    }
                    else
                    {
                        // TODO Ensure complex type initializer is constant !?
                        // TODO Do we need to extend ConstValue so it can express a constant of any type including complex ones?
                        // This can include:
                        // - arrays (& buffers?)
                        // - structs & unions
                        // - functions
                        // - any! (this would have to ref other constant values?)
                    }
                }
                currentValue = init;
            }
            else
                initExpr = NewInitExpr( currentValue, it.pos );

            type->enum_.items.Push( NewEnumItem( it.pos, it.name, initExpr ) );

            if( isIntegral )
                IncValue( &currentValue );
        }
    }
    else
    {
        ASSERT( decl->kind == Decl::Struct || decl->kind == Decl::Union );
        bool isStruct = decl->kind == Decl::Struct;

        bool hasInit = false;
        // FIXME Need a temporary array here, as we're potentially going to be adding unnamed stuff to this
        Array<TypeField> fields( &globalArena, CountAggregateItems( decl->aggregate.items ) );
        int itemIndex = 0;
        for( Decl* d : decl->aggregate.items )
        {
            // Attributes
            if( d->kind == Decl::Var )
            {
                if( !d->var.type )
                {
                    RSLV_ERROR( d->pos, "Aggregate field declarations must specify a type" );
                    continue;
                }

                Type* fieldType = ResolveTypeSpec( d->var.type, type->symbol );

                if( CompleteType( fieldType, d->pos ) && TypeSize( fieldType ) == 0 )
                {
                    if( fieldType->kind == Type::Array )
                    {
                        if( isStruct && itemIndex != decl->aggregate.items.count - 1 )
                            RSLV_ERROR( d->pos, "Zero-sized array can only be declared as the last element of a struct" );
                    }
                    else
                        RSLV_ERROR( d->pos, "Aggregate field of size 0 is not allowed" );
                }

                Expr* baseInitExpr = d->var.initExpr;
                if( !isStruct && hasInit && baseInitExpr != nullptr )
                    RSLV_ERROR( d->pos, "Only one field initializer is allowed in unions" );

                hasInit |= baseInitExpr != nullptr;
                // If there are several names, create a separate field for each one
                int i = 0;
                for( char const* name : d->names )
                {
                    Expr* initExpr = baseInitExpr;
                    ResolvedExpr init = resolvedNull;

                    if( initExpr && initExpr->kind == Expr::Comma )
                        initExpr = (i < initExpr->commaExprs.count ) ? initExpr->commaExprs[i] : nullptr;

                    if( initExpr )
                    {
                        init = ResolveExpr( initExpr, fieldType );
                        if( IsValid( init ) && !ConvertType( &init, fieldType ) )
                            RSLV_ERROR( pos, "No implicit conversion available in initializer expression. Expected '%s', got '%s'",
                                        fieldType->name, init.type->name );

                        //fieldType = init.type;
                    }

                    fields.Push( { name, fieldType, init, 0 } );
                    i++;
                }

                // FIXME Use a Multi if there are several names
                d->resolvedType = fieldType;
            }
            else 
            {
                // Sub-aggregates have been given their own symbol,
                // so should resolve through the same dependency-directed process as anything else
            }

            itemIndex++;
        }
        type->aggregate.fields = fields;

        if( isStruct )
            CompleteStructType( type, fields );
        else
            CompleteUnionType( type, fields );
    }

    decl->resolvedType = type;

    if( !type->symbol->isLocal )
        globalOrderedSymbols.Push( type->symbol );

    return true;
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

void ResolveSymbol( Symbol* sym )
{
    if( sym->state == Symbol::Resolved || sym->state == Symbol::Tainted )
        // Nothing to do
        return;

    Decl* decl = sym->decl;
    if( ContainsDirective( decl, Directive::DebugBreak ) )
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
                if( result && IsValid( init.type ) && !ConvertType( &init, result ) )
                {
                    // Make an exception for empty-size arrays
                    if( result->kind != Type::Array || result->array.length != 0 )
                    {
                        RSLV_ERROR( pos, "No implicit conversion available in initializer expression. Expected '%s', got '%s'",
                                    result->name, init.type->name );
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
            NOT_IMPLEMENTED;
        } break;

        case Symbol::Func:
        {
            result = ResolveFuncDecl( decl );
            // Remember the decl so we can access arg names
            result->func.resolvedDecl = decl;
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

void CompleteSymbol( Symbol* sym )
{
    ResolveSymbol( sym );

    if( sym->kind == Symbol::Type )
        CompleteType( sym->type, sym->decl->pos );
    else if( sym->kind == Symbol::Func )
    {
        ASSERT( sym->state == Symbol::Resolved );

        Decl* decl = sym->decl;
        ASSERT( decl->resolvedType == sym->type );

        ResolveFuncBody( decl );
    }
}

void BuildQualifiedNameTmp( StringBuilder* sb, Symbol* parentSymbol, char const* name )
{
    if( parentSymbol )
    {
        BuildQualifiedNameTmp( sb, parentSymbol->parent, parentSymbol->name );
        sb->Append( "." );
    }

    sb->Append( name );
}

char const* BuildAndInternQualifiedName( Symbol* parentSymbol, char const* name )
{
    StringBuilder sb( &globalTmpArena );
    BuildQualifiedNameTmp( &sb, parentSymbol, name );

    InternString* internName = Intern( sb.ToString( &globalTmpArena ) );
    return internName->data;
}

Symbol* ResolveName( char const* name, SourcePos const& pos, Symbol* parentSymbol /*= nullptr*/ )
{
    char const* fullName = name;

    // FIXME Make a fast path for builtin symbols that skips this
    // FIXME Only do this if the parentSymbol is an aggregate with any defined sub-aggregates

    // If there's a parent symbol, try first prepending its fully qualified name, as that would take precedence
    if( parentSymbol )
        fullName = BuildAndInternQualifiedName( parentSymbol, name );

    Symbol* sym = GetSymbol( fullName );
    if( sym )
        ResolveSymbol( sym );
    else
    {
        if( parentSymbol )
        {
            // Try again with just the name
            sym = GetSymbol( name );
            if( sym )
                ResolveSymbol( sym );
        }
    }

    if( !sym )
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
    if( ContainsDirective( node, Directive::ExpectError ) )
        RSLV_ERROR( node->pos, "Expected error but none was generated" );
}

bool ResolveStmt( Stmt* stmt, Type* returnType )
{
    bool returns = false;

    if( ContainsDirective( stmt, Directive::DebugBreak ) ||
        (stmt->kind == Stmt::Decl && ContainsDirective( stmt->decl, Directive::DebugBreak )) )
        __debugbreak();

    PushNode( stmt );

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

            if( stmt->decl->kind == Decl::Enum )
                globalForwardDecls.Push( stmt->decl );
        } break;
        case Stmt::Assign:
        {
            ResolvedExpr left = ResolveExpr( stmt->assign.left );

            if( stmt->assign.left->kind != Expr::Comma )
            {
                if( !left.isLvalue )
                {
                    RSLV_ERROR( stmt->assign.left->pos, "Expression cannot be assigned to (not an l-value)" );
                    break;
                }

                ResolvedExpr right = ResolveExpr( stmt->assign.right, left.type );
                TokenKind::Enum op = stmt->assign.op;
                TokenKind::Enum binaryOp = assignOpToBinaryOp[ stmt->assign.op ];

                ResolvedExpr result = {};
                if( op == TokenKind::Assign )
                    result = right;
                else
                    result = ResolveBinaryExpr( binaryOp, TokenKind::names[ op ], &left, &right, stmt->pos );

                if( !ConvertType( &result, left.type ) )
                {
                    RSLV_ERROR( stmt->pos, "No implicit conversion available in assignment (expected '%s', got '%s')",
                                left.type->name, right.type->name );
                }
            }
            else
            {
                ASSERT( left.type->kind == Type::Multi );
                Array<Type*> const& leftTypes = left.type->multi.types;

                if( !left.isLvalue )
                {
                    int i = 0;
                    for( Expr* e : stmt->assign.left->commaExprs )
                    {
                        if( !e->resolvedExpr.isLvalue )
                            RSLV_ERROR( e->pos, "Expression %d in comma expression cannot be assigned to (not an l-value)", i + 1 );
                        i++;
                    }
                    break;
                }

                // NOTE For now we require a comma expr on the right too
                if( stmt->assign.right->kind != Expr::Comma )
                {
                    RSLV_ERROR( stmt->assign.right->pos, "The expression assigned to a comma expression must be a comma expression" );
                    break;
                }

                Array<Expr*> const& leftExprs = stmt->assign.left->commaExprs;
                Array<Expr*> const& rightExprs = stmt->assign.right->commaExprs;

                if( leftExprs.count != rightExprs.count )
                {
                    RSLV_ERROR( stmt->pos, "Assignment operation for comma expression must have the same number of terms on both sides (left has %d, right has %d)",
                                leftExprs.count, rightExprs.count );
                    break;
                }

                ResolvedExpr right = ResolveExpr( stmt->assign.right, left.type );

                TokenKind::Enum op = stmt->assign.op;
                TokenKind::Enum binaryOp = assignOpToBinaryOp[ stmt->assign.op ];
                char const* opName = TokenKind::names[ op ];

                Array<ResolvedExpr> result = Array<ResolvedExpr>( &globalTmpArena, leftExprs.count );
                for( int i = 0; i < leftExprs.count; ++i )
                {
                    if( op == TokenKind::Assign )
                        result.Push( right );
                    else
                        result.Push( ResolveBinaryExpr( binaryOp, opName, &leftExprs[i]->resolvedExpr, &rightExprs[i]->resolvedExpr,
                                                        leftExprs[i]->pos ) );
                }
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
            Decl* varDecl = NewVarDecl( stmt->pos, stmt->for_.indexName, nullptr, rangeExpr, BucketArray<NodeDirective>::Empty, 0,
                                        stmt->for_.block );
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
                    RSLV_ERROR( stmt->expr->pos, "No implicit conversion available in return expression (expected '%s', got '%s')",
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

char const* BuildAndInternQualifiedName( char const* prefix, char const* name )
{
    StringBuilder sb( &globalTmpArena );
    sb.AppendFmt( "%s.%s", prefix, name );

    InternString* internName = Intern( sb.ToString( &globalTmpArena ) );
    return internName->data;
}

Array<Symbol*> CreateDeclSymbols( Decl* decl, bool isLocal, Symbol* parentSymbol /*= nullptr*/ )
{
    ASSERT( decl->names );

    if( ContainsDirective( decl, Directive::DebugBreak ) )
        __debugbreak();

    Symbol::Kind kind = Symbol::None;
    switch( decl->kind )
    {
        case Decl::Var:
            kind = decl->var.isConst ? Symbol::Const : Symbol::Var;
            break;
        case Decl::Func:
            kind = Symbol::Func;
            break;
        case Decl::Struct:
        case Decl::Union:
        case Decl::Enum:
            kind = Symbol::Type;
            break;

        INVALID_DEFAULT_CASE;
    }

    BucketArray<Symbol*> result( &globalTmpArena, decl->names.count );
    for( char const* name : decl->names )
    {
        if( parentSymbol )
            name = BuildAndInternQualifiedName( parentSymbol->name, name );

        Symbol* sym = PushSymbol( name, isLocal, decl->pos );
        if( sym )
        {
            sym->decl = decl;
            sym->name = name;
            sym->kind = kind;
            sym->parent = parentSymbol;
            sym->isLocal = isLocal;
            sym->state = Symbol::Unresolved;

            if( decl->kind == Decl::Struct || decl->kind == Decl::Union || decl->kind == Decl::Enum )
            {
                sym->state = Symbol::Resolved;
                sym->type = NewIncompleteType( name, sym );

                // Add symbols for declared sub-aggregates
                if( decl->kind != Decl::Enum )
                {
                    for( Decl* inner : decl->aggregate.items )
                    {
                        if( inner->kind != Decl::Var )
                        {
                            Array<Symbol*> innerSymbols = CreateDeclSymbols( inner, isLocal, sym );
                            result.Append( innerSymbols );
                        }
                    }
                }
            }

            result.Push( sym );
        }
    }

    return result.ToArray( &globalTmpArena );
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
    return result;
}

Symbol* PushGlobalConstSymbol( char const* name, Type* type, u64 bitsValue )
{
    InternString* internName = Intern( String( name ) );
    // Use interned string for both key and value!
    name = internName->data;

    Symbol sym = {};
    sym.name = name;
    sym.type = type;
    sym.kind = Symbol::Const;
    sym.state = Symbol::Resolved;
    sym.constValue.bitsValue = bitsValue;

    Symbol* result = globalSymbols.Put( name, sym );
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
    return result;
}

void InitResolver( int globalSymbolsCount )
{
    // NOTE These store the symbols
    // We want to make the global symbols hashtable be a fixed size so we count how many total symbols there are
    const int builtinConstsAndFuncs = 4;
    INIT( globalSymbols ) Hashtable<char const*, Symbol, MemoryArena>( &globalArena,
        globalSymbolsCount + I32( ARRAYCOUNT(globalBuiltinTypes) ) + builtinConstsAndFuncs, HTF_FixedSize );

    INIT( globalScopeStack ) BucketArray<Symbol>( &globalArena, 1024 );
    // TODO This is not used ??
    //globalCurrentScopeStart = globalScopeStack.First();

    // These refer to symbols in one of the previous tables
    INIT( globalSymbolsList ) BucketArray<Symbol*>( &globalArena, 256 );
    INIT( globalOrderedSymbols ) BucketArray<Symbol*>( &globalArena, 256 );
    INIT( globalNodeStack ) BucketArray<Stmt*>( &globalArena, 16 );

    // TODO Make a generic type comparator so we can turn this into a hashtable
    INIT( globalCachedTypes ) BucketArray<Type*>( &globalArena, 256 );

    INIT( globalForwardDecls ) BucketArray<Decl*>( &globalArena, 16 );
    INIT( globalForwardTypes ) BucketArray<Type*>( &globalArena, 16 );

    InitErrorBuffer();

    // Push symbols for builtin types
    // TODO We should have a table for builtins like we do for keywords, mark their name exprs in the parser with an index
    // into the table so resolving them is trivial
    for( BuiltinType& b : globalBuiltinTypes )
    {
        if( IsNullOrEmpty( b.symbolName ) )
            break;

        if( b.isAlias )
            PushGlobalTypeSymbol( b.symbolName, b.aliasType );
        else
            PushGlobalTypeSymbol( b.symbolName, &b.type );
    }

    PushGlobalConstSymbol( "true", boolType, 1u );
    PushGlobalConstSymbol( "false", boolType, 0u );
    PushGlobalConstSymbol( "null", NewPtrType( voidType ), 0u );

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
