internal MemoryArena* outArena = &globalOutArena;

internal sz globalIndent = 0;
internal SourcePos globalPos = {};

internal BucketArray<Decl*> globalInnerFuncs;


INLINE void Out( char const* str, sz len = 0 )
{
    // TODO Making symbol names use the full InternString would reduce the need for this a lot
    if( !len )
        len = StringLength( str );
    char* buf = PUSH_STRING( outArena, len );
    PCOPY( str, buf, len );
}

#undef OUT
#define OUTSTR( s ) Out( "" s "", sizeof(s) - 1 )
#define INDENT_STR "                                                                            "

internal INLINE void OutIndent()
{
    ASSERT( globalIndent * 4 < sizeof(INDENT_STR) - 1 );
    if( globalIndent )
        Out( INDENT_STR, globalIndent * 4 );
}

internal INLINE void OutNL()
{
    Out( "\n" );
    globalPos.lineNumber++;
}

char* Strf( char const* fmt, ... )
{
    va_list args;
    va_start( args, fmt );
    // TODO Use an Append function with support for adding one string/int/etc. at a time to remove the need for sprintf at all
    sz n = 1 + vsnprintf( nullptr, 0, fmt, args );
    va_end( args );

    char* buf = PUSH_STRING( &globalTmpArena, n );
    va_start( args, fmt );
    vsnprintf( buf, Size( n ), fmt, args );
    va_end( args );

    return buf;
}

internal void EmitPos( SourcePos const& pos )
{
    if( pos.lineNumber != globalPos.lineNumber || pos.filename != globalPos.filename )
    {
        Out( Strf( "#line %d", pos.lineNumber ) );
        if( pos.filename != globalPos.filename )
        {
            OUTSTR( " " );
            // TODO
            Out( Strf( "\"%s\"", pos.filename ) );
        }
        OutNL();
        globalPos = pos;
    }
}

char const* BuildQualifiedNameTmp( StmtList* parentBlock, char const* name )
{
    StringBuilder sb( &globalTmpArena );
    sb.AppendFmt( "%s_%s", parentBlock->globalPath.data, name );
    return sb.ToString( &globalTmpArena ).data;
}

// NOTE Only uses first name in decl
char const* BuildQualifiedNameTmp( Decl* decl )
{
    char const* name = decl->names[0];
    // Do we actually need to qualify the name?
    if( decl->parentBlock )
        name = BuildQualifiedNameTmp( decl->parentBlock, name );

    return name;
}

char const* CdeclType( Type* type )
{
    switch( type->kind )
    {
        case Type::Void:
        case Type::Bool:
        case Type::String:
            return type->name;
        // Use the sized versions for all integral and arithmetic types
        case Type::Bits:
            type = (type == bitsType) ? b32Type : type;
            return type->name;
        case Type::Int:
            type = (type == intType) ? i32Type : type;
            return type->name;
        case Type::Float:
            type = (type == floatType) ? f32Type : type;
            return type->name;
        case Type::Struct:
        case Type::Union:
        {
            String cName = String::CloneReplace( type->symbol->name, ".", "::", &globalTmpArena );
            return cName.data;
        }
        case Type::Enum:
        {
            Decl* enumDecl = type->symbol->decl;
            return BuildQualifiedNameTmp( enumDecl );
        } break;

        INVALID_DEFAULT_CASE;
    }
    return nullptr;
}

char* OptParen( char* str, bool b )
{
    return b ? Strf( "(%s)", str ) : str;
}

char* TypeToCdecl( Type* type, char const* symbolName )
{
    ASSERT( symbolName );
    bool haveSymbol = *symbolName != 0;

    switch( type->kind )
    {
        case Type::Void:
        case Type::Bool:
        case Type::Bits:
        case Type::Int:
        case Type::Float:
        case Type::String:
        case Type::Struct:
        case Type::Union:
        case Type::Enum:
            return Strf( "%s%s%s", CdeclType( type ), haveSymbol ? " " : "", symbolName );
        case Type::Pointer:
            return TypeToCdecl( type->ptr.base, OptParen( Strf( "*%s", symbolName ), haveSymbol ) );
        case Type::Array:
            return TypeToCdecl( type->array.base, OptParen( Strf( "%s[%lld]", symbolName, type->array.length ), haveSymbol ) );
        case Type::Buffer:
            return Strf( "buffer<%s>%s%s", TypeToCdecl( type->array.base, "" ), haveSymbol ? " " : "", symbolName );
        case Type::Func:
        {
            char* result = nullptr;
            result = Strf( "%s(", OptParen( Strf( "*%s", symbolName ), haveSymbol ) );
            for( FuncArg& arg : type->func.args )
                result = Strf( "%s%s%s", result, &arg == type->func.args.begin() ? "" : ", ", TypeToCdecl( arg.type, "" ) );

            result = Strf( "%s)", result );
            return TypeToCdecl( type->func.returnType, result );
        } break;

        INVALID_DEFAULT_CASE;
    }
    return nullptr;
}

void EmitExpr( Expr* expr, Type* expectedType = nullptr, StmtList* parentBlock = nullptr );

char const* NamesToCType( Array<char const*> const& names, int index = 0 )
{
    if( index >= names.count )
        return nullptr;

    char const* rhs = NamesToCType( names, index + 1 );
    return Strf( "%s%s%s", names[index], rhs ? "::" : "", rhs ? rhs : "" );
}

char const* TypeSpecToCdecl( TypeSpec* type, char const* symbolName );

// TODO Test!
char const* ReturnTypeSpecToCdecl( TypeSpec* funcType, char const* funcCdecl )
{
    ASSERT( funcType->kind == TypeSpec::Func );

    char const* result = nullptr;
    Array<TypeSpec*> const& types = funcType->func.returnTypes;

    if( types )
    {
        if( types.count == 1 )
            result = TypeSpecToCdecl( types[0], funcCdecl );
        else
        {
            // TODO Create a new type using the function's name and line and the types returned
            NOT_IMPLEMENTED;
        }
    }
    else
        result = Strf( "void %s", funcCdecl );

    return result;
}

char const* TypeSpecToCdecl( TypeSpec* type, char const* symbolName )
{
    ASSERT( symbolName );
    bool haveSymbol = *symbolName != 0;

    switch( type->kind )
    {
        case TypeSpec::Name:
            return Strf( "%s%s%s", NamesToCType( type->names ), haveSymbol ? " " : "", symbolName );
        case TypeSpec::Pointer:
            return TypeSpecToCdecl( type->ptr.base, OptParen( Strf( "*%s", symbolName ), haveSymbol ) );
        case TypeSpec::Array:
        {
            if( type->array.isView )
            {
                return Strf( "buffer<%s>%s%s", TypeSpecToCdecl( type->array.base, "" ), haveSymbol ? " " : "", symbolName );
            }

            char* countStr = "";
            if( type->array.length )
            {
                // FIXME This is still a friggin hack
                u8* startBase = globalTmpArena.base;
                countStr = (char*)(startBase + globalTmpArena.used);
                outArena = &globalTmpArena;
                EmitExpr( type->array.length );

                ASSERT( globalTmpArena.base == startBase );
                outArena = &globalOutArena;
            }
            char const* result = TypeSpecToCdecl( type->array.base, OptParen( Strf( "%s[%s]", symbolName, countStr ), !IsNullOrEmpty( symbolName ) ) );
            return result;
        } break;
        case TypeSpec::Func:
        {
            char* result = nullptr;
            result = Strf( "%s(", OptParen( Strf( "*%s", symbolName ), !IsNullOrEmpty( symbolName ) ) );

            for( FuncArgSpec const& arg : type->func.args )
                result = Strf( "%s%s%s", result, &arg == type->func.args.begin() ? "" : ", ", TypeSpecToCdecl( arg.type, "" ) );

            result = Strf( "%s)", result );
            return ReturnTypeSpecToCdecl( type, result );
        } break;

        INVALID_DEFAULT_CASE;
    }
    return nullptr;
}

internal char charToEscape[256];

void EmitStringLiteral( String const& str, bool isChar )
{
    // TODO Multiline
    if( isChar )
        OUTSTR( "'" );
    else
        OUTSTR( "\"" );

    char const* p = str.data;
    char const* end = str.data + str.length;
    while( p < end )
    {
        char const* start = p;
        while( p < end && isprint( (unsigned char)*p ) && !charToEscape[ (unsigned char)*p ] )
            p++;

        if( start != p )
            Out( start, p - start );

        if( p < end )
        {
            char c = charToEscape[ (unsigned char)*p ];
            if( c )
            {
                Out( Strf( "\\%c", c ) );
            }
            else
            {
                ASSERT( !isprint( (unsigned char)*p ) );
                Out( Strf( "\\x%X", (unsigned char)*p ) );
            }
            p++;
        }
    }

    if( isChar )
        OUTSTR( "'" );
    else
        OUTSTR( "\"" );
}

bool EmitMetaExpr( Expr* base, char const* name )
{
    int index = FindMetaAttr( name );
    ASSERT( index >= 0 );

    switch( index )
    {
        case MetaAttr::Size:
            OUTSTR( "SIZEOF( " );
            EmitExpr( base );
            OUTSTR( " )" );
            break;
        case MetaAttr::Offset:
        {
            ASSERT( base->kind == Expr::Field );
            name = base->field.name;
            base = base->field.base;

            OUTSTR( "OFFSETOF( " );
            EmitExpr( base );
            OUTSTR( ", " );
            Out( name );
            OUTSTR( " )" );
        } break;
        case MetaAttr::Name:
        {
            EmitExpr( base );
            OUTSTR( ".name()" );
        } break;

        // Use default behaviour for fields
        case MetaAttr::Index:
        default:
            return false;
    }

    return true;
}

void EmitStmtBlock( StmtList const* block );

ArgExpr const* FindByName( Type* funcType, int wantedIndex, Array<ArgExpr> const& givenArgs )
{
    ASSERT( funcType->kind == Type::Func );

    // TODO Same issue here as in ResolveArgNameExpr()
    Decl* funcDecl = funcType->func.resolvedDecl;
    // Assert to find out where this doesn't hold
    //ASSERT( funcDecl );

    if( funcDecl )
    {
        Array<FuncArgDecl> const& declArgs = funcDecl->func.args;
        char const* wantedName = declArgs[wantedIndex].name;

        for( ArgExpr const& a : givenArgs )
        {
            if( a.nameExpr && a.nameExpr->name.ident == wantedName )
                return &a;
        }
    }

    return nullptr;
}

void EmitArgExpr( ArgExpr const& arg, Type* wantedType, bool isForeignFunc )
{
    ResolvedExpr const& givenExpr = arg.expr->resolvedExpr;
    // Convert strings inside foreign function calls
    // TODO All this shit we're only doing because of printf, so make a better print function
    if( isForeignFunc )
    {
        if( givenExpr.type == stringType && !givenExpr.isConst )
            OUTSTR( "(char const*)" );
        else if( givenExpr.type->kind == Type::Enum )
        {
            Type* valueType = givenExpr.type->enum_.base;
            OUTSTR( "(" ); Out( TypeToCdecl( valueType, "" ) ); OUTSTR( ")" );
        }
    }

    EmitExpr( arg.expr, wantedType );
}

void EmitExpr( Expr* expr, Type* expectedType /*= nullptr*/, StmtList* parentBlock /*= nullptr*/ )
{
    switch( expr->kind )
    {
        case Expr::Int:
            Out( Strf( "%lld", expr->literal.intValue ) );
            break;
        case Expr::Float:
            // TODO Copy the actual token string instead
            Out( Strf( "%f", expr->literal.floatValue ) );
            if( expectedType && expectedType->size == 4 )
                OUTSTR( "f" );
            break;
        case Expr::Str:
        {
            bool isChar = expectedType && expectedType->kind != Type::String && expr->literal.strValue.length == 1;
            EmitStringLiteral( expr->literal.strValue, isChar );
        } break;
        case Expr::Name:
        {
            char const* name = expr->name.ident;
            Type* nameType = expr->resolvedExpr.type;

            if( expectedType && expectedType->kind == Type::Buffer && nameType->kind == Type::Array )
            {
                OUTSTR( "{ " );
                Out( expr->name.ident );
                OUTSTR( ", ARRAYCOUNT(" );
                Out( expr->name.ident );
                OUTSTR( ") }" );
            }
            else if( nameType->kind == Type::Func )
            {
                Symbol* sym = expr->name.symbol;
                Decl* funcDecl = (sym->kind == Symbol::Func) ? sym->decl : nullptr;

                if( funcDecl && funcDecl->parentBlock && !IsForeign( funcDecl ) )
                    name = BuildQualifiedNameTmp( funcDecl->parentBlock, expr->name.ident );
                Out( name );
            }
            else if( nameType->kind == Type::TypeVal && nameType->value.type->kind == Type::Enum )
            {
                Symbol* sym = expr->name.symbol;
                Decl* enumDecl = sym->decl;

                name = BuildQualifiedNameTmp( enumDecl );
                Out( name );
            }
            else
            {
                if( name == Intern( String( "null" ) )->data )
                    name = "nullptr";

                Out( name );
            }
        } break;
        case Expr::Cast:
            OUTSTR( "(" );
            Out( TypeToCdecl( expr->resolvedExpr.type, "" ) );
            OUTSTR( ")" );
            EmitExpr( expr->cast.expr );
            break;
        case Expr::Call:
        {
            EmitExpr( expr->call.func, nullptr, parentBlock );
            Type* funcType = expr->call.func->resolvedExpr.type;
            ASSERT( funcType->kind == Type::Func );

            // FIXME This doesn't work for anything that doesn't call directly by name
            // TODO Add 'foreign' as a flag to the type
            Symbol* sym = expr->call.func->kind == Expr::Name ? expr->call.func->name.symbol : nullptr;
            bool isForeign = (sym && sym->decl) ? IsForeign( sym->decl ) : false;

            int givenIndex = 0, wantedIndex = 0;
            Array<FuncArg> const& wantedArgs = funcType->func.args;
            Array<ArgExpr> const& givenArgs = expr->call.args;

            struct EmittedArgExpr
            {
                FuncArg const* wantedArg;
                ArgExpr const* givenArg;
                int varargLen;
            };
            // Copy given args array to house the final args we'll emit
            Array<EmittedArgExpr> emittedArgs( &globalTmpArena, wantedArgs.count, NoClear() );
            emittedArgs.ResizeToCapacity();
            for( int i = 0; i < emittedArgs.count; ++i )
                emittedArgs[i] = { &wantedArgs[i], nullptr, 0 };

            // TODO Test thoroughly
            int varargCount = 0;
            while( givenIndex < givenArgs.count && wantedIndex < wantedArgs.count )
            {
                FuncArg const& wantedArg = wantedArgs[wantedIndex];
                Type* wantedType = wantedArg.type;

                ArgExpr const* givenArg = &givenArgs[givenIndex];
                ArgExpr const* namedArg = FindByName( funcType, wantedIndex, givenArgs );
                if( namedArg )
                {
                    givenArg = namedArg;
                }
                else if( wantedArg.isVararg )
                {
                    if( !varargCount )
                        emittedArgs[wantedIndex] = { &wantedArg, givenArg, 0 };

                    if( !isForeign )
                    {
                        // Use the base type for the conversion
                        ASSERT( wantedType->kind == Type::Buffer );
                        wantedType = wantedType->array.base;
                    }

                    // Keep adding the given args to the vararg until the type doesn't match anymore
                    ResolvedExpr const& givenExpr = givenArg->expr->resolvedExpr;
                    // Make a copy so we don't modify the resolved type while converting
                    ResolvedExpr cnvtExpr = givenExpr;
                    if( ConvertType( &cnvtExpr, wantedType ) )
                    {
                        varargCount++;
                        givenIndex++;
                        continue;
                    }
                    else
                    {
                        emittedArgs[wantedIndex].varargLen = varargCount;
                        varargCount = 0;
                        wantedIndex++;
                        continue;
                    }
                }

                emittedArgs[wantedIndex] = { &wantedArg, givenArg, 0 };

                givenIndex++;
                wantedIndex++;
            }
            // If the last one is a vararg, fix the count
            if( wantedArgs )
            {
                FuncArg const& lastWantedArg = wantedArgs.Last();
                if( lastWantedArg.isVararg )
                    emittedArgs[wantedArgs.count - 1].varargLen = varargCount;
            }

            ASSERT( givenIndex == givenArgs.count );
            // TODO We're relying on C++'s varargs and optionals here, but this won't be enough
            //ASSERT( wantedIndex == wantedArgs.count );


            // Emit resulting args
            OUTSTR( "(" );
            if( emittedArgs.count )
                OUTSTR( " " );

            for( EmittedArgExpr const& a : emittedArgs )
            {
                if( !a.givenArg )
                    continue;

                if( &a != emittedArgs.begin() )
                    OUTSTR( ", " );

                Type* wantedType = a.wantedArg->type;
                if( a.wantedArg->isVararg )
                {
                    if( !isForeign )
                    {
                        OUTSTR( "BUFFER( " );
                        Out( TypeToCdecl( wantedType->array.base, "" ) );
                        OUTSTR( ", " );
                    }

                    for( int i = 0; i < a.varargLen; ++i )
                    {
                        if( i != 0 )
                            OUTSTR( ", " );
                        // FIXME Assert givenArg in range
                        EmitArgExpr( a.givenArg[i], wantedType, isForeign );
                    }

                    if( !isForeign )
                        OUTSTR( " )" );

                    continue;
                }
                else
                    EmitArgExpr( *a.givenArg, wantedType, isForeign );
            }

            if( emittedArgs.count )
                OUTSTR( " " );
            OUTSTR( ")" );
        } break;

        case Expr::Index:
            EmitExpr( expr->index.base );
            OUTSTR( "[" );
            EmitExpr( expr->index.index );
            OUTSTR( "]" );
            break;
        case Expr::Field:
        {
            if( expr->field.isMeta && EmitMetaExpr( expr->field.base, expr->field.name ) )
                break;

            Expr* base = expr->field.base;
            EmitExpr( base );

            bool isType = base->resolvedExpr.type->kind == Type::TypeVal;
            if( isType )
                OUTSTR( "::" );
            else
                OUTSTR( "." );

            Out( expr->field.name );
        } break;
        case Expr::Compound:
        {
            // Check if we need an inplace conversion to buffer
            bool convertToBuffer = false;
            if( expectedType && expectedType->kind == Type::Buffer && expr->resolvedExpr.type->kind == Type::Array )
            {
                OUTSTR( "BUFFER( " );
                Out( TypeToCdecl( expectedType->array.base, "" ) );
                OUTSTR( ", " );
                convertToBuffer = true;
            }

            if( !convertToBuffer )
                OUTSTR( "{ " );

            if( expr->compoundFields[0].kind == CompoundField::Name )
            {
                // NOTE Assume all of them are named
                Type* type = expr->resolvedExpr.type;
                ASSERT( type && (type->kind == Type::Struct || type->kind == Type::Union) );

                for( TypeField const& tf : type->aggregate.fields )
                {
                    if( &tf != type->aggregate.fields.begin() )
                        OUTSTR( ", " );

                    CompoundField const* field = nullptr;
                    for( CompoundField const& f : expr->compoundFields )
                        if( f.name == tf.name )
                        {
                            field = &f;
                            break;
                        }

                    if( field )
                        EmitExpr( field->initValue );
                    else
                        OUTSTR( "{}" );
                }
            }
            else
            {
                int capacity = 16;
                Expr** fieldExprs = PUSH_ARRAY( &globalTmpArena, Expr*, capacity );

                sz index = 0;
                bool fillRange = false;
                for( CompoundField const& f : expr->compoundFields )
                {
                    if( f.kind == CompoundField::Index )
                    {
                        if( f.index->kind == Expr::Int )
                            index = I32( f.index->resolvedExpr.constValue.intValue );
                        else if( f.index->kind == Expr::Range )
                        {
                            if( f.index->range.lowerBound == nullptr && f.index->range.upperBound == nullptr )
                            {
                                ASSERT( expr->compoundFields.count == 1 );
                                
                                fieldExprs[0] = f.initValue;
                                index = expectedType->array.length;
                                fillRange = true;

                                break;
                            }
                        }
                    }

                    if( index >= capacity )
                    {
                        Expr** newBuffer = PUSH_ARRAY( &globalTmpArena, Expr*, 2 * capacity );
                        PCOPY( fieldExprs, newBuffer, Sz( capacity * sizeof(Expr*) ) );
                        capacity = 2 * capacity;

                        fieldExprs = newBuffer;
                    }

                    fieldExprs[index++] = f.initValue;
                }

                Expr* fieldExpr = fillRange ? fieldExprs[0] : nullptr;
                for( int i = 0; i < index; ++i )
                {
                    if( i != 0 )
                        OUTSTR( ", " );

                    if( !fillRange )
                        fieldExpr = fieldExprs[i];

                    if( fieldExpr )
                        EmitExpr( fieldExpr );
                    else
                        OUTSTR( "{}" );
                }
            }

            if( convertToBuffer )
                OUTSTR( " )" );
            else
                OUTSTR( " }" );
        } break;

        case Expr::Unary:
            Out( TokenKind::names[expr->unary.op] );
            OUTSTR( "(" );
            EmitExpr( expr->unary.expr, expectedType );
            OUTSTR( ")" );
            break;
        case Expr::Binary:
            OUTSTR( "(" );
            EmitExpr( expr->binary.left );
            OUTSTR( " " );
            Out( TokenKind::names[expr->binary.op] );
            OUTSTR( " " );
            EmitExpr( expr->binary.right );
            OUTSTR( ")" );
            break;
        case Expr::Ternary:
            OUTSTR( "(" );
            EmitExpr( expr->ternary.cond );
            OUTSTR( " ? " );
            EmitExpr( expr->ternary.thenExpr );
            OUTSTR( " : " );
            EmitExpr( expr->ternary.elseExpr );
            OUTSTR( ")" );
            break;
        case Expr::Comma:
            for( Expr* e : expr->commaExprs )
            {
                if( e != expr->commaExprs[0] )
                    OUTSTR( ", " );
                EmitExpr( e );
            }
            break;
        //case Expr::Range:
        //break;
        case Expr::Sizeof:
            OUTSTR( "sizeof(" );
            EmitExpr( expr->sizeofExpr );
            OUTSTR( ")" );
            break;

        case Expr::Lambda:
        {
            Decl* decl = expr->lambda.decl;
            auto const& declArgs = decl->func.args;
            auto const& args = decl->resolvedType->func.args;

            // TODO Multiple return types
            OUTSTR( "[](" );
            if( args.count )
                OUTSTR( " " );
            int i = 0;
            for( FuncArg const& a : args )
            {
                if( &a != args.begin() )
                    OUTSTR( ", " );
                char const* name = declArgs[i++].name;
                Out( TypeToCdecl( a.type, name ) );
                if( a.defaultValue )
                {
                    OUTSTR( " = " );
                    EmitExpr( a.defaultValue );
                }
            }
            if( args.count )
                OUTSTR( " " );
            OUTSTR( ")" );

            OutNL();
            EmitStmtBlock( decl->func.body );
            OutNL();
        } break;

        INVALID_DEFAULT_CASE
    }
}

char const* BuildTypeName( Array<Type*> const& types )
{
    StringBuilder sb( &globalTmpArena );
    for( Type* t : types )
        sb.Append( t->name );
    return sb.ToString( &globalTmpArena ).data;
}

void EmitReturnTypes( Decl* decl, char const* funcName )
{
    ASSERT( decl->kind == Decl::Func );
    
    Type* type = decl->resolvedType->func.returnType;
    if( type->kind == Type::Multi )
    {
        Array<Type*> const& types = type->multi.types;
        Out( BuildTypeName( types ) );
        OUTSTR( " " );
        Out( funcName );
    }
    else
        Out( TypeToCdecl( type, funcName ) );
}

void EmitFuncDecl( Decl* decl )
{
    ASSERT( decl->kind == Decl::Func );

    char const* name = BuildQualifiedNameTmp( decl );

    EmitReturnTypes( decl, name );
    auto const& declArgs = decl->func.args;
    auto const& args = decl->resolvedType->func.args;
    OUTSTR( "(" );
    if( args.count )
        OUTSTR( " " );
    int i = 0;
    for( FuncArg const& a : args )
    {
        if( &a != args.begin() )
            OUTSTR( ", " );
        name = declArgs[i++].name;
        Out( TypeToCdecl( a.type, name ) );
        if( a.defaultValue )
        {
            OUTSTR( " = " );
            EmitExpr( a.defaultValue );
        }
    }
    if( args.count )
        OUTSTR( " " );
    OUTSTR( ")" );
}

void EmitVarDecl( Decl* decl, char const* name, int exprIndex, bool nested, bool init = true )
{
    Type* resolvedType = decl->resolvedType; 

    OutIndent();
    char const* multiTypeVarName = nullptr;
    if( resolvedType->kind == Type::Multi )
    {
        multiTypeVarName = Strf( "__multi%d", decl->pos.lineNumber );

        // NOTE Assume multi decls are processed in order
        if( exprIndex == 0 )
        {
            Array<Type*> const& types = resolvedType->multi.types;
            Out( BuildTypeName( types ) );
            OUTSTR( " " );

            Out( multiTypeVarName );
        }
        else
            init = false;
    }
    else
    {
        if( decl->var.isConst )
        {
            if( nested )
                OUTSTR( "static " );
            OUTSTR( "constexpr " );
        }

#if 0
        TypeSpec* typeSpec = decl->var.type;
        if( typeSpec )
            Out( TypeSpecToCdecl( typeSpec, name ) );
        else
#endif
            Out( TypeToCdecl( resolvedType, name ) );
    }

    if( init )
    {
        Expr* initExpr = decl->var.initExpr;

        if( initExpr && resolvedType->kind == Type::Union )
        {
            OUTSTR( "; " );

            // TODO Should probably clear out the whole thing (largest field) first
            ASSERT( initExpr->kind == Expr::Compound && initExpr->compoundFields.count == 1 );
            CompoundField const& field = initExpr->compoundFields[0];
            ASSERT( field.kind == CompoundField::Name );

            Out( name );
            OUTSTR( "." );
            Out( field.name );

            initExpr = field.initValue;
            // Find out which type is the actual field we're initializing
            for( TypeField const& f : resolvedType->aggregate.fields )
            {
                if( f.name == field.name )
                    resolvedType = f.type;
            }
        }

        OUTSTR( " = " );

        if( initExpr && initExpr->kind == Expr::Comma )
            initExpr = exprIndex < initExpr->commaExprs.count ? initExpr->commaExprs[exprIndex] : nullptr;

        if( initExpr )
        {
            if( resolvedType == boolType && initExpr->resolvedExpr.type != boolType )
                OUTSTR( "(bool)" );

            EmitExpr( initExpr, resolvedType );
        }
        else
            OUTSTR( "{}" );
    }

    OUTSTR( ";" );
    OutNL();

    if( resolvedType->kind == Type::Multi )
    {
        resolvedType = resolvedType->multi.types[exprIndex];

        OutIndent();
        if( decl->var.isConst )
        {
            if( nested )
                OUTSTR( "static " );
            OUTSTR( "constexpr " );
        }

#if 0
        TypeSpec* typeSpec = decl->var.type;
        if( typeSpec )
            Out( TypeSpecToCdecl( typeSpec, name ) );
        else
#endif
            Out( TypeToCdecl( resolvedType, name ) );

        OUTSTR( " = " );
        Out( multiTypeVarName );
        Out( Strf( "._%d", exprIndex ) );
        OUTSTR( ";" );
        OutNL();
    }
}

void EmitMultiType( Type* type )
{
    ASSERT( type->kind == Type::Multi );

    Array<Type*> const& types = type->multi.types;

    OutIndent();
    OUTSTR( "struct " );
    Out( BuildTypeName( types ) );
    OutNL();
    OutIndent();
    OUTSTR( "{" );
    OutNL();

    globalIndent++;
    int i = 0;
    for( Type* t : types )
    {
        OutIndent();
        Out( TypeToCdecl( t, Strf( "_%d", i++ ) ) );
        OUTSTR( ";" );
        OutNL();
    }
    globalIndent--;
    OutIndent();
    OUTSTR( "};" );
    OutNL();

    type->flags |= Type::Emitted;
}

// TODO Remove 'nested' and link nodes to parent nodes so we have that info available here
void EmitDecl( Decl* decl, Symbol* symbol = nullptr, bool nested = false, bool init = true )
{
    if( decl->flags & Node::SkipCodegen )
        return;

    switch( decl->kind )
    {
        case Decl::Var:
        {
            // NOTE For declarations where multiple names are declared together, right now the situation is different
            // for top-level variables and fields inside aggregates. Top-level variables will have a separate symbol created
            // for each name, so we just emit the given symbol name. For aggregate fields, we just emit
            // a field per name on separate lines
            // TODO Is nested true for _function_ decls too?
            if( nested )
            {
                int i = 0;
                for( char const* name : decl->names )
                {
                    EmitVarDecl( decl, name, i, nested, init );
                    i++;
                }
            }
            else
            {
                ASSERT( symbol, "Need a (named) symbol for top-level variable declarations" );
                char const** name = decl->names.Find( symbol->name ); 
                ASSERT( name );

                int index = I32( name - decl->names.begin() );
                EmitVarDecl( decl, symbol->name, index, nested, init );
            }
        } break;
        case Decl::Struct:
        case Decl::Union:
        {
            OutIndent();
            bool isStruct = decl->kind == Decl::Struct;
            if( isStruct )
                OUTSTR( "struct " );
            else
                OUTSTR( "union " );
            Out( decl->names[0] );
            OutNL();
            OutIndent();
            OUTSTR( "{" );
            OutNL();

            globalIndent++;

            Decl* initField = nullptr;
            // If this is a union, find out which single member to initialize
            if( !isStruct )
            {
                sz maxFieldSize = 0;
                Decl* largestField = nullptr;
                for( Decl* field : decl->aggregate.items )
                {
                    if( field->kind != Decl::Var )
                        continue;

                    if( field->var.initExpr )
                    {
                        initField = field;
                        break;
                    }

                    sz fieldSize = field->resolvedType->size;
                    if( fieldSize > maxFieldSize )
                    {
                        maxFieldSize = fieldSize;
                        largestField = field;
                    }
                }

                if( !initField )
                    initField = largestField;
            }

            for( Decl* field : decl->aggregate.items )
            {
                EmitDecl( field, nullptr, true, isStruct || field == initField );
            }

            globalIndent--;
            OutIndent();
            OUTSTR( "};" );
            OutNL();
        } break;

        case Decl::Enum:
        {
            // TODO Have a quick check in Godbolt how this code compares against the ENUM_STRUCT for the typical ops

            Type* type = decl->resolvedType;
            Type* baseType = type->enum_.base;

            char const* typeName = BuildQualifiedNameTmp( decl );
            char const* baseTypeName = TypeToCdecl( baseType, "" );

            OutIndent(); OUTSTR( "struct " ); Out( typeName ); OutNL();
            OutIndent(); OUTSTR( "{" ); OutNL();

            globalIndent++;

            OutIndent(); Out( TypeToCdecl( i32Type, "index" ) ); OUTSTR( ";" ); OutNL();
            OutNL();
            OutIndent(); OUTSTR( "INLINE char const* name() const { return names[index]; }" ); OutNL();
            OutIndent(); OUTSTR( "INLINE " ); Out( baseTypeName ); OUTSTR( " const& value() const { return values[index]; }" ); OutNL();
            OutIndent(); OUTSTR( "INLINE operator " ); Out( baseTypeName ); OUTSTR( " const& () const { return value(); }" ); OutNL();
            OutNL();
            // TODO These should be Strings
            OutIndent(); OUTSTR( "static constexpr char const* const names[] = { " );
            for( EnumItem const& it : type->enum_.items )
            {
                OUTSTR( "\"" ); Out( it.name ); OUTSTR( "\", " );
            }
            OUTSTR( "};" ); OutNL();
            OutIndent(); OUTSTR( "static constexpr " ); Out( baseTypeName ); OUTSTR( " const values[] = { " );
            for( EnumItem const& it : type->enum_.items )
            {
                EmitExpr( it.initExpr ); OUTSTR( ", " );
            }
            OUTSTR( "};" ); OutNL();

            for( EnumItem const& it : type->enum_.items )
            {
                OutIndent(); OUTSTR( "static " ); Out( typeName ); OUTSTR( " const " ); Out( it.name ); OUTSTR( ";" ); OutNL();
            }

            globalIndent--;
            OUTSTR( "};" ); OutNL();

            int index = 0;
            for( EnumItem const& it : type->enum_.items )
            {
                OutIndent(); OUTSTR( "constexpr " ); Out( typeName ); OUTSTR( " const " ); Out( typeName ); OUTSTR( "::" ); Out( it.name );
                OUTSTR( " = {" ); Out( Strf( "%d", index++ ) ); OUTSTR( "};" ); OutNL();
            }
            OutNL();
        } break;

        case Decl::Func:
        {
            if( IsForeign( decl ) )
                break;

            // Only function prototypes at this stage
            // TODO Most of these are completely unnecessary

            bool localDecl = decl->parentBlock != nullptr;
            if( localDecl )
                globalInnerFuncs.Push( decl );

            EmitFuncDecl( decl );
            OUTSTR( ";" );
        } break;
    }

    if( !nested )
        OutNL();
}

void EmitAssignExpr( Expr* leftExpr, Expr* rightExpr, char const* op )
{
    Type* leftType = leftExpr->resolvedExpr.type;
    EmitExpr( leftExpr );

    OUTSTR( " " );
    Out( op );
    OUTSTR( " " );

    Type* rightType = rightExpr->resolvedExpr.type;
    if( leftType == boolType && rightType != boolType )
        OUTSTR( "(bool)" );
    EmitExpr( rightExpr );
    OUTSTR( ";" );
}

void EmitStmt( Stmt* stmt )
{
    if( stmt->flags & Node::SkipCodegen )
        return;

    EmitPos( stmt->pos );

    switch( stmt->kind )
    {
        case Stmt::Expr:
            OutIndent();
            EmitExpr( stmt->expr, nullptr, stmt->parentBlock );
            OUTSTR( ";" );
            break;
        case Stmt::Decl:
            // Enums must be emitted in the global scope
            if( stmt->decl->kind != Decl::Enum )
                EmitDecl( stmt->decl, nullptr, true );
            break;
        case Stmt::Assign:
        {
            Expr* leftExpr = stmt->assign.left;
            Expr* rightExpr = stmt->assign.right;
            char const* opName = TokenKind::names[stmt->assign.op];

            OutIndent();
            if( leftExpr->kind == Expr::Comma )
            {
                Array<Expr*> const& leftExprs = leftExpr->commaExprs;
                Array<Expr*> const& rightExprs = rightExpr->commaExprs;

                for( int i = 0; i < leftExprs.count; ++i )
                {
                    if( i != 0 )
                        OUTSTR( " " );
                    EmitAssignExpr( leftExprs[i], rightExprs[i], opName );
                }
            }
            else
                EmitAssignExpr( leftExpr, rightExpr, opName );
        } break;
        case Stmt::If:
            OutIndent();
            OUTSTR( "if( " );
            EmitExpr( stmt->if_.cond );
            OUTSTR( " )" );
            OutNL();
            EmitStmtBlock( stmt->if_.thenBlock );

            for( ElseIf const& ei : stmt->if_.elseIfs )
            {
                OutIndent();
                OUTSTR( "else if( " );
                EmitExpr( ei.cond );
                OUTSTR( " )" );
                OutNL();
                EmitStmtBlock( ei.block );
            }

            if( stmt->if_.elseBlock->stmts.count )
            {
                OutIndent();
                OUTSTR( "else" );
                OutNL();
                EmitStmtBlock( stmt->if_.elseBlock );
            }
            break;
        case Stmt::While:
            OutIndent();
            if( stmt->while_.isDoWhile )
                OUTSTR( "do" );
            else
            {
                OUTSTR( "while( " );
                EmitExpr( stmt->while_.cond );
                OUTSTR( " )" );
            }
            OutNL();
            EmitStmtBlock( stmt->while_.block );
            if( stmt->while_.isDoWhile )
            {
                OutIndent();
                OUTSTR( "while( " );
                EmitExpr( stmt->while_.cond );
                OUTSTR( " );" );
            }
            break;
        case Stmt::For:
        {
            OutIndent();
            OUTSTR( "for( " );

            Expr* rangeExpr = stmt->for_.rangeExpr;
            // TODO Iterator range
            Expr* lowerBound = rangeExpr->range.lowerBound;
            if( lowerBound )
            {
                //Out( TypeToCdecl( lowerBound->resolvedExpr.type, stmt->for_.indexName ) );
                Out( TypeToCdecl( rangeExpr->resolvedExpr.type, stmt->for_.indexName ) );
                OUTSTR( " = " );
                EmitExpr( lowerBound );
                OUTSTR( "; " );
                Out( stmt->for_.indexName );
                OUTSTR( " < " );
                EmitExpr( rangeExpr->range.upperBound );
                OUTSTR( "; ++" );
                Out( stmt->for_.indexName );
            }
            else
            {
                Out( TypeToCdecl( rangeExpr->resolvedExpr.type, stmt->for_.indexName ) );
                OUTSTR( " : " );
                EmitExpr( rangeExpr->range.upperBound );
            }
            OUTSTR( " )" );
            OutNL();
            EmitStmtBlock( stmt->for_.block );
        } break;

        case Stmt::Switch:
            OutIndent();
            OUTSTR( "switch( " );
            EmitExpr( stmt->switch_.expr );
            OUTSTR( " )" );
            OutNL();
            OutIndent();
            OUTSTR( "{" );
            OutNL();
            globalIndent++;

            for( SwitchCase const& c : stmt->switch_.cases )
            {
                OutIndent();
                if( c.isDefault )
                    OUTSTR( "default:" );
                else
                {
                    OUTSTR( "case " );
                    EmitExpr( c.expr );
                    OUTSTR( ":" );
                }
                OutNL();
                EmitStmtBlock( c.block );
            }

            globalIndent--;
            OutIndent();
            OUTSTR( "}" );
            OutNL();
            break;
        case Stmt::Break:
            OutIndent();
            OUTSTR( "break;" );
            break;
        case Stmt::Continue:
            OutIndent();
            OUTSTR( "continue;" );
            break;
        case Stmt::Return:
            OutIndent();
            OUTSTR( "return" );
            if( stmt->expr )
            {
                OUTSTR( " " );
                if( stmt->expr->kind == Expr::Comma )
                    OUTSTR( "{ " );
                EmitExpr( stmt->expr );
                if( stmt->expr->kind == Expr::Comma )
                    OUTSTR( " }" );
            }
            OUTSTR( ";" );
            break;
        case Stmt::Block:
            EmitStmtBlock( stmt->block );
            break;

        INVALID_DEFAULT_CASE;
    }

    OutNL();
}

void EmitStmtBlock( StmtList const* block )
{
    OutIndent();
    OUTSTR( "{" );
    OutNL();

    globalIndent++;
    for( Stmt* stmt : block->stmts )
        EmitStmt( stmt );
    globalIndent--;

    OutIndent();
    OUTSTR( "}" );
    OutNL();
}

void EmitForwardDecls()
{
    for( auto idx = globalOrderedSymbols.First(); idx; ++idx )
    {
        Symbol const* sym = *idx;
        Decl* decl = sym->decl;
        if( !decl )
            continue;

        char const* symName = sym->name;
        if( sym->parent )
        {
            String cName = String::CloneReplace( symName, ".", "::", &globalTmpArena );
            symName = cName.data;
        }

        switch( decl->kind )
        {
            case Decl::Struct:
            {
                OUTSTR( "struct " );
                Out( symName );
                OUTSTR( ";" );
                OutNL();
            } break;
            case Decl::Union:
            {
                OUTSTR( "union " );
                Out( symName );
                OUTSTR( ";" );
                OutNL();
            } break;
        }
    }

    for( auto idx = globalForwardDecls.First(); idx; ++idx )
    {
        Decl* decl = *idx;
        EmitDecl( decl, nullptr, true );
        OutNL();
    }

    for( auto idx = globalForwardTypes.First(); idx; ++idx )
    {
        Type* type = *idx;
        // Emit a synthetic type to hold Multi types
        if( type->kind == Type::Multi && (type->flags & Type::Emitted) == 0 )
        {
            EmitMultiType( type );
            OutNL();
        }
    }
}

void EmitOrderedSymbols()
{
    for( auto idx = globalOrderedSymbols.First(); idx; ++idx )
    {
        Symbol* sym = *idx;

        Decl* decl = sym->decl;
        if( !decl )
            continue;

        EmitDecl( decl, sym );
    }

    // Do just function bodies at the end
    for( auto idx = globalOrderedSymbols.First(); idx; ++idx )
    {
        Symbol* sym = *idx;

        Decl* decl = sym->decl;
        if( !decl || decl->kind != Decl::Func || IsForeign( decl ) )
            continue;

        EmitPos( decl->pos );
        OutIndent();
        EmitFuncDecl( decl );
        OutNL();
        EmitStmtBlock( decl->func.body );
        OutNL();
    }
}

void EmitInnerFunctions()
{
    for( auto idx = globalInnerFuncs.First(); idx; ++idx )
    {
        Decl* decl = *idx;
        ASSERT( decl->kind == Decl::Func );

        EmitPos( decl->pos );
        EmitFuncDecl( decl );
        OutNL();
        EmitStmtBlock( decl->func.body );
        OutNL();
    }
}


char const* globalPreamble =
#include "preamble.inl"

void GenerateAll()
{
    INIT( globalInnerFuncs ) BucketArray<Decl*>( &globalArena, 16 );

    // Escape sequences
    charToEscape['\0'] = '0';
    charToEscape['\''] = '\'';
    charToEscape['"'] = '"';
    charToEscape['\\'] = '\\';
    charToEscape['\n'] = 'n';
    charToEscape['\r'] = 'r';
    charToEscape['\t'] = 't';
    charToEscape['\v'] = 'v';
    charToEscape['\b'] = 'b';
    charToEscape['\a'] = 'a';

    OUTSTR( "#pragma region Preamble" );
    OutNL();
    Out( globalPreamble );
    OUTSTR( "#pragma endregion Preamble" );
    OutNL();
    OutNL();

    OUTSTR( "///// Forward declarations" );
    OutNL();
    EmitForwardDecls();
    OutNL();
    OUTSTR( "///// Ordered declarations" );
    OutNL();
    EmitOrderedSymbols();
    OutNL();
    OUTSTR( "///// Inner functions" );
    OutNL();
    EmitInnerFunctions();
}
