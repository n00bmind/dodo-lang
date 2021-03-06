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

char const* CdeclType( Type* type )
{
    switch( type->kind )
    {
        case Type::Void:
            return "void";
        case Type::Bool:
            return "bool";
        case Type::Int:
            return "int";
        case Type::Float:
            return "float";
        case Type::Enum:
        case Type::Struct:
        case Type::Union:
            return type->symbol->name;

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
    switch( type->kind )
    {
        case Type::Void:
        case Type::Bool:
        case Type::Int:
        case Type::Float:
        case Type::Enum:
        case Type::Struct:
        case Type::Union:
            return Strf( "%s%s%s", CdeclType( type ), *symbolName ? " " : "", symbolName );
        case Type::Pointer:
            return TypeToCdecl( type->ptr.base, OptParen( Strf( "*%s", symbolName ), !IsNullOrEmpty( symbolName ) ) );
        case Type::Array:
            return TypeToCdecl( type->array.base, OptParen( Strf( "%s[%lld]", symbolName, type->array.count ), !IsNullOrEmpty( symbolName ) ) );
        case Type::Func:
        {
            char* result = nullptr;
            result = Strf( "%s(", OptParen( Strf( "*%s", symbolName ), !IsNullOrEmpty( symbolName ) ) );
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

char* TypeSpecToCdecl( TypeSpec* type, char const* symbolName )
{
    switch( type->kind )
    {
        case TypeSpec::Name:
            // TODO Multiple names
            return Strf( "%s%s%s", type->names[0], *symbolName ? " " : "", symbolName );
        case TypeSpec::Pointer:
            return TypeSpecToCdecl( type->ptr.base, OptParen( Strf( "*%s", symbolName ), !IsNullOrEmpty( symbolName ) ) );
        case TypeSpec::Array:
        {
            if( type->array.isView )
            {
                return Strf( "buffer<%s>%s%s", TypeSpecToCdecl( type->array.base, "" ), *symbolName ? " " : "", symbolName );
            }

            char* countStr = "";
            if( type->array.count )
            {
                // FIXME This is still a friggin hack
                u8* startBase = globalTmpArena.base;
                countStr = (char*)(startBase + globalTmpArena.used);
                outArena = &globalTmpArena;
                EmitExpr( type->array.count );

                ASSERT( globalTmpArena.base == startBase );
                outArena = &globalOutArena;
            }
            char* result = TypeSpecToCdecl( type->array.base, OptParen( Strf( "%s[%s]", symbolName, countStr ), !IsNullOrEmpty( symbolName ) ) );
            return result;
        } break;
        case TypeSpec::Func:
        {
            char* result = nullptr;
            result = Strf( "%s(", OptParen( Strf( "*%s", symbolName ), !IsNullOrEmpty( symbolName ) ) );

            for( FuncArgSpec const& arg : type->func.args )
                result = Strf( "%s%s%s", result, &arg == type->func.args.begin() ? "" : ", ", TypeSpecToCdecl( arg.type, "" ) );

            result = Strf( "%s)", result );
            return TypeSpecToCdecl( type->func.returnType, result );
        } break;

        INVALID_DEFAULT_CASE;
    }
    return nullptr;
}

char const* BuildQualifiedNameTmp( StmtList* parentBlock, char const* name )
{
    StringBuilder sb( &globalTmpArena );
    sb.Append( "%s_%s", parentBlock->globalPath.data, name );
    return sb.ToString( &globalTmpArena ).data;
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
            if( expectedType->size == 4 )
                OUTSTR( "f" );
            break;
        case Expr::Str:
        {
            bool isChar = expectedType && expectedType->kind != Type::String && expr->literal.strValue.length == 1;
            EmitStringLiteral( expr->literal.strValue, isChar );
        } break;
        case Expr::Name:
        {
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
                Decl* funcDecl = sym->decl;

                char const* name = expr->name.ident;
                if( funcDecl && funcDecl->parentBlock && !IsForeign( funcDecl ) )
                    name = BuildQualifiedNameTmp( funcDecl->parentBlock, expr->name.ident );
                Out( name );
            }
            else
                Out( expr->name.ident );
        } break;
        case Expr::Cast:
            OUTSTR( "(" );
            Out( TypeSpecToCdecl( expr->cast.type, "" ) );
            OUTSTR( ")" );
            EmitExpr( expr->cast.expr );
            break;
        case Expr::Call:
        {
            EmitExpr( expr->call.func, nullptr, parentBlock );
            Type* funcType = expr->call.func->resolvedExpr.type;
            ASSERT( funcType->kind == Type::Func );

            // TODO This doesn't work for function "pointers"
            Symbol* sym = expr->call.func->kind == Expr::Name ? expr->call.func->name.symbol : nullptr;
            bool isForeign = (sym && sym->decl) ? IsForeign( sym->decl ) : false;

            OUTSTR( "( " );

            int givenIndex = 0, wantedIndex = 0;
            Array<FuncArg> const& wantedArgs = funcType->func.args;
            Array<Expr*> const& givenArgs = expr->call.args;

            while( givenIndex < givenArgs.count )
            {
                ASSERT( wantedIndex < wantedArgs.count );

                if( givenIndex || wantedIndex )
                    OUTSTR( ", " );

                Expr* argExpr = expr->call.args[givenIndex];
                Type* givenType = argExpr->resolvedExpr.type;

                FuncArg const& wantedArg = wantedArgs[wantedIndex];
                Type* wantedType = wantedArg.type;

                // Ensure types match
                if( wantedType != anyType && givenType != wantedType )
                {
                    ASSERT( wantedArg.isVararg );
                    wantedIndex++;
                    continue;
                }

                // Convert strings inside foreign function calls
                if( isForeign )
                {
                    if( givenType == stringType && !argExpr->resolvedExpr.isConst )
                        OUTSTR( "(char const*)" );
                }

                EmitExpr( argExpr, wantedType );

                givenIndex++;
                if( !wantedArg.isVararg )
                    wantedIndex++;
            }

            OUTSTR( " )" );
        } break;

        case Expr::Index:
            EmitExpr( expr->index.base );
            OUTSTR( "[" );
            EmitExpr( expr->index.index );
            OUTSTR( "]" );
            break;
        case Expr::Field:
            EmitExpr( expr->field.base );
            OUTSTR( "." );
            Out( expr->field.name );
            break;
        case Expr::Compound:
        {
            // Check if we need an inplace conversion to buffer
            bool convertToBuffer = false;
            if( expectedType && expectedType->kind == Type::Buffer && expr->resolvedExpr.type->kind == Type::Array )
            {
                OUTSTR( "BUFFER(ARGS(" );
                convertToBuffer = true;
            }
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

                int index = 0;
                for( CompoundField const& f : expr->compoundFields )
                {
                    if( f.kind == CompoundField::Index )
                        index = I32( f.index->resolvedExpr.constValue.intValue );

                    if( index >= capacity )
                    {
                        Expr** newBuffer = PUSH_ARRAY( &globalTmpArena, Expr*, 2 * capacity );
                        PCOPY( fieldExprs, newBuffer, Sz( capacity * sizeof(Expr*) ) );
                        capacity = 2 * capacity;

                        fieldExprs = newBuffer;
                    }

                    fieldExprs[index++] = f.initValue;
                }
                for( int i = 0; i < index; ++i )
                {
                    if( i != 0 )
                        OUTSTR( ", " );

                    Expr* e = fieldExprs[i];
                    if( e )
                        EmitExpr( e );
                    else
                        OUTSTR( "{}" );
                }
            }
            OUTSTR( " }" );
            if( convertToBuffer )
            {
                OUTSTR( "), " );
                Out( TypeToCdecl( expectedType->array.base, "" ) );
                OUTSTR( ")" );
            }
        } break;

        case Expr::Unary:
            Out( TokenKind::Items::names[expr->unary.op] );
            OUTSTR( "(" );
            EmitExpr( expr->unary.expr, expectedType );
            OUTSTR( ")" );
            break;
        case Expr::Binary:
            OUTSTR( "(" );
            EmitExpr( expr->binary.left );
            OUTSTR( " " );
            Out( TokenKind::Items::names[expr->binary.op] );
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

        INVALID_DEFAULT_CASE
    }
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

void EmitFuncDecl( Decl* decl )
{
    ASSERT( decl->kind == Decl::Func );

    char const* name = BuildQualifiedNameTmp( decl );

    if( decl->func.returnType )
        Out( TypeSpecToCdecl( decl->func.returnType, name ) );
    else
    {
        OUTSTR( "void " );
        Out( name );
    }
    OUTSTR( "(" );
    if( decl->func.args.count )
        OUTSTR( " " );
    for( FuncArgDecl const& a : decl->func.args )
    {
        if( &a != decl->func.args.begin() )
            OUTSTR( ", " );
        Out( TypeSpecToCdecl( a.type, a.name ) );
    }
    if( decl->func.args.count )
        OUTSTR( " " );
    OUTSTR( ")" );
}

void EmitForwardDecls()
{
    for( auto idx = globalOrderedSymbols.First(); idx; ++idx )
    {
        Symbol const* sym = *idx;
        Decl* decl = sym->decl;
        if( !decl )
            continue;

        switch( decl->kind )
        {
            case Decl::Struct:
            {
                OUTSTR( "struct " );
                Out( sym->name );
                OUTSTR( ";" );
                OutNL();
            } break;
            case Decl::Union:
            {
                OUTSTR( "union " );
                Out( sym->name );
                OUTSTR( ";" );
                OutNL();
            } break;
        }
    }
}

void EmitStmtBlock( StmtList const* block );

void EmitVarDecl( Decl* decl, char const* name, int exprIndex, bool nested )
{
    Type* resolvedType = decl->resolvedType; 

    OutIndent();
    if( decl->var.isConst )
    {
        //if( nested )
            OUTSTR( "static constexpr " );
        //else
            //OUTSTR( "const " );
    }

    TypeSpec* typeSpec = decl->var.type;
    if( typeSpec )
        Out( TypeSpecToCdecl( typeSpec, name ) );
    else
        Out( TypeToCdecl( resolvedType, name ) );

    OUTSTR( " = " );

    Expr* initExpr = decl->var.initExpr;
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

    OUTSTR( ";" );
    OutNL();
}

// TODO Remove 'nested' and link nodes to parent nodes so we have that info available here
void EmitDecl( Decl* decl, Symbol* symbol = nullptr, bool nested = false )
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
                    EmitVarDecl( decl, name, i, nested );
                    i++;
                }
            }
            else
            {
                ASSERT( symbol, "Need a (named) symbol for top-level variable declarations" );
                char const** name = decl->names.Find( symbol->name ); 
                ASSERT( name );

                int index = I32( name - decl->names.begin() );
                EmitVarDecl( decl, symbol->name, index, nested );
            }
        } break;
        case Decl::Struct:
        case Decl::Union:
        {
            OutIndent();
            if( decl->kind == Decl::Struct )
                OUTSTR( "struct " );
            else
                OUTSTR( "union " );
            Out( decl->names[0] );
            OutNL();
            OUTSTR( "{" );
            OutNL();

            globalIndent++;
            for( Decl* field : decl->aggregate.items )
            {
                EmitDecl( field, nullptr, true );
            }
            globalIndent--;
            OUTSTR( "};" );
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
            EmitDecl( stmt->decl, nullptr, true );
            break;
        case Stmt::Assign:
        {
            Expr* leftExpr = stmt->assign.left;
            Expr* rightExpr = stmt->assign.right;
            char const* opName = TokenKind::Items::names[stmt->assign.op];

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
            OutIndent();
            OUTSTR( "for( " );
            // TODO Array & string iterables
            Out( TypeToCdecl( stmt->for_.rangeExpr->range.lowerBound->resolvedExpr.type, stmt->for_.indexName ) );
            OUTSTR( " = " );
            EmitExpr( stmt->for_.rangeExpr->range.lowerBound );
            OUTSTR( "; " );
            Out( stmt->for_.indexName );
            OUTSTR( " < " );
            EmitExpr( stmt->for_.rangeExpr->range.upperBound );
            OUTSTR( "; ++" );
            Out( stmt->for_.indexName );
            OUTSTR( " )" );
            OutNL();
            EmitStmtBlock( stmt->for_.block );
            break;

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
                EmitExpr( stmt->expr );
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


    Out( globalPreamble );
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
