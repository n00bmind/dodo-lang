#include <new>
#include <math.h>

#include "magic.h"
#include "common.h"
#include "platform.h"
#include "intrinsics.h"
#include "math.h"
#include "memory.h"
#include "datatypes.h"
#include "string.h"
#include "lexer.h"
#include "ast.h"

MemoryArena globalArena;
MemoryArena globalTmpArena;
MemoryArena globalOutArena;
InternStringBuffer globalInternStrings;
bool globalError = false;

#include "common.cpp"
#include "lexer.cpp"
#include "parser.cpp"
#include "resolver.cpp"
#include "codegen_c.cpp"


void InitTestMemory()
{
    ClearArena( &globalArena );
    ClearArena( &globalTmpArena );
    ClearArena( &globalOutArena );
    ClearArena( &globalInternStrings.arena );

    // Init intern strings
    InitArena( &globalInternStrings.arena );
    new (&globalInternStrings.entries) BucketArray<InternString>( &globalArena, 1024 );
}

void RunTests()
{
    {
        static char const* testTokenStrings[] =
        {
            "fact\n('num\v', 0x2a)//this is a comment\na.b .111<<=11234> 1.234E-100abc {}*: sizeof*=0o52 &= &&/*another comment*/\r\n\tstruct 0b101010",
        };
#define ASSERT_TOKEN(k) \
            token = NextToken( &lexer ); \
            ASSERT( token.kind == TokenKind::k );

        InitTestMemory();
        Lexer lexer = Lexer( String( testTokenStrings[0] ), "" );
        Token token;

        // First token was already eaten
        ASSERT(lexer.token.kind == TokenKind::Name);
        ASSERT_TOKEN(OpenParen);
        ASSERT_TOKEN(StringLiteral);
        ASSERT( StringsEqual( token.strValue, "num\v" ) );
        ASSERT_TOKEN(Comma);
        ASSERT_TOKEN(IntLiteral);
        ASSERT( token.intValue == 42 && token.mod == Token::Hexadecimal );
        ASSERT_TOKEN(CloseParen);
        ASSERT_TOKEN(Name);
        ASSERT_TOKEN(Dot);
        ASSERT_TOKEN(Name);
        ASSERT_TOKEN(FloatLiteral);
        ASSERT( token.floatValue == 0.111 );
        ASSERT_TOKEN(LShiftAssign);
        ASSERT_TOKEN(IntLiteral);
        ASSERT( token.intValue == 11234 );
        ASSERT_TOKEN(GreaterThan);
        ASSERT_TOKEN(FloatLiteral);
        ASSERT( token.floatValue == 1.234e-100 );
        ASSERT_TOKEN(Name);                         // NOTE This should probably be an error!
        ASSERT_TOKEN(OpenBrace);
        ASSERT_TOKEN(CloseBrace);
        ASSERT_TOKEN(Asterisk);
        ASSERT_TOKEN(Colon);
        ASSERT_TOKEN(Keyword);
        ASSERT_TOKEN(MulAssign);
        ASSERT_TOKEN(IntLiteral);
        ASSERT( token.intValue == 42 && token.mod == Token::Octal );
        ASSERT_TOKEN(AndAssign);
        ASSERT_TOKEN(LogicAnd);
        ASSERT_TOKEN(Keyword);
        ASSERT_TOKEN(IntLiteral);
        ASSERT( token.intValue == 42 && token.mod == Token::Binary );
#undef ASSERT_TOKEN
    }

    struct TestString
    {
        char const* expr;
        char const* sExpr;
    }; 

    {
        static TestString testExprStrings[] =
        {
            { "a.val+b[5]+10", "(+ (+ (. a val) ([] b 5)) 10)" },
            { "func('test!', {1.5, 3.0})", "(call func ('test!', ({} 1.5, 3)))" },
            { "a*b+c", "(+ (* a b) c)" },
            { "a+b*c", "(+ a (* b c))" },
            { "(a+b)*-c", "(* (+ a b) (- c))" },
            { "a && b || c ? (&func)() : &func", "((|| (&& a b) c) ? (call (& func) ()) : (& func))" },
            { "(*names.First()).ident", "(. (* (call (. names First) ())) ident)" },
        };

        int i = 0;
        Lexer lexer;
        Expr* expr = nullptr;

        InitTestMemory();

        for( TestString const& t : testExprStrings )
        {
            char buf[256] = {};

            lexer = Lexer( String( t.expr ), "" );
            expr = ParseExpr( &lexer );

            char* outBuf = buf;
            sz len = ARRAYCOUNT(buf);
            int indent = 0;
            DebugPrintSExpr( expr, outBuf, len );

            //globalPlatform.Print( "%s\n", buf );
            ASSERT( StringsEqual( t.sExpr, buf ) );
        }
        //globalPlatform.Print( "\n" );
    }

    {
        // TODO Auto compare strings
        static TestString testDeclStrings[] =
        {
            { "a: int = 42;", "" },
            { "a := 42;", "" },
            { "a:: 42;", "" },
            { "f : (int) -> int = &factorial;", "" },
            { "Fruit :: enum { Lemon, Pineapple = 42, Kiwi, }", "" },
            { "Mesh :: struct { vertices: [] v3; indices: []u32; material: *Material; } ", "" },
            { "factorial :: (x: int) -> int { }", "" },
            { R"STR(
complex_func :: ()
{
    a, b, c :: 10, 1.0, 'whatever';
    a, b, c := some_func();
    a, b, c = some_func(1, 2, 3), b+c, x > 10 ? 1 : 0;
    a, b, c += d, {e1, e2, e3}, f; //?

    if( i == 0 )
        break;
    else if( bla() )
    {
        have_one;
        have_two;
    }
    else
        MoarStuff();

    for( i :: 0..array.count; i *= 2; j :: i*4 )
    {
        lotsa;
        stmts;
    }

    do
    {
        factorial ( a );
        a += 1;
    }
    while( a < i );

    switch( array.count )
    case 0:
    {
        a += 0;
    }
    case 1:
        could_be_anything;
    default:
    {
        probably_the_end( array.count );
    }
})STR",
              ""
            }
        };

        Lexer lexer;
        Decl* decl = nullptr;

        InitTestMemory();

        for( TestString const& t : testDeclStrings )
        {
            char buf[16768] = {};

            lexer = Lexer( String( t.expr ), "" );
            decl = ParseDecl( &lexer );

            char* outBuf = buf;
            sz len = ARRAYCOUNT(buf);
            int indent = 0;
            DebugPrintSExpr( decl, outBuf, len, indent );

            //globalPlatform.Print( "%s\n", buf );
            //ASSERT( StringsEqual( t.sExpr, buf ) );
        }
    }

    static char const* testDeclStrings[] =
    {
        "n :: 1 + sizeof(p);",
        "p : *T;",
        "u := *p;",
        "T :: struct { a: [n] int; };",
        "r := &t.a;",
        "t: T;",
        "s: [n+m]int = { 0, 1, 2 };",
        "m :: sizeof(t.a);",
        "i := n + m;",
        "q := &i;",
        "sx :: sizeof(x);",
        "x: R;",
        "R :: struct { s: *S; };",
        "S :: struct { r: [sx] R; };",
        "U :: struct { a: [3] int; };",
        "uu: U = { { 0 } };",
        "a, b, c :: 0;",
        //"b := 1;",                                  // Redeclared
        "add :: (v: Vector, w: Vector) -> Vector { return { v.x + w.x, v.y + w.y }; }",
        "result := add_func( { 1, 2 }, { 3, 4 } );",
        "add_func := add;",
        "Vector :: struct { x, y: int; }",
        // TODO Check this actually works for all cond types
        "it := 1 ? 2 : 3;",
        "ptr := &s[1 + 1];",
        // TODO Do we want to disallow indexing pointers (unchecked) so it feels less safe than indexing arrays (always checked)?
        "item := ptr[-1];",
        "bin := 1000 / (2 + 3 * 5) << 10;",
        "aptr: *int = -s[3] as *int;",
        "f :: () { i += 1; }",
        "h :: (x: int) -> int { if(x) { return -x; } else { return 1; } }",
    };

    {
        InitTestMemory();
        InitResolver();

        Type* intPtr = NewPtrType( intType );
        ASSERT( NewPtrType( intType ) == intPtr );
        Type* intPtrPtr = NewPtrType( NewPtrType( intType ) );
        ASSERT( NewPtrType( NewPtrType( intType ) ) == intPtrPtr );
        ASSERT( intPtrPtr != intPtr );
        //Type* float4Array = NewArrayType( floatType, 4 );
        //ASSERT( NewArrayType( floatType, 4 ) == float4Array );
        Array<Type*> args( &globalTmpArena, 1 );
        args.Push( intType );
        Type* intIntFunc = NewFuncType( args, intType );
        ASSERT( NewFuncType( args, intType ) == intIntFunc );

        InternString* fooIntern = Intern( String( "foo" ) );
        char const* foo = fooIntern->data;
        ASSERT( GetSymbol( foo ) == nullptr );
        PushGlobalTypeSymbol( foo, intType );
        ASSERT( GetSymbol( foo ) && GetSymbol( foo )->type == intType );

        for( int i = 0; i < ARRAYCOUNT(testDeclStrings); ++i )
        {
            Lexer lexer = Lexer( String( testDeclStrings[i] ), "" );
            Decl* decl = ParseDecl( &lexer );
            PushGlobalDeclSymbols( decl );
        }
        {
            auto idx = globalSymbolList.First();
            while( idx )
            {
                Symbol* sym = &*idx;

#if 0
                if( sym->decl )
                {
                    char buf[1024] = {};
                    char* outBuf = buf;
                    sz len = ARRAYCOUNT(buf);
                    int indent = 0;

                    DebugPrintSExpr( sym->decl, outBuf, len, indent );
                    printf( "Completing %s\n", buf );
                }
#endif

                CompleteSymbol( sym );
                idx.Next();
            }
        }

        //printf( "\n-----------------------\n" );
        auto idx2 = globalOrderedSymbols.First();
        while( idx2 )
        {
            Symbol* sym = *idx2;

            if( sym->decl )
            {
                char buf[1024] = {};
                char* outBuf = buf;
                sz len = ARRAYCOUNT(buf);
                int indent = 0;

                DebugPrintSExpr( sym->decl, outBuf, len, indent );
                //printf( "Declare %s\n", buf );
            }
            //else
                //printf( "%s\n", sym->name );

            idx2.Next();
        }
    }

    {
        InitTestMemory();
        InitResolver();

        char* cdecl1 = TypeToCdecl( intType, "x" );
        char* cdecl2 = TypeToCdecl( NewPtrType( intType ), "x" );
        char* cdecl3 = TypeToCdecl( NewArrayType( intType, 10 ), "x" );
        Array<Type*> args( &globalTmpArena, 2 );
        args.Push( NewPtrType( intType ) );
        args.Push( intType );
        char* cdecl4 = TypeToCdecl( NewFuncType( args, intType ), "x" );
        char* cdecl5 = TypeToCdecl( NewArrayType( NewFuncType( args, intType ), 10 ), "x" );
        Array<Type*> emptyArgs( &globalTmpArena, 0 );
        char* cdecl6 = TypeToCdecl( NewFuncType( emptyArgs, NewArrayType( NewFuncType( emptyArgs, intType ), 10 ) ), "x" );

        for( int i = 0; i < ARRAYCOUNT(testDeclStrings); ++i )
        {
            Lexer lexer = Lexer( String( testDeclStrings[i] ), "" );
            Decl* decl = ParseDecl( &lexer );
            PushGlobalDeclSymbols( decl );
        }
        for( auto idx = globalSymbolList.First(); idx; ++idx )
        {
            Symbol* sym = &*idx;
            CompleteSymbol( sym );
        }

        GenerateAll();
        String outString( (char*)globalOutArena.base, I32( globalOutArena.used ) );
        printf( "%.*s", outString.length, outString.data );

        __debugbreak();
    }

}

void Run( int argCount, char const* args[] )
{
    InitArena( &globalArena, MEGABYTES(16) );
    InitArena( &globalTmpArena, MEGABYTES(16) );
    InitArena( &globalOutArena, MEGABYTES(16) );
    InitArena( &globalInternStrings.arena );

#if !CONFIG_RELEASE
    RunTests();
#endif

#if 0
    Array<String> argsList( &globalArena, argCount );
    // Skip exe path from args list
    for( int i = 1; i < argCount; ++i )
        argsList.Push( String( args[i] ) );

    // Parse arguments
    if( argsList.count == 0 )
    {
        globalPlatform.Error( "Need an input file" );
        return;
    }

    // Init intern strings
    new (&globalInternStrings.entries) BucketArray<InternString>( &globalArena, 1024 );

    InitResolver();

    String inputFilename = argsList[0];
    char const* filename_str = inputFilename.CString( globalTmpArena );
    // TODO Temp memory scopes
    // FIXME Null terminate!
    Buffer readResult = globalPlatform.ReadEntireFile( filename_str, globalTmpArena );

    Parse( String( readResult ), filename_str );
#endif
}
