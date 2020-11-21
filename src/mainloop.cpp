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
InternStringBuffer globalInternStrings;
bool globalError = false;

#include "common.cpp"
#include "lexer.cpp"
#include "parser.cpp"
#include "resolver.cpp"


void InitTestMemory()
{
    ClearArena( &globalArena );
    ClearArena( &globalInternStrings.arena );

    // Init intern strings
    InitArena( &globalInternStrings.arena );
    new (&globalInternStrings.entries) BucketArray<InternString>( &globalArena, 1024 );
}

void RunTests()
{
    static char const* testTokenStrings[] =
    {
        "fact\n('num\v', 0x2a)//this is a comment\na.b .111<<=11234> 1.234E-100abc {}*: sizeof*=0o52 &= &&/*another comment*/\r\n\tstruct 0b101010",
    };
#define ASSERT_TOKEN(k) \
        token = NextToken( &lexer ); \
        ASSERT( token.kind == TokenKind::k );
    {
        InitTestMemory();
        Lexer lexer = Lexer( String( testTokenStrings[0] ), "" );
        Token token;

        ASSERT_TOKEN(Name);
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
    }
#undef ASSERT_TOKEN

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

            globalPlatform.Print( "%s\n", buf );
            ASSERT( StringsEqual( t.sExpr, buf ) );
        }
        globalPlatform.Print( "\n" );
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

            globalPlatform.Print( "%s\n", buf );
            //ASSERT( StringsEqual( t.sExpr, buf ) );
        }
    }

    {
        InitTestMemory();
        InitResolver();

        Type* intPtr = NewPtrType( intType );
        ASSERT( NewPtrType( intType ) == intPtr );
        Type* intPtrPtr = NewPtrType( NewPtrType( intType ) );
        ASSERT( NewPtrType( NewPtrType( intType ) ) == intPtrPtr );
        ASSERT( intPtrPtr != intPtr );
        Type* float4Array = NewArrayType( floatType, 4 );
        ASSERT( NewArrayType( floatType, 4 ) == float4Array );
        BucketArray<Type*> args( &globalTmpArena, 1 );
        args.Push( intType );
        Type* intIntFunc = NewFuncType( args, intType );
        ASSERT( NewFuncType( args, intType ) == intIntFunc );

        InternString* fooIntern = Intern( String( "foo" ) );
        char const* foo = fooIntern->data;
        ASSERT( GetSymbol( foo ) == nullptr );
        Decl nilDecl = {};
        nilDecl.name = foo;
        PushSymbol( &nilDecl );
        ASSERT( GetSymbol( foo ) && GetSymbol( foo )->decl == &nilDecl );

        static char const* testDeclStrings[] =
        {
            "n :: 1 + sizeof(p)",
            "p : *T",
            "u := *p",
            "struct T { a: [n]int; }",
            "r := &t.a",
            "t: T",
            "s: [n+m]int",
            "m :: sizeof(t.a)",
            "i := n + m",
            "q = &i",
            "sx :: sizeof(x)",
            "x: R",
            "struct R { s: *S; }",
            "struct S { r: [sx]R; }",
        };

        for( int i = 0; i < ARRAYCOUNT(testDeclStrings); ++i )
        {
            Lexer lexer = Lexer( String( testDeclStrings[i] ), "" );
            Decl* decl = ParseDecl( &lexer );
            PushSymbol( decl );
        }
        auto idx = globalSymbolList.First();
        while( idx )
        {
            ResolveSymbol( *idx );
            idx.Next();
        }
        auto idx2 = globalOrderedSymbols.First();
        while( idx2 )
        {
            Symbol* sym = *idx2;
            if( sym->decl )
                printf( "Declare %s\n", sym->decl->name );
            else
                printf( "%s\n", sym->name );
        }
    }
    __debugbreak();
}

void Run( Array<String> const& argsList )
{
    // Parse arguments
    if( argsList.count == 0 )
    {
        globalPlatform.Error( "Need an input file" );
        return;
    }

#if !CONFIG_RELEASE
    RunTests();
#endif

#if 0
    // Init intern strings
    InitArena( &globalInternStrings.arena );
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
