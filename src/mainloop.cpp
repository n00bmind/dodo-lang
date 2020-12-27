#include <new>
#include <stdio.h>
#include <stdint.h>
#include <float.h>
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
int globalErrorCount = 0;


#include "common.cpp"
#include "lexer.cpp"
#include "parser.cpp"
#include "resolver.cpp"
#include "codegen_c.cpp"

//#include "../test2.do.cpp"


void InitInternStrings()
{
    InitArena( &globalInternStrings.arena );
    // TODO Can we get rid of this after parsing and just keep the arena?
    INIT( globalInternStrings.entries ) Hashtable<String, InternString, MemoryArena>(
                &globalArena, 0, 0, StringHash, StringsEqual );
}

void InitTestMemory()
{
    ClearArena( &globalArena );
    ClearArena( &globalTmpArena );
    ClearArena( &globalOutArena );
    ClearArena( &globalInternStrings.arena );

    InitInternStrings();
}

int CountSymbols( Array<Decl*> const& fileDecls )
{
    int result = 0;
    for( Decl* d : fileDecls )
        result += d->names.count;

    return result;
}

void RunTests()
{
    TestHashtable();

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

        Lexer lexer;
        Expr* expr = nullptr;

        InitTestMemory();

        for( TestString const& t : testExprStrings )
        {
            char buf[256] = {};

            lexer = Lexer( String( t.expr ), "<tests>" );
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

    for( i in 0..array.count ) //; i *= 2; j :: i*4 )
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

            lexer = Lexer( String( t.expr ), "<tests>" );
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
        "h :: (hx: int) -> int { if(hx) { return -hx; } else { return 1; } }",
    };

    {
        InitTestMemory();

        Array<Decl*> globalDecls( &globalArena, ARRAYCOUNT(testDeclStrings) );
        for( int i = 0; i < ARRAYCOUNT(testDeclStrings); ++i )
        {
            Lexer lexer = Lexer( String( testDeclStrings[i] ), "<tests>" );
            Decl* decl = ParseDecl( &lexer );
            globalDecls.Push( decl );
        }

        int globalSymbolsCount = CountSymbols( globalDecls );
        // Plus 1 type symbol pushed below
        InitResolver( globalSymbolsCount + 1 );

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

        ResolveAll( globalDecls );

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

        Array<Decl*> globalDecls( &globalArena, ARRAYCOUNT(testDeclStrings) );
        for( int i = 0; i < ARRAYCOUNT(testDeclStrings); ++i )
        {
            Lexer lexer = Lexer( String( testDeclStrings[i] ), "<tests>" );
            Decl* decl = ParseDecl( &lexer );
            globalDecls.Push( decl );
        }

        int globalSymbolsCount = CountSymbols( globalDecls );
        InitResolver( globalSymbolsCount );

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

        ResolveAll( globalDecls );
        GenerateAll();
        String outString( (char*)globalOutArena.base, I32( globalOutArena.used ) );
        //printf( "%.*s", outString.length, outString.data );

        //__debugbreak();
    }

    InitTestMemory();
}

f64 globalTimeMillis = 0;

f64 ElapsedTimeMillis()
{
    f64 time = globalPlatform.CurrentTimeMillis();
    f64 result = time - globalTimeMillis;
    globalTimeMillis = time;

    return result;
}

char globalCompilerCmdLine[2048] = {};

char const* BuildCmdLine( char const* compilerName, char const* filename, Array<char const*> const& commonFlags,
                          Array<char const*> const& configFlags, Array<char const*> const& linkerFlags,
                          char const* outputPath )
{
    char* out = globalCompilerCmdLine;

#define STROUT( s ) \
    String s##Str( s ); \
    s##Str.CopyTo( out );\
    out += s##Str.length;

    STROUT( compilerName );
    *out++ = ' ';
    STROUT( filename );

    for( char const* f : commonFlags )
    {
        *out++ = ' ';
        STROUT( f );
    }

    for( char const* f : configFlags )
    {
        *out++ = ' ';
        STROUT( f );
    }

    char const* objFlag = " -Fo";
    STROUT( objFlag );
    STROUT( outputPath );
    char const* exeFlag = " -Fe";
    STROUT( exeFlag );
    char const* outputPath2 = outputPath;
    STROUT( outputPath2 );

    for( char const* f : linkerFlags )
    {
        *out++ = ' ';
        STROUT( f );
    }
#undef STROUT

    *out = 0;
    return globalCompilerCmdLine;
}

bool Run( int argCount, char const* args[] )
{
    InitArena( &globalArena, MEGABYTES(16) );
    InitArena( &globalTmpArena, MEGABYTES(16) );
    InitArena( &globalOutArena, MEGABYTES(16) );

#if !CONFIG_RELEASE
    RunTests();
    if( globalErrorCount )
    {
        globalPlatform.Error( "Tests found %d errors.", globalErrorCount );
        return false;
    }
#endif

    f64 startTime = globalPlatform.CurrentTimeMillis();

    Array<String> argsList( &globalArena, argCount );
    // Skip exe path from args list
    for( int i = 1; i < argCount; ++i )
        argsList.Push( String( args[i] ) );

    // Parse arguments
    if( argsList.count == 0 )
    {
        globalPlatform.Error( "Need an input file" );
        return false;
    }

    InitInternStrings();

    String inputFilename( argsList[0] );
    char absPath[256] = {};
    if( !globalPlatform.GetAbsolutePath( inputFilename.data, absPath, ARRAYCOUNT(absPath) ) )
        return false;

    // TODO Temp memory scopes
    Buffer readResult = globalPlatform.ReadEntireFile( absPath, &globalTmpArena );
    if( !readResult.data )
        return false;

    // Do work!
    f64 parseTime = 0, resolveTime = 0, generateTime = 0, compileTime = 0;
    globalTimeMillis = globalPlatform.CurrentTimeMillis();

    Array<Decl*> fileDecls = Parse( String( readResult ), absPath );
    parseTime = ElapsedTimeMillis();

    if( !globalErrorCount )
    {
        int globalSymbolsCount = CountSymbols( fileDecls );
        InitResolver( globalSymbolsCount );

        ResolveAll( fileDecls );
        resolveTime = ElapsedTimeMillis();

        if( !globalErrorCount )
        {
            GenerateAll();
            generateTime = ElapsedTimeMillis();
        }
    }

    if( globalErrorCount )
    {
        globalPlatform.Error( "\nCompilation found %d errors.", globalErrorCount );
        return false;
    }
    else
    {
        // Collect all output arena pages in order and write them to disk
        Array<Buffer> pages( &globalArena, globalOutArena.pageCount );
        pages.ResizeToCapacity();

        MemoryArena arena = globalOutArena;
        for( int i = pages.count - 1; i >= 0; --i )
        {
            pages[i].data = arena.base;
            pages[i].size = arena.used;

            if( i != 0 )
            {
                MemoryArenaHeader* header = GetArenaHeader( &arena );
                arena.base = header->base;
                arena.size = header->size;
                arena.used = header->used;
            }
        }

        // Write intermediate C file
        // TODO Cmdline switch for output dir
        char cppFilePath[256] = {};
        inputFilename.CopyTo( cppFilePath );

        int extIndex = inputFilename.FindLast( '.' );
        if( extIndex == -1 )
            extIndex = inputFilename.length;

        sz available = ARRAYCOUNT(cppFilePath) - extIndex;
        ASSERT( available >= sizeof(".cpp") );
        StringCopy( ".cpp", cppFilePath + extIndex, available );

        if( !globalPlatform.WriteEntireFile( cppFilePath, pages ) )
            return false;

        // Execute C compiler
        // TODO Cmdline switch & env. variable for C compiler location
        static char const* compilerName = "cl.exe";

//#define ARRAY( name, type, count ) \
        //extern type _##name##_items[count]; \
        //Array<type> name( _##name##_items, ARRAYCOUNT(_##name##_items) ); \
        //type _##name##_items[count] =

        //ARRAY( commonFlags, char const*, 9 )
        char const* commonFlags[] =
        {
            "-nologo", "-FC", "-Wall", "-WX", "-Oi", "-GR-", "-EHa-",
            "-D_HAS_EXCEPTIONS=0", "-D_CRT_SECURE_NO_WARNINGS",
            "-wd4100",          // Unreferenced parameter
            "-wd4189",          // Initialized but unreferenced local variable
            "-wd4514",          // Unreferenced inline function removed
            "-wd4668",          // Undefined preprocessor macro
            "-wd4710",          // Function not inlined
            "-wd4820",          // Padding added
        };
        char const* commonLinkerFlags[] =
        {
            "/link", "/opt:ref", "/incremental:no", "/debug:full",
        };

        char const* debugFlags[] =
        {
            "-DCONFIG_DEBUG=1", "-Z7", "-MTd", "-Od",
        };
        char const* releaseFlags[] =
        {
            "-DCONFIG_RELEASE=1", "-Z7", "-MT", "-O2",
        };

        bool debug = true;
        Array<char const*> configFlags = debug
            ? Array<char const*>( debugFlags, ARRAYCOUNT(debugFlags) )
            : Array<char const*>( releaseFlags, ARRAYCOUNT(releaseFlags) );

        // Output directory
        char outPath[256] = {};
        inputFilename.CopyTo( outPath );
        int sepIndex = inputFilename.FindLast( '\\' );
        if( sepIndex == -1 )
            sepIndex = inputFilename.FindLast( '/' );

        sepIndex++;
        outPath[sepIndex] = '\0';

        char const* cmdLine = BuildCmdLine( compilerName,
                                            cppFilePath,
                                            Array<char const*>( commonFlags, ARRAYCOUNT(commonFlags) ),
                                            configFlags,
                                            Array<char const*>( commonLinkerFlags, ARRAYCOUNT(commonLinkerFlags) ),
                                            outPath );
        
        // TODO Console colors
        globalPlatform.Print( cmdLine );
        globalPlatform.Print( "\n\n" );
        int exitCode = globalPlatform.ShellExecute( cmdLine );
        compileTime = ElapsedTimeMillis();

        // Stats
        if( exitCode == 0 )
        {
            f64 totalTime = globalPlatform.CurrentTimeMillis() - startTime;
            globalPlatform.Print( "Compilation took %.3f seconds.\n", totalTime * 0.001f );
            globalPlatform.Print( "Parsing:       %.3f s.\n", parseTime * 0.001f );
            globalPlatform.Print( "Resolving:     %.3f s.\n", resolveTime * 0.001f );
            globalPlatform.Print( "C codegen:     %.3f s.\n", generateTime * 0.001f );
            globalPlatform.Print( "C compilation: %.3f s.\n", compileTime * 0.001f );
            globalPlatform.Print( "\nDONE." );
        }

        return exitCode == 0;
    }
}
