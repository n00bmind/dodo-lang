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
bool globalRunning = true;

#include "lexer.cpp"
#include "parser.cpp"


static char const* testTokenStrings[] =
{
    "fact\n('num\v', 0x2a)//this is a comment\na.b .111<<=11234> 1.234E-100abc {}*: sizeof*=0o52 &= &&/*another comment*/\r\n\tstruct 0b101010",
};
static char const* testExprStrings[] =
{
    "a+b",
};
static char const* testDeclStrings[] =
{
    "a: int = 42;",
};

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

    {
        int i = 0;
        Lexer lexer;
        Expr* expr = nullptr;

        InitTestMemory();

        lexer = Lexer( String( testExprStrings[i++] ), "" );
        NextToken( &lexer );
        expr = ParseExpr( &lexer );
        ASSERT( expr->kind == Expr::Binary );

        int a = 42;
    }

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

    String inputFilename = argsList[0];
    char const* filename_str = inputFilename.CString( globalTmpArena );
    // TODO Temp memory scopes
    // FIXME Null terminate!
    Buffer readResult = globalPlatform.ReadEntireFile( filename_str, globalTmpArena );

    Parse( String( readResult ), filename_str );
#endif
}
