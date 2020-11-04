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


static char const* testStrings[] =
{
    "a: int = 42;",
};

void RunTests()
{
    // Init intern strings
    InitArena( &globalInternStrings.arena );
    new (&globalInternStrings.entries) BucketArray<InternString>( &globalArena, 1024 );

    for( char const* stream : testStrings )
    {
        Parse( String( stream ), "" );
    }

    // TODO Tear down
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
    Buffer readResult = globalPlatform.ReadEntireFile( filename_str, globalTmpArena );

    Parse( String( readResult ), filename_str );
#endif
}
