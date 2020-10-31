#include <new>
#include <math.h>

#include "common.h"
#include "platform.h"
#include "intrinsics.h"
#include "math.h"
#include "memory.h"
#include "datatypes.h"
#include "string.h"
#include "lexer.h"

InternStringBuffer globalInternStrings;

#include "lexer.cpp"
#include "parser.cpp"

void Run( Array<String> const& argsList, MemoryArena* globalArena, MemoryArena* tmpArena )
{
    int bla = 42;
    ASSERT( false, "TEST MESSAGE", bla );

    // Parse arguments
    if( argsList.count == 0 )
    {
        globalPlatform.Error( "Need an input file" );
        return;
    }

    // Init intern strings
    InitArena( &globalInternStrings.arena );
    new (&globalInternStrings.entries) BucketArray<InternString>( globalArena, 1024 );

    String inputFilename = argsList[0];
    char const* filename_str = inputFilename.CString( tmpArena );
    // TODO Temp memory scopes
    Buffer readResult = globalPlatform.ReadEntireFile( filename_str, tmpArena );

    Parse( String( readResult ), filename_str );
}
