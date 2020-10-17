#include <stdio.h>
#include <string.h>
#include <stdint.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include "common.h"
#include "intrinsics.h"
#include "memory.h"
#include "platform.h"
#include "datatypes.h"
#include "string.h"


ASSERT_HANDLER(DefaultAssertHandler)
{
    // TODO Logging
    // TODO Stacktrace
    printf( "ASSERTION FAILED! :: '%s' (%s@%d)\n", msg, file, line );
}


int main( int argCount, char const* args[] )
{
    void* baseAddress = (void*)GIGABYTES(2048);
    // TODO Use MEM_LARGE_PAGES and call AdjustTokenPrivileges when not in XP
    // TODO Reserve only and commit in chunks
    sz globalMemorySize = GIGABYTES(2);
    void* globalMemoryBlock = VirtualAlloc( baseAddress, globalMemorySize, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE );

    MemoryArena globalArena;
    InitArena( &globalArena, globalMemoryBlock, globalMemorySize );

#if 0
    String argString( args );
    BucketArray<String> result( &globalArena, 16 );
    argString.Split( ' ', &result );

    auto idx = result.First();
    while( idx )
    {
        printf( "%s\n", (*idx).data );
        idx.Next();
    }
#endif
    Array<String> argsList( &globalArena, argCount );
    for( int i = 0; i < argCount; ++i )
    {
        argsList.Push( String( args[i] ) );
        printf( "%s\n", argsList.Last().data );
    }

    return 0;
}
