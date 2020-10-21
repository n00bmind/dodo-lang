#include <stdio.h>
#include <string.h>
#include <stdint.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include "common.h"
#include "intrinsics.h"
#include "memory.h"
#include "datatypes.h"
#include "platform.h"
#include "string.h"

#include "lexer.cpp"
#include "mainloop.cpp"


PlatformAPI globalPlatform;


PLATFORM_PRINT(Win32Print)
{
    va_list args;
    va_start( args, fmt );
    vfprintf( stdout, fmt, args );
    va_end( args );
}

PLATFORM_PRINT(Win32Error)
{
    va_list args;
    va_start( args, fmt );
    vfprintf( stderr, fmt, args );
    va_end( args );
}

PLATFORM_READ_ENTIRE_FILE(Win32ReadEntireFile)
{
    Buffer result = {};

    HANDLE fileHandle = CreateFile( filename, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0 );
    if( fileHandle != INVALID_HANDLE_VALUE )
    {
        LARGE_INTEGER fileSize;
        if( GetFileSizeEx( fileHandle, &fileSize ) )
        {
            u32 fileSize32 = U32( (u64)fileSize.QuadPart );
            result.data = PUSH_SIZE( arena, fileSize32 + 1 );

            if( result.data )
            {
                DWORD bytesRead;
                if( ReadFile( fileHandle, result.data, fileSize32, &bytesRead, 0 )
                    && (fileSize32 == bytesRead) )
                {
                    // Null-terminate to help when handling text files
                    *((u8 *)result.data + fileSize32) = '\0';
                    result.size = fileSize32 + 1;
                }
                else
                {
                    globalPlatform.Error( "ERROR: ReadFile failed for '%s'", filename );
                    result.data = 0;
                }
            }
            else
            {
                globalPlatform.Error( "ERROR: Couldn't allocate buffer for file contents" );
            }
        }
        else
        {
            globalPlatform.Error( "ERROR: Failed querying file size for '%s'", filename );
        }

        CloseHandle( fileHandle );
    }
    else
    {
        globalPlatform.Error( "ERROR: Failed opening file '%s' for reading", filename );
    }

    return result;
}

ASSERT_HANDLER(DefaultAssertHandler)
{
    // TODO Logging
    // TODO Stacktrace
    printf( "ASSERTION FAILED! :: '%s' (%s@%d)\n", msg, file, line );
}


int main( int argCount, char const* args[] )
{
    // Init global platform
    globalPlatform = {};
    globalPlatform.Print = Win32Print;
    globalPlatform.Error = Win32Error;
    globalPlatform.ReadEntireFile = Win32ReadEntireFile;

#if CFG_DEBUG
    void* globalBaseAddress = (void*)GIGABYTES(2048);
    void* tmpBaseAddress = (void*)GIGABYTES(3048);
#else
    void* globalBaseAddress = 0;
    void* tmpBaseAddress = 0;
#endif
    // TODO Use MEM_LARGE_PAGES and call AdjustTokenPrivileges when not in XP
    // TODO Reserve only and commit in chunks
    sz globalMemorySize = GIGABYTES(1);
    void* globalMemoryBlock = VirtualAlloc( globalBaseAddress, globalMemorySize, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE );
    sz tmpMemorySize = GIGABYTES(1);
    void* tmpMemoryBlock = VirtualAlloc( tmpBaseAddress, tmpMemorySize, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE );

    MemoryArena globalArena;
    InitArena( &globalArena, globalMemoryBlock, globalMemorySize );
    MemoryArena tmpArena;
    InitArena( &tmpArena, tmpMemoryBlock, tmpMemorySize );

    Array<String> argsList( &globalArena, argCount );
    // Skip exe path from args list
    for( int i = 1; i < argCount; ++i )
        argsList.Push( String( args[i] ) );


    Run( argsList, &globalArena, &tmpArena );

    return 0;
}
