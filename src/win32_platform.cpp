#include <stdio.h>
#include <string.h>
#include <stdint.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include "mainloop.cpp"


PlatformAPI globalPlatform;
const sz PlatformAPI::PointerSize = 8;


PLATFORM_ALLOC(Win32Alloc)
{
    void* result = VirtualAlloc( 0, sizeBytes, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE );
    return result;
}

PLATFORM_FREE(Win32Free)
{
    VirtualFree( memoryBlock, 0, MEM_RELEASE );
}

PLATFORM_PRINT(Win32Print)
{
    va_list args;
    va_start( args, fmt );
    vfprintf( stdout, fmt, args );
    va_end( args );
}

PLATFORM_PRINT_VA(Win32PrintVA)
{
    vfprintf( stdout, fmt, args );
}

PLATFORM_PRINT(Win32Error)
{
    va_list args;
    va_start( args, fmt );
    vfprintf( stderr, fmt, args );
    va_end( args );

#if CONFIG_DEBUG
    __debugbreak();
#endif
}

PLATFORM_PRINT_VA(Win32ErrorVA)
{
    vfprintf( stderr, fmt, args );
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

PLATFORM_WRITE_ENTIRE_FILE(Win32WriteEntireFile)
{
    DWORD creationMode = CREATE_NEW;
#if CONFIG_DEBUG
    creationMode = CREATE_ALWAYS;
#endif
    HANDLE outFile = CreateFile( filename, GENERIC_WRITE, 0, NULL,
                                 creationMode, FILE_ATTRIBUTE_NORMAL, NULL ); 
    if( outFile == INVALID_HANDLE_VALUE )
    {
        globalPlatform.Error( "Could not open '%s' for writing", filename );
        return;
    }

    for( int i = 0; i < chunks.count; ++i )
    {
        Buffer const& chunk = chunks[i];
        SetFilePointer( outFile, 0, NULL, FILE_END );

        DWORD bytesWritten;
        if( !WriteFile( outFile, chunk.data, U32( chunk.size ), &bytesWritten, NULL ) )
        {
            globalPlatform.Error( "Failed writing %d bytes to '%s'", chunk.size, filename );
            return;
        }
    }
}

ASSERT_HANDLER(DefaultAssertHandler)
{
    // TODO Logging
    // TODO Stacktrace
    char buffer[256] = {};

    va_list args;
    va_start( args, msg );
    vsnprintf( buffer, ARRAYCOUNT(buffer), msg, args );
    va_end( args );

    fprintf( stderr, "ASSERTION FAILED! :: \"%s\" (%s@%d)\n", buffer, file, line );
}


int main( int argCount, char const* args[] )
{
    // Init global platform
    globalPlatform = {};
    globalPlatform.Alloc = Win32Alloc;
    globalPlatform.Free = Win32Free;
    globalPlatform.ReadEntireFile = Win32ReadEntireFile;
    globalPlatform.WriteEntireFile = Win32WriteEntireFile;
    globalPlatform.Print = Win32Print;
    globalPlatform.Error = Win32Error;
    globalPlatform.PrintVA = Win32PrintVA;
    globalPlatform.ErrorVA = Win32ErrorVA;

    Run( argCount, args );

    return 0;
}
