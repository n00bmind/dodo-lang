#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <DbgHelp.h>

#include "mainloop.cpp"


PlatformAPI globalPlatform;

internal f64 globalPerfCounterFrequency;


PLATFORM_ALLOC(Win32Alloc)
{
    void* result = VirtualAlloc( 0, Size( sizeBytes ), MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE );
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
}

PLATFORM_PRINT_VA(Win32ErrorVA)
{
    vfprintf( stderr, fmt, args );
}

PLATFORM_GET_ABSOLUTE_PATH(Win32GetAbsolutePath)
{
    bool result = true;
    char path[MAX_PATH] = {};

    DWORD ret = GetFullPathName( filename, U32( ARRAYCOUNT(path) ), path, nullptr );
    if( ret && ret < outBufferLen - 1 )
    {
        // Escape backslashes
        char* outp = outBuffer;
        for( sz i = 0; i < outBufferLen - 1; ++i )
        {
            *outp++ = path[i];
            if( path[i] == '\\' )
                *outp++ = '\\';
            else if( path[i] == 0 )
                break;
        }
        *outp = '\0';
    }
    else
    {
        globalPlatform.Error( "Couldn't find path for '%s'", filename );
        result = false;
    }

    return result;
}

PLATFORM_READ_ENTIRE_FILE(Win32ReadEntireFile)
{
    void* resultData = nullptr;
    int resultLength = 0;

    HANDLE fileHandle = CreateFile( filename, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0 );
    if( fileHandle != INVALID_HANDLE_VALUE )
    {
        LARGE_INTEGER fileSize;
        if( GetFileSizeEx( fileHandle, &fileSize ) )
        {
            u32 fileSize32 = U32( (u64)fileSize.QuadPart );
            resultData = PUSH_SIZE( arena, fileSize32 + 1 );

            if( resultData )
            {
                DWORD bytesRead;
                if( ReadFile( fileHandle, resultData, fileSize32, &bytesRead, 0 )
                    && (fileSize32 == bytesRead) )
                {
                    // Null-terminate to help when handling text files
                    *((u8 *)resultData + fileSize32) = '\0';
                    resultLength = I32( fileSize32 + 1 );
                }
                else
                {
                    globalPlatform.Error( "ReadFile failed for '%s'", filename );
                    resultData = 0;
                }
            }
            else
            {
                globalPlatform.Error( "Couldn't allocate buffer for file contents" );
            }
        }
        else
        {
            globalPlatform.Error( "Failed querying file size for '%s'", filename );
        }

        CloseHandle( fileHandle );
    }
    else
    {
        globalPlatform.Error( "Failed opening file '%s' for reading", filename );
    }

    return { resultData, resultLength };
}

PLATFORM_WRITE_ENTIRE_FILE(Win32WriteEntireFile)
{
    DWORD creationMode = CREATE_NEW;
    if( overwrite ) 
        creationMode = CREATE_ALWAYS;

    HANDLE outFile = CreateFile( filename, GENERIC_WRITE, 0, NULL,
                                 creationMode, FILE_ATTRIBUTE_NORMAL, NULL ); 
    if( outFile == INVALID_HANDLE_VALUE )
    {
        globalPlatform.Error( "Could not open '%s' for writing", filename );
        return false;
    }

    bool error = false;
    for( int i = 0; i < chunks.count; ++i )
    {
        buffer const& chunk = chunks[i];
        SetFilePointer( outFile, 0, NULL, FILE_END );

        DWORD bytesWritten;
        if( !WriteFile( outFile, chunk.data, U32( chunk.length ), &bytesWritten, NULL ) )
        {
            globalPlatform.Error( "Failed writing %d bytes to '%s'", chunk.length, filename );
            error = true;
            break;
        }
    }

    CloseHandle( outFile );

    return !error;
}

PLATFORM_CURRENT_TIME_MILLIS(Win32CurrentTimeMillis)
{
    LARGE_INTEGER counter;
    QueryPerformanceCounter( &counter );
    f64 result = (f64)counter.QuadPart / globalPerfCounterFrequency * 1000;
    return result;
}

PLATFORM_SHELL_EXECUTE(Win32ShellExecute)
{
    int exitCode = -1;
    char outBuffer[2048] = {};

    FILE* pipe = _popen( cmdLine, "rt" );
    if( pipe == NULL )
    {
        _strerror_s( outBuffer, Size( ARRAYCOUNT(outBuffer) ), "Error executing compiler command" );
        globalPlatform.Error( outBuffer );
        globalPlatform.Error( "\n" );
    }
    else
    {
        while( fgets( outBuffer, I32( ARRAYCOUNT(outBuffer) ), pipe ) )
            globalPlatform.Print( outBuffer );

        if( feof( pipe ) )
            exitCode = _pclose( pipe );
        else
        {
            globalPlatform.Error( "Failed reading compiler pipe to the end\n" );
        }
    }

    return exitCode;
}

#define ASSERT_SIZE( value ) \
    (ASSERT( (value) >= 0 ), (size_t)(value))

#define ASSERT_U32( value ) \
    (ASSERT( 0 <= (value) && (value) <= U32MAX ), (u32)(value))

DWORD Win32GetLastError( char* result_string = nullptr, sz result_string_len = 0 )
{
    DWORD result = ::GetLastError();

    if( result == 0 )
    {
        if( result_string && result_string_len )
            *result_string = 0;
        return result;
    }

    CHAR msg_buffer[2048] = {};
    CHAR* out = result_string ? result_string : msg_buffer;
    DWORD max_out_len = ASSERT_U32( result_string ? result_string_len : ARRAYCOUNT(msg_buffer) );

    DWORD msg_size = FormatMessageA( FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                                     NULL,
                                     result,
                                     MAKELANGID( LANG_NEUTRAL, SUBLANG_DEFAULT ),
                                     out,
                                     max_out_len,
                                     NULL );
    if( msg_size )
    {
        // Remove trailing newline
        for( int i = 0; i < 2; ++i )
            if( out[msg_size - 1] == '\n' || out[msg_size - 1] == '\r' )
            {
                msg_size--;
                out[msg_size] = 0;
            }

        if( !result_string )
            Win32Print( "ERROR [%08x] : %s\n", result, out );
    }

    return result;
}

// Leave at least one byte for the terminator
#define APPEND_TO_BUFFER( fmt, ... ) { int msg_len = snprintf( buffer, ASSERT_SIZE( buffer_end - buffer - 1 ), fmt, ##__VA_ARGS__ ); buffer += msg_len; }

void TraceToBuffer( char* buffer, sz buffer_length, int ignore_number_of_frames = 0 )
{
    char* buffer_end = buffer + buffer_length;

    HANDLE hProcess = GetCurrentProcess();

    char exe_path[MAX_PATH];
    {
        if( GetModuleFileName(NULL, exe_path, MAX_PATH) )
        {
            char* p_scan = exe_path;
            char* last_sep = nullptr;
            while( *p_scan != 0 )
            {
                if( (*p_scan == '\\' || *p_scan == '/') && last_sep != p_scan - 1 )
                    last_sep = p_scan;
                ++p_scan;
            }
            if( last_sep )
                *last_sep = 0;

#if 0
            APPEND_TO_BUFFER( "Path for symbols is '%s'\n", exe_path );
#endif
        }
        else
        {
            char error_msg[2048] = {};
            u32 error_code = Win32GetLastError( error_msg, ARRAYCOUNT( error_msg ) );

            APPEND_TO_BUFFER( "<<< GetModuleFileName failed with error 0x%08x ('%s') >>>\n", error_code, error_msg );
        }
    }

    if (!SymInitialize(hProcess, exe_path, TRUE))
    {
        char error_msg[2048] = {};
        u32 error_code = Win32GetLastError( error_msg, ARRAYCOUNT( error_msg ) );

        APPEND_TO_BUFFER( "<<< SymInitialize failed with error 0x%08x ('%s') >>>\n\n", error_code, error_msg );
    }

    char pwd[MAX_PATH] = {};
    GetCurrentDirectory( ASSERT_U32( ARRAYCOUNT(pwd) ), pwd );
    int pwd_len = StringLength( pwd );

    //File::UnfixSlashes( pwd );
    StringToLowercase( pwd );

    void* stacktrace_addresses[32]{};
    CaptureStackBackTrace( ASSERT_U32( ignore_number_of_frames ), 32, stacktrace_addresses, NULL);

    int trace_index = 0;
    char frame_desc_buffer[256];
    while (stacktrace_addresses[trace_index] != nullptr)
    {
        frame_desc_buffer[0] = '\0';

        // Resolve source filename & line
        DWORD lineDisplacement = 0;
        IMAGEHLP_LINE64 lineInfo;
        ZeroMemory(&lineInfo, sizeof(lineInfo));
        lineInfo.SizeOfStruct = sizeof(lineInfo);

        if (SymGetLineFromAddr64(hProcess, (DWORD64)stacktrace_addresses[trace_index], &lineDisplacement, &lineInfo))
        {
            char const* file_path = lineInfo.FileName;

            // Try to convert to relative path using current dir
            StringToLowercase( (char*)lineInfo.FileName );

            if( StringStartsWith( file_path, pwd ) )
                file_path += pwd_len;
            if( file_path[0] == '\\' || file_path[0] == '/' )
                file_path++;

            snprintf( frame_desc_buffer, ASSERT_SIZE( ARRAYCOUNT(frame_desc_buffer) ), "%s (%u)", file_path, (u32)lineInfo.LineNumber );
        }
        else
        {
            char error_msg[2048] = {};
            DWORD error_code = Win32GetLastError( error_msg, ARRAYCOUNT( error_msg ) );

            snprintf(frame_desc_buffer, ASSERT_SIZE( ARRAYCOUNT(frame_desc_buffer) ), "[SymGetLineFromAddr64 failed: %s]", error_msg );
        }

        // Resolve symbol name
        const u32 sym_name_len = 256;
        char sym_name_buffer[sym_name_len];
        {
            DWORD64 dwDisplacement = 0;

            char sym_buffer[sizeof(IMAGEHLP_SYMBOL64) + sym_name_len];
            ZeroMemory( &sym_buffer, ASSERT_SIZE( ARRAYCOUNT(sym_buffer) ) );

            IMAGEHLP_SYMBOL64* symbol = (IMAGEHLP_SYMBOL64*)sym_buffer;
            symbol->SizeOfStruct = sizeof(IMAGEHLP_SYMBOL64);
            symbol->MaxNameLength = sym_name_len - 1;

            if (SymGetSymFromAddr64(hProcess, (DWORD64)stacktrace_addresses[trace_index], &dwDisplacement, symbol))
            {
                SymUnDName64( symbol, sym_name_buffer, ASSERT_U32( ARRAYCOUNT(sym_name_buffer) ) );
            }
            else
            {
                char error_msg[2048] = {};
                DWORD error_code = Win32GetLastError( error_msg, ARRAYCOUNT( error_msg ) );
                snprintf( sym_name_buffer, ASSERT_SIZE( ARRAYCOUNT(sym_name_buffer) ), "[SymGetSymFromAddr64 failed: %s]", error_msg );
            }
        }

        APPEND_TO_BUFFER( "[%2d] %s\n" "\t0x%016llx %s\n", trace_index, sym_name_buffer,
                          (DWORD64)stacktrace_addresses[trace_index], frame_desc_buffer );

        // bail when we hit the application entry-point, don't care much about beyond this level
        if (strstr(sym_name_buffer, "main") != nullptr)
            break;

        trace_index++;
    }

    *buffer = 0;
}
#undef APPEND_TO_BUFFER

ASSERT_HANDLER(DefaultAssertHandler)
{
    // TODO Logging
    char buffer[256] = {};

    va_list args;
    va_start( args, msg );
    vsnprintf( buffer, ASSERT_SIZE( ARRAYCOUNT(buffer) ), msg, args );
    va_end( args );

    Win32Error( "ASSERTION FAILED! :: \"%s\" (%s@%d)\n", buffer, file, line );

    char callstack[16384];
    TraceToBuffer( callstack, ARRAYCOUNT(callstack), 2 );

    Win32Error( "%s", callstack );
}

internal LONG WINAPI Win32ExceptionHandler( LPEXCEPTION_POINTERS exception_pointers )
{
    char callstack[16384];
    TraceToBuffer( callstack, ARRAYCOUNT(callstack), 8 );

    Win32Error( "UNHANDLED EXCEPTION\n" );
    Win32Error( "%s", callstack );

    return EXCEPTION_EXECUTE_HANDLER;
}

int main( int argCount, char const* args[] )
{
    SetUnhandledExceptionFilter( Win32ExceptionHandler );

    // Init global platform
    globalPlatform = {};
    globalPlatform.Alloc = Win32Alloc;
    globalPlatform.Free = Win32Free;
    globalPlatform.GetAbsolutePath = Win32GetAbsolutePath;
    globalPlatform.ReadEntireFile = Win32ReadEntireFile;
    globalPlatform.WriteEntireFile = Win32WriteEntireFile;
    globalPlatform.CurrentTimeMillis = Win32CurrentTimeMillis;
    globalPlatform.ShellExecute = Win32ShellExecute;
    globalPlatform.Print = Win32Print;
    globalPlatform.Error = Win32Error;
    globalPlatform.PrintVA = Win32PrintVA;
    globalPlatform.ErrorVA = Win32ErrorVA;

    LARGE_INTEGER perfCounterFreqMeasure;
    QueryPerformanceFrequency( &perfCounterFreqMeasure );
    globalPerfCounterFrequency = (f64)perfCounterFreqMeasure.QuadPart;

    bool result = Run( argCount, args );
    return result ? 0 : 1;
}
