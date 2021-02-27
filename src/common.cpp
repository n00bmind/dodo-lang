
void ParseError( Lexer* lexer, char const* fmt, ... )
{
    va_list arg_list;
    va_start( arg_list, fmt );

    globalPlatform.ErrorVA( fmt, arg_list );
    va_end( arg_list );

    lexer->error = true;
    globalErrorCount++;

#if  CONFIG_DEBUG
    __debugbreak();
#endif
}


struct ErrorInfo
{
    String msg;     // Points to error arena
    i32 infoCount;  // How many info notes follow this guy if any
};

MemoryArena globalErrorArena;
RingBuffer<ErrorInfo> globalErrorBuffer;
ErrorInfo* globalLastError;

void Error( char const* fmt, ... )
{
    va_list args;
    va_start( args, fmt );
    String errorString = String::FromFormat( &globalErrorArena, fmt, args );
    va_end( args );

    ErrorInfo error = {};
    error.msg = errorString;

    globalLastError = globalErrorBuffer.Push( error );
//#if  CONFIG_DEBUG
    //__debugbreak();
//#endif
}

void Info( char const* fmt, ... )
{
    va_list args;
    va_start( args, fmt );
    String errorString = String::FromFormat( &globalErrorArena, fmt, args );
    va_end( args );

    ErrorInfo error = {};
    error.msg = errorString;

    globalLastError->infoCount++;
}

#define PARSE_ERROR( pos, msg, ... ) \
    ParseError( lexer, "%s(%d,%d): Error: "msg"\n", (pos).filename, (pos).lineNumber, (pos).columnNumber, ##__VA_ARGS__ )

#define RSLV_ERROR( pos, msg, ... ) \
    Error( "%s(%d,%d): Error: "msg"\n", (pos).filename, (pos).lineNumber, (pos).columnNumber, ##__VA_ARGS__ )
#define RSLV_INFO( pos, msg, ... ) \
    Info( "%s(%d,%d): Note: "msg"\n", (pos).filename, (pos).lineNumber, (pos).columnNumber, ##__VA_ARGS__ )


void InitErrorBuffer()
{
    InitArena( &globalErrorArena );
    INIT( globalErrorBuffer ) RingBuffer<ErrorInfo>( 256, &globalErrorArena );
}
