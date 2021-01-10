
void Error( Lexer* lexer, char const* fmt, ... )
{
    va_list arg_list;
    va_start( arg_list, fmt );

    globalPlatform.ErrorVA( fmt, arg_list );
    va_end( arg_list );

    if( lexer ) 
        lexer->error = true;
    globalErrorCount++;

#if  CONFIG_DEBUG
    __debugbreak();
#endif
}

void Info( char const* fmt, ... )
{
    va_list arg_list;
    va_start( arg_list, fmt );

    globalPlatform.ErrorVA( fmt, arg_list );
    va_end( arg_list );
}

#define PARSE_ERROR( pos, msg, ... ) Error( lexer, "%s(%d,%d): Error: "msg"\n", \
                                        (pos).filename, (pos).lineNumber, (pos).columnNumber, ##__VA_ARGS__ )

#define RSLV_ERROR( pos, msg, ... ) Error( nullptr, "%s(%d,%d): Error: "msg"\n", \
                                        (pos).filename, (pos).lineNumber, (pos).columnNumber, ##__VA_ARGS__ )
#define RSLV_INFO( pos, msg, ... ) Info( "%s(%d,%d): Note: "msg"\n", \
                                        (pos).filename, (pos).lineNumber, (pos).columnNumber, ##__VA_ARGS__ )
