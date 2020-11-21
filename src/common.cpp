
void Error( Lexer* lexer, char const* fmt, ... )
{
    va_list arg_list;
    va_start( arg_list, fmt );

    globalPlatform.ErrorVA( fmt, arg_list );
    va_end( arg_list );

    if( lexer ) 
        lexer->error = true;
    globalError = true;

#if DEBUG
    __debugbreak();
#endif
}

#define PARSE_ERROR( pos, msg, ... ) Error( lexer, "%s(%d,%d): Error: "msg"\n", \
                                        (pos).filename, (pos).lineNumber, (pos).columnNumber, ##__VA_ARGS__ )

// TODO How could we properly unwind the stack here so we can try to continue afterwards?
#define RSLV_ERROR( pos, msg, ... ) Error( nullptr, "%s(%d,%d): Error: "msg"\n", \
                                        (pos).filename, (pos).lineNumber, (pos).columnNumber, ##__VA_ARGS__ )
