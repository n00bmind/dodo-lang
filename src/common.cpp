
struct DirectiveWithIndex
{
    Directive::Enum d;
    // Predicate
    bool operator()( NodeDirective const& item )
    {
        return item.name == globalDirectives[d];
    }
};

NodeDirective* FindDirective( Node* node, Directive::Enum d )
{
    NodeDirective* result = node->directives.Find( DirectiveWithIndex{ d } );
    return result;
}

bool ContainsDirective( Node* node, Directive::Enum d )
{
    NodeDirective* result = FindDirective( node, d );
    return result != nullptr;
}

NodeDirective const* FindDirective( BucketArray<NodeDirective> const& directives, Directive::Enum d )
{
    NodeDirective const* result = directives.Find( DirectiveWithIndex{ d } );
    return result;
}

bool ContainsDirective( BucketArray<NodeDirective> const& directives, Directive::Enum d )
{
    NodeDirective const* result = FindDirective( directives, d );
    return result != nullptr;
}


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
    i32 infoCount;  // >= 0: How many info notes follow this guy if any. < 0: this is an info note
};

internal MemoryArena globalErrorArena;
RingBuffer<ErrorInfo> globalErrorBuffer;
internal ErrorInfo* globalLastError;
internal bool globalSilenceInfos;

bool globalBreakOnError = false;


void Error( char const* fmt, ... )
{
    for( auto it = globalNodeStack.Last(); it; --it )
    {
        Stmt* node = *it;
        // TODO Specific error number
        if( NodeDirective* slot = FindDirective( node, Directive::ExpectError ) )
        {
            node->directives.Remove( slot );
            globalSilenceInfos = true;
            return;
        }
    }

    va_list args;
    va_start( args, fmt );
    String errorString = String::FromFormat( &globalErrorArena, fmt, args );
    va_end( args );

    ErrorInfo error = {};
    error.msg = errorString;

    globalLastError = globalErrorBuffer.Push( error );
    globalSilenceInfos = false;

    if( globalBreakOnError )
        __debugbreak();
}

void Info( char const* fmt, ... )
{
    if( globalSilenceInfos )
        return;

    va_list args;
    va_start( args, fmt );
    String errorString = String::FromFormat( &globalErrorArena, fmt, args );
    va_end( args );

    ErrorInfo error = {};
    error.msg = errorString;
    error.infoCount = -1;

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
