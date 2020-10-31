
// Read only string
struct String
{
    char const* data;
    i32 length;

    String()
        : data( nullptr )
        , length( 0 )
    {}

    String( char const* cString )
        : data( cString )
        , length( I32( strlen( cString ) ) )
    {}

    String( char const* cString, int len )
        : data( cString )
        , length( len )
    {}

    String( Buffer const& buffer )
        : data( (char*)buffer.data )
        , length( I32( buffer.size ) )
    {}

    const char* CString( MemoryArena* arena, MemoryParams params = DefaultMemoryParams() ) const
    {
        char* result = PUSH_ARRAY( arena, char, length + 1, params );
        PCOPY( data, result, length * sizeof(char) );

        return result;
    }

    void Split( char separator, BucketArray<String>* result )
    {
        i32 nextLength = 0;

        char const* s = data;
        char const* nextData = s;
        while( char c = *s++ )
        {
            if( c == separator )
            {
                if( nextLength )
                {
                    result->Push( String( nextData, nextLength ) );
                    nextData = s;
                    nextLength = 0;
                }
            }
            else
                nextLength++;
        }

        // Last piece
        if( nextLength )
        {
            result->Push( String( nextData, nextLength ) );
        }
    }

    void CopyTo( char* dst ) const
    {
        strncpy( dst, data, Sz( length ) );
    }

    void CopyToNullTerminated( char* dst ) const
    {
        CopyTo( dst );
        dst[length] = 0;
    }
};


bool IsNewline( char c )
{
    bool result = (c == '\n' || c == '\r' );
    return result;
}

bool IsSpacing( char c )
{
    bool result = (c == ' ' ||
                c == '\t' ||
                c == '\f' ||
                c == '\v');
    return result;
}

bool IsWhitespace( char c )
{
    bool result = IsSpacing( c )
        || IsNewline( c );
    return result;
}

bool IsAlpha( char c )
{
    bool result = (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z' );
    return result;
}

bool IsNumber( char c )
{
    bool result = (c >= '0' && c <= '9');
    return result;
}

bool StringsEqual( char const* a, char const* b )
{
    return strcmp( a, b ) == 0;
}

