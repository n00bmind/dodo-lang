
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

    void CopyFrom( BucketArray<char> const& src, MemoryArena* arena )
    {
        length = src.count;
        data = PUSH_STRING( arena, length );
        src.CopyTo( (char*)data, length );
    }
};


INLINE bool IsNewline( char c )
{
    bool result = (c == '\n' || c == '\r' );
    return result;
}

INLINE bool IsSpacing( char c )
{
    bool result = (c == ' ' ||
                c == '\t' ||
                c == '\f' ||
                c == '\v');
    return result;
}

INLINE bool IsWhitespace( char c )
{
    bool result = IsSpacing( c )
        || IsNewline( c );
    return result;
}

INLINE bool IsAlpha( char c )
{
    bool result = (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z' );
    return result;
}

INLINE bool IsNumber( char c )
{
    bool result = (c >= '0' && c <= '9');
    return result;
}

INLINE bool StringsEqual( String const& a, String const& b )
{
    return a.length == b.length && strncmp( a.data, b.data, Sz( a.length ) ) == 0;
}

// NOTE Unsafe!
INLINE bool StringsEqual( String const& a, char const* b )
{
    return b[ a.length ] == 0 && strncmp( a.data, b, Sz( a.length ) ) == 0;
}

INLINE bool StringsEqual( char const* a, char const* b )
{
    return strcmp( a, b ) == 0;
}

INLINE sz StringLength( char const* s )
{
    return strlen( s );
}

INLINE void StringCopy( char const* src, char* dst, sz dstSize )
{
    strncpy( dst, src, dstSize );
}

INLINE u64 StringHash( String const& str )
{
    return Hash64( str.data, str.length );
}
