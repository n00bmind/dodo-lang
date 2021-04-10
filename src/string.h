
INLINE bool IsNullOrEmpty( char const* str )
{
    return str == nullptr || *str == 0;
}

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

INLINE bool StringsEqual( char const* a, char const* b )
{
    return strcmp( a, b ) == 0;
}

INLINE bool StringsEqualNoCase( char const* a, char const* b )
{
    size_t len = strlen( a );
    if( len != strlen( b ) )
        return false;

    for( size_t i = 0; i < len; ++i )
        if( tolower( a[i] ) != tolower( b[i] ) )
            return false;
    return true;
}

INLINE int StringLength( char const* s )
{
    return I32( strlen( s ) );
}

INLINE void StringCopy( char const* src, char* dst, sz dstSize )
{
    strncpy( dst, src, Size( dstSize ) );
}

INLINE bool StringStartsWith( char const* str, char const* find) { return str && find && strstr(str, find) == str; }

// In-place conversion to lowercase. Use length if provided or just advance until a null terminator is found
inline char* StringToLowercase( char* str, int len = 0 )
{
    ASSERT( str, "No string" );

    char* c = str;
    int remaining = len;
    while( (len && remaining) || (!len && *c) )
    {
        int c2 = tolower( *c );
        *c++ = (char)c2;

        remaining--;
    }

    return str;
}


struct String;

// String builder to help compose Strings piece by piece
struct StringBuilder
{
    BucketArray<char> buckets;
    MemoryParams memoryParams;

    // TODO Raise bucket size when we know it works
    StringBuilder( MemoryArena* arena, MemoryParams params = Temporary() )
        //: buckets( arena, 32, params )
        : buckets( arena, 4, params )
        , memoryParams( params )
    {}

    bool Empty() const { return buckets.count == 0; }

    void Append( char const* str, int length = 0 )
    {
        if( !length )
            length = StringLength( str );

        // Not including null terminator
        buckets.Append( str, length );
    }

    void AppendFmt( char const* fmt, ... )
    {
        va_list args;
        va_start( args, fmt );

        int n = 1 + vsnprintf( nullptr, 0, fmt, args );
        char* buf = PUSH_STRING( buckets.arena, n, memoryParams );

        // TODO Does this work??
        vsnprintf( buf, Size( n ), fmt, args );
        va_end( args );

        // TODO We probably want this struct to be made out of irregular buckets that are allocated exactly of the
        // length needed for each append (n above) so we don't need this extra copy
        buckets.Append( buf, n - 1 );
    }

    String ToString( MemoryArena* arena );
};


// Read only string
// FIXME Review everything here to make sure we always append a null terminator
// FIXME Review everything here to make sure we always append a null terminator
// FIXME Review everything here to make sure we always append a null terminator
// FIXME Review everything here to make sure we always append a null terminator
struct String
{
    char const* data;
    i32 length;

    String()
        : data( nullptr )
        , length( 0 )
    {}

    explicit String( char const* cString )
        : data( cString )
        , length( I32( strlen( cString ) ) )
    {}

    explicit String( char const* cString, int len )
        : data( cString )
        , length( len )
    {}

    explicit String( Buffer<char> const& buffer )
        : data( buffer )
        , length( I32( buffer.length ) )
    {}

    explicit String( buffer const& buffer )
        : data( (char*)buffer.data )
        , length( I32( buffer.length ) )
    {}

    static String Clone( char const* src, MemoryArena* arena )
    {
        String result = {};
        result.length = StringLength( src );
        result.data = PUSH_STRING( arena, result.length );
        StringCopy( src, (char*)result.data, result.length );

        return result;
    }

    static String Clone( BucketArray<char> const& src, MemoryArena* arena )
    {
        String result = {};
        result.length = src.count;
        result.data = PUSH_STRING( arena, result.length );
        src.CopyTo( (char*)result.data, result.length );

        return result;
    }

    // Non overlapping
    static String CloneReplace( char const* src, char const* match, char const* subst,
                                MemoryArena* arena )
    {
        int matchLen = StringLength( match );

        StringBuilder builder( arena );
        int index = 0;
        while( char const* m = strstr( src + index, match ) )
        {
            int subLen = I32( m - (src + index) );
            builder.Append( src + index, subLen );
            builder.Append( subst );

            index += subLen + matchLen;
        }

        int srcLen = StringLength( src );
        ASSERT( index <= srcLen );

        if( index < srcLen )
            builder.Append( src + index, srcLen - index );

        return builder.ToString( arena );
    }

    static String FromFormat( MemoryArena* arena, char const* fmt, va_list args )
    {
        va_list argsCopy;
        va_copy( argsCopy, args );

        String result = {};
        result.length = 1 + vsnprintf( nullptr, 0, fmt, args );
        result.data = PUSH_STRING( arena, result.length );

        vsnprintf( (char*)result.data, Size( result.length ), fmt, argsCopy );
        va_end( argsCopy );

        return result;
    }

    static String FromFormat( MemoryArena* arena, char const* fmt, ... )
    {
        va_list args;
        va_start( args, fmt );
        String result = FromFormat( arena, fmt, args );
        va_end( args );

        return result;
    }

    bool Empty() { return data == nullptr || length == 0; }

    const char* CString( MemoryArena* arena, MemoryParams params = DefaultMemoryParams() ) const
    {
        char* result = PUSH_ARRAY( arena, char, length + 1, params );
        PCOPY( data, result, length * SIZEOF(char) );
        result[length] = 0;

        return result;
    }

    int FindLast( char c )
    {
        int result = -1;
        for( int i = length - 1; i >= 0; --i )
        {
            if( data[i] == c )
            {
                result = i;
                break;
            }
        }

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
        strncpy( dst, data, Size( length ) );
    }

    void CopyToNullTerminated( char* dst ) const
    {
        CopyTo( dst );
        dst[length] = 0;
    }
};


inline String StringBuilder::ToString( MemoryArena* arena )
{
    buckets.Append( "\0", 1 );
    return String::Clone( buckets, arena );
}

INLINE bool StringsEqual( String const& a, String const& b )
{
    return a.length == b.length && strncmp( a.data, b.data, Size( a.length ) ) == 0;
}

INLINE bool StringsEqual( String const& a, char const* b )
{
    return strncmp( a.data, b, Size( a.length ) ) == 0 && b[ a.length ] == 0;
}

INLINE u64 StringHash( String const& str )
{
    return Hash64( str.data, str.length );
}


