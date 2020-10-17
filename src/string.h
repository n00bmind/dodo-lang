
// Read only string
struct String
{
    char const* data;
    i32 length;

    String( char const* cString )
        : data( cString )
        , length( I32( strlen( cString ) ) )
    {}

    String( char const* cString, int len )
        : data( cString )
        , length( len )
    {}

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
};

