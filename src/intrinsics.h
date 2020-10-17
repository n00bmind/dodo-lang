
INLINE bool
IsPowerOf2( u64 value )
{
    return value > 0 && (value & (value - 1)) == 0;
}

INLINE bool
IsPowerOf2( i64 value )
{
    return value > 0 && (value & (value - 1)) == 0;
}

INLINE sz
AlignUp( sz size, sz alignment )
{
    ASSERT( IsPowerOf2( alignment ) );
    sz result = (size + (alignment - 1)) & ~(alignment - 1);
    return result;
}

INLINE void*
AlignUp( const void* address, sz alignment )
{
    ASSERT( IsPowerOf2( alignment ) );
    void* result = (void*)(((sz)address + (alignment - 1)) & ~(alignment - 1));
    return result;
}


