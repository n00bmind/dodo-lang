R"STR(#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>

#define ARRAYCOUNT(array) (sizeof(array) / sizeof((array)[0]))
#if _MSC_VER
#define INLINE __forceinline
#else
#define INLINE inline __attribute__((always_inline))
#endif

#define HALT() (__debugbreak(), 1)
#if !CONFIG_RELEASE
#define ASSERT(expr) \
    ((void)( !(expr) \
             && (globalAssertHandler( __FILE__, __LINE__, #expr ), 1) \
             && HALT()))
#else
#define ASSERT(expr) ((void)0)
#endif

#define ASSERT_HANDLER(name) void name( const char* file, int line, const char* msg, ... )
typedef ASSERT_HANDLER(AssertHandlerFunc);

ASSERT_HANDLER(DefaultAssertHandler)
{
    // TODO Stacktrace
    char buffer[256] = {};

    va_list args;
    va_start( args, msg );
    vsnprintf( buffer, ARRAYCOUNT(buffer), msg, args );
    va_end( args );

    fprintf( stderr, "ASSERTION FAILED! :: \"%s\" (%s@%d)\n", buffer, file, line );
}
AssertHandlerFunc* globalAssertHandler = DefaultAssertHandler;


typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef unsigned long long u64;

typedef float f32;
typedef double f64;

typedef size_t sz;

#define I8MIN INT8_MIN
#define I8MAX INT8_MAX
#define U8MAX UINT8_MAX
#define I16MAX INT16_MAX
#define I16MIN INT16_MIN
#define I32MAX INT32_MAX
#define I32MIN INT32_MIN
#define U16MAX UINT16_MAX
#define U32MAX UINT32_MAX
#define U64MAX UINT64_MAX // ULLONG_MAX?
#define I64MAX INT64_MAX
#define I64MIN INT64_MIN

#define F32MAX FLT_MAX
#define F32MIN FLT_MIN
#define F32INF INFINITY
#define F64MAX DBL_MAX
#define F64MIN DBL_MIN
#define F64INF (f64)INFINITY


INLINE i8
I8( i32 value )
{
    ASSERT( I8MIN <= value && value <= I8MAX );
    return (i8)value;
}

INLINE i16
I16( i32 value )
{
    ASSERT( I16MIN <= value && value <= I16MAX );
    return (i16)value;
}

INLINE i32
I32( sz value )
{
    ASSERT( value <= I32MAX );
    return (i32)value;
}

INLINE i32
I32( ptrdiff_t value )
{
    ASSERT( I32MIN <= value && value <= I32MAX );
    return (i32)value;
}

INLINE i32
I32( f32 value )
{
    ASSERT( (f32)I32MIN <= value && value <= (f32)I32MAX );
    return (i32)value;
}

INLINE i32
I32( f64 value )
{
    ASSERT( I32MIN <= value && value <= I32MAX );
    return (i32)value;
}

INLINE i32
I32( u32 value )
{
    ASSERT( value <= (u32)I32MAX );
    return (i32)value;
}

INLINE i64
I64( sz value )
{
    ASSERT( value <= (sz)I64MAX );
    return (i64)value;
}

INLINE u32
U32( i32 value )
{
    ASSERT( value >= 0 );
    return (u32)value;
}

INLINE u32
U32( u64 value )
{
    ASSERT( value <= U32MAX );
    return (u32)value;
}

INLINE u32
U32( f64 value )
{
    ASSERT( 0 <= value && value <= U32MAX );
    return (u32)value;
}

INLINE u16
U16( i64 value )
{
    ASSERT( 0 <= value && value <= U16MAX );
    return (u16)value;
}

INLINE u16
U16( f64 value )
{
    ASSERT( 0 <= value && value <= U16MAX );
    return (u16)value;
}

INLINE u8
U8( u32 value )
{
    ASSERT( value <= U8MAX );
    return (u8)value;
}

INLINE u8
U8( i32 value )
{
    ASSERT( value >= 0 && value <= U8MAX );
    return (u8)value;
}

INLINE sz
Sz( i32 value )
{
    ASSERT( value >= 0 );
    return (sz)value;
}

INLINE sz
Sz( i64 value )
{
    ASSERT( value >= 0 );
    return (sz)value;
}

class str
{
    static constexpr char const* Empty = "";

    char const* data;
    i32 length;

public:
    str()
        : data( Empty )
        , length( 0 )
    {}

    str( char const* cString )
        : data( cString )
        , length( I32( strlen( cString ) ) )
    {}

    str( char const* cString, int len )
        : data( cString )
        , length( len )
    {}

    operator char const*() const { return data; }
};
)STR";
