R"~~~(#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>

#define ARGS(...) __VA_ARGS__  
#define SIZEOF(s) Sz( sizeof(s) )
#define OFFSETOF(type, member) Sz( (uintptr_t)&(((type *)0)->member) )
#define ARRAYCOUNT(array) Sz( sizeof(array) / sizeof((array)[0]) )

#if _MSC_VER
#define INLINE __forceinline
#else
#define INLINE inline __attribute__((always_inline))
#endif

// TODO Only debugbreak if there's a debugger present, otherwise exit and print stack trace
#define HALT() (__debugbreak(), 1)
#if !CONFIG_RELEASE
#define expect(expr, ...) \
    ((void)( !(expr) \
             && (globalAssertHandler( __FILE__, __LINE__, #expr " " ##__VA_ARGS__ ), 1) \
             && HALT()))
#else
#define expect(expr, ...) ((void)0)
#endif

#define ASSERT_HANDLER(name) void name( const char* file, int line, const char* msg, ... )
typedef ASSERT_HANDLER(AssertHandlerFunc);

ASSERT_HANDLER(DefaultAssertHandler);
AssertHandlerFunc* globalAssertHandler = DefaultAssertHandler;


typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef uint8_t b8;
typedef uint16_t b16;
typedef uint32_t b32;
typedef unsigned long long b64;

typedef float f32;
typedef double f64;

typedef int64_t sz;

#define I8MIN INT8_MIN
#define I8MAX INT8_MAX
#define I16MAX INT16_MAX
#define I16MIN INT16_MIN
#define I32MAX INT32_MAX
#define I32MIN INT32_MIN
#define I64MAX INT64_MAX
#define I64MIN INT64_MIN
#define B8MAX UINT8_MAX
#define B16MAX UINT16_MAX
#define B32MAX UINT32_MAX
#define B64MAX UINT64_MAX // ULLONG_MAX?

#define F32MAX FLT_MAX
#define F32MIN FLT_MIN
#define F32INF INFINITY
#define F64MAX DBL_MAX
#define F64MIN DBL_MIN
#define F64INF (f64)INFINITY


INLINE constexpr i8
I8( i32 value )
{
    expect( I8MIN <= value && value <= I8MAX );
    return (i8)value;
}

INLINE constexpr b8
B8( b32 value )
{
    expect( value <= B8MAX );
    return (b8)value;
}

INLINE constexpr b8
B8( i32 value )
{
    expect( value >= 0 && value <= B8MAX );
    return (b8)value;
}

INLINE constexpr i16
I16( i32 value )
{
    expect( I16MIN <= value && value <= I16MAX );
    return (i16)value;
}

INLINE constexpr b16
B16( i64 value )
{
    expect( 0 <= value && value <= B16MAX );
    return (b16)value;
}

INLINE constexpr b16
B16( f64 value )
{
    expect( 0 <= value && value <= B16MAX );
    return (b16)value;
}

INLINE constexpr i32
I32( sz value )
{
    expect( I32MIN <= value && value <= I32MAX );
    return (i32)value;
}

INLINE constexpr i32
I32( f32 value )
{
    expect( (f32)I32MIN <= value && value <= (f32)I32MAX );
    return (i32)value;
}

INLINE constexpr i32
I32( f64 value )
{
    expect( I32MIN <= value && value <= I32MAX );
    return (i32)value;
}

INLINE constexpr i32
I32( b64 value )
{
    expect( value <= (b64)I32MAX );
    return (i32)value;
}

INLINE constexpr i32
I32( b32 value )
{
    expect( value <= (b32)I32MAX );
    return (i32)value;
}

INLINE constexpr b32
B32( i32 value )
{
    expect( value >= 0 );
    return (b32)value;
}

INLINE constexpr b32
B32( b64 value )
{
    expect( value <= B32MAX );
    return (b32)value;
}

INLINE constexpr b32
B32( i64 value )
{
    expect( 0 <= value && value <= B32MAX );
    return (b32)value;
}

INLINE constexpr b32
B32( f64 value )
{
    expect( 0 <= value && value <= B32MAX );
    return (b32)value;
}

INLINE constexpr sz
Sz( size_t value )
{
    expect( value <= (size_t)I64MAX );
    return (sz)value;
}

INLINE constexpr size_t
Size( sz value )
{
    expect( value >= 0 );
    return (size_t)value;
}

ASSERT_HANDLER(DefaultAssertHandler)
{
    // TODO Stacktrace
    char buffer[256] = {};

    va_list args;
    va_start( args, msg );
    vsnprintf( buffer, Size( ARRAYCOUNT(buffer) ), msg, args );
    va_end( args );

    fprintf( stderr, "ASSERTION FAILED! :: \"%s\" (%s@%d)\n", buffer, file, line );
}


//_______________________________________________________________________________
// TODO At some point all these basic types will need to be ported over
// or implemented internally in dodo and hidden from user code

template <typename T>
struct buffer
{
    T* data;
    i64 length;

public:
    buffer( T* data_, i64 length_ )
        : data( data_ )
        , length( length_ )
    {}

    operator T*() { return data; }
};

#define BUFFER(T, ...)                                        \
[]()                                                          \
{                                                             \
    static T literal[] = { __VA_ARGS__ };                     \
    return buffer<T>( literal, sizeof(literal) / sizeof(T) ); \
}()


struct string
{
    static constexpr unsigned char Empty[] = "";

    unsigned char const* data;
    i32 length;

public:
    string()
        : data( Empty )
        , length( 0 )
    {}

    string( char const* s )
        : data( (unsigned char const*)s )
        , length( I32( strlen( s ) ) )
    {}

    string( char const* s, int len )
        : data( (unsigned char const*)s )
        , length( len )
    {}

    unsigned char operator []( int i ) const { return data[i]; }
    operator char const*() const { return (char const*)data; }
};
)~~~";

