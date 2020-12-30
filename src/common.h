#ifndef __COMMON_H__
#define __COMMON_H__ 

#if NON_UNITY_BUILD
#include <stdint.h>
#include <math.h>
#endif

//
// Common definitions
//

#define internal static
#define persistent static


#define HALT() (__debugbreak(), 1)
#if !CONFIG_RELEASE
#define ASSERT(expr, ...) \
    ((void)( !(expr) \
             && (globalAssertHandler( __FILE__, __LINE__, IF_ELSE( HAS_ARGS(__VA_ARGS__) )( __VA_ARGS__ )( #expr ) ), 1) \
             && HALT()))
#else
#define ASSERT(expr, ...) ((void)0)
#endif

#define ASSERT_HANDLER(name) void name( const char* file, int line, const char* msg, ... )
typedef ASSERT_HANDLER(AssertHandlerFunc);

ASSERT_HANDLER(DefaultAssertHandler);
AssertHandlerFunc* globalAssertHandler = DefaultAssertHandler;

#if !CONFIG_RELEASE
#define NOT_IMPLEMENTED ASSERT(!"NotImplemented")
#else
#define NOT_IMPLEMENTED NotImplemented!!!
#endif

#define INVALID_CODE_PATH ASSERT(!"InvalidCodePath");
#define INVALID_DEFAULT_CASE default: { INVALID_CODE_PATH; } break;


#define SIZEOF(s) Sz( sizeof(s) )
#define ARRAYCOUNT(array) Sz( sizeof(array) / sizeof((array)[0]) )
#define OFFSETOF(type, member) Sz( &(((type *)0)->member) )
#define STR(s) _STR(s)
#define _STR(s) #s

#define KILOBYTES(value) ((value)*1024)
#define MEGABYTES(value) (KILOBYTES(value)*1024)
#define GIGABYTES(value) (MEGABYTES((u64)value)*1024)

#define COPY(source, dest) memcpy( &dest, &source, sizeof(dest) )
#define SET(dest, value) memset( &dest, value, sizeof(dest) )
#define ZERO(dest) memset( &dest, 0, sizeof(dest) )
#define EQUAL(source, dest) (memcmp( &source, &dest, sizeof(source) ) == 0)
#define PCOPY(source, dest, size) memcpy( dest, source, Size( size ) )
#define PSET(dest, value, size) memset( dest, value, Size( size ) )
#define PZERO(dest, size) memset( dest, 0, Size( size ) )
#define PEQUAL(source, dest, size) (memcmp( source, dest, Size( size ) ) == 0)


#if _MSC_VER
#define INLINE __forceinline
#else
#define INLINE inline __attribute__((always_inline))
#endif


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

typedef int64_t sz;

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

INLINE constexpr i8
I8( i32 value )
{
    ASSERT( I8MIN <= value && value <= I8MAX );
    return (i8)value;
}

INLINE constexpr u8
U8( u32 value )
{
    ASSERT( value <= U8MAX );
    return (u8)value;
}

INLINE constexpr u8
U8( i32 value )
{
    ASSERT( value >= 0 && value <= U8MAX );
    return (u8)value;
}

INLINE constexpr i16
I16( i32 value )
{
    ASSERT( I16MIN <= value && value <= I16MAX );
    return (i16)value;
}

INLINE constexpr u16
U16( i64 value )
{
    ASSERT( 0 <= value && value <= U16MAX );
    return (u16)value;
}

INLINE constexpr u16
U16( f64 value )
{
    ASSERT( 0 <= value && value <= U16MAX );
    return (u16)value;
}

INLINE constexpr i32
I32( sz value )
{
    ASSERT( I32MIN <= value && value <= I32MAX );
    return (i32)value;
}

INLINE constexpr i32
I32( f32 value )
{
    ASSERT( (f32)I32MIN <= value && value <= (f32)I32MAX );
    return (i32)value;
}

INLINE constexpr i32
I32( f64 value )
{
    ASSERT( I32MIN <= value && value <= I32MAX );
    return (i32)value;
}

INLINE constexpr i32
I32( u64 value )
{
    ASSERT( value <= (u64)I32MAX );
    return (i32)value;
}

INLINE constexpr i32
I32( u32 value )
{
    ASSERT( value <= (u32)I32MAX );
    return (i32)value;
}

INLINE constexpr u32
U32( i32 value )
{
    ASSERT( value >= 0 );
    return (u32)value;
}

INLINE constexpr u32
U32( u64 value )
{
    ASSERT( value <= U32MAX );
    return (u32)value;
}

INLINE constexpr u32
U32( i64 value )
{
    ASSERT( 0 <= value && value <= U32MAX );
    return (u32)value;
}

INLINE constexpr u32
U32( f64 value )
{
    ASSERT( 0 <= value && value <= U32MAX );
    return (u32)value;
}

INLINE constexpr sz
Sz( size_t value )
{
    ASSERT( value <= (size_t)I64MAX );
    return (sz)value;
}

INLINE constexpr size_t
Size( sz value )
{
    ASSERT( value >= 0 );
    return (size_t)value;
}


/////     BUFFER    /////

struct Buffer
{
    void* data;
    sz size;
};


/////     STRUCT ENUM    /////
// TODO Combine with the ideas in https://blog.paranoidcoding.com/2010/11/18/discriminated-unions-in-c.html to create a similar
// TAGGED_UNION for discriminated unions

/* Usage:

struct V
{
    char const* sVal;
    int iVal;
};

#define VALUES(x) \
    x(None,         ("a", 100)) \
    x(Animation,    ("b", 200)) \
    x(Landscape,    ("c", 300)) \
    x(Audio,        ("d", 400)) \
    x(Network,      ("e", 500)) \
    x(Scripting,    ("f", 600)) \

// Values must go in () even if they're a single primitive type!
STRUCT_ENUM_WITH_VALUES(MemoryTag, V, VALUES)
#undef VALUES

int main()
{
    // Can be used as normal
    int index = MemoryTag::Audio;
    
    // But also
    MemoryTag const& t = MemoryTag::Values::Audio;
    ASSERT( t.index == index );

    V const& value = MemoryTag::Landscape.value;

    for( int i = 0; i < MemoryTag::Values::count; ++i )
    {
        MemoryTag const& t = MemoryTag::Values::items[i];
        std::cout << "sVal: " << t.value.sVal << "\tiVal: " << t.value.iVal << std::endl;
    }
}
*/

#define _ENUM_ARGS(...)         { __VA_ARGS__ }
#define _ENUM_ENTRY(x, ...)     x,
#define _ENUM_NAME(x, ...)      items[EnumName::x].name,
#define _ENUM_REF(x, ...)       static constexpr EnumName const& x = items[EnumName::x];
#define _ENUM_ITEM(x)                           { #x, ValueType(), (i32)EnumName::x },
#define _ENUM_ITEM_WITH_NAMES(x, n)             {  n, ValueType(), (i32)EnumName::x },
#define _ENUM_ITEM_WITH_VALUES(x, v)            { #x, _ENUM_ARGS v, (i32)EnumName::x },
#define _ENUM_ITEM_WITH_NAMES_VALUES(x, n, v)   {  n, _ENUM_ARGS v, (i32)EnumName::x },


#define _CREATE_ENUM(enumName, valueType, xValueList, xBuilder)            \
struct enumName                                                            \
{                                                                          \
    enum Enum : i32                                                        \
    {                                                                      \
        xValueList(_ENUM_ENTRY)                                            \
    };                                                                     \
                                                                           \
    char const* name;                                                      \
    valueType value;                                                       \
    i32 index;                                                             \
                                                                           \
    bool operator ==( enumName const& other ) const                        \
    { return index == other.index; }                                       \
    bool operator !=( enumName const& other ) const                        \
    { return index != other.index; }                                       \
                                                                           \
    /* Need to define this separately so the base type is fully defined */ \
    struct Values;                                                         \
};                                                                         \
struct enumName::Values                                                    \
{                                                                          \
    using EnumName = enumName;                                             \
    using ValueType = valueType;                                           \
                                                                           \
    static constexpr enumName items[] =                                    \
    {                                                                      \
        xValueList(xBuilder)                                               \
    };                                                                     \
    static constexpr char const* const names[] =                           \
    {                                                                      \
        xValueList(_ENUM_NAME)                                             \
    };                                                                     \
    static constexpr sz count = ARRAYCOUNT(items);                         \
                                                                           \
    xValueList(_ENUM_REF)                                                  \
};                                                                         \

#define STRUCT_ENUM(enumName, xValueList)                               _CREATE_ENUM(enumName, i32, xValueList, _ENUM_ITEM)
#define STRUCT_ENUM_WITH_NAMES(enumName, xValueList)                    _CREATE_ENUM(enumName, i32, xValueList, _ENUM_ITEM_WITH_NAMES)
#define STRUCT_ENUM_WITH_VALUES(enumName, valueType, xValueList)        _CREATE_ENUM(enumName, valueType, xValueList, _ENUM_ITEM_WITH_VALUES)
#define STRUCT_ENUM_WITH_NAMES_VALUES(enumName, valueType, xValueList)  _CREATE_ENUM(enumName, valueType, xValueList, _ENUM_ITEM_WITH_NAMES_VALUES)



#endif /* __COMMON_H__ */
