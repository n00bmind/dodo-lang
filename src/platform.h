
struct PlatformReadFileResult
{
    void *contents;
    i32 contentSize;
};

#define PLATFORM_READ_ENTIRE_FILE(name) PlatformReadFileResult name( const char *filename, MemoryArena* arena )
typedef PLATFORM_READ_ENTIRE_FILE(PlatformReadEntireFileFunc);

