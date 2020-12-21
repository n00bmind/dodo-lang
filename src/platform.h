
struct MemoryArena;
template <typename T> struct Array;

#define PLATFORM_ALLOC(name) void* name( sz sizeBytes, u32 flags )
typedef PLATFORM_ALLOC(PlatformAlloc);
#define PLATFORM_FREE(name) void name( void* memoryBlock )
typedef PLATFORM_FREE(PlatformFree);

#define PLATFORM_GET_ABSOLUTE_PATH(name) bool name( char const* filename, char* outBuffer, sz outBufferLen )
typedef PLATFORM_GET_ABSOLUTE_PATH(PlatformGetAbsolutePathFunc);
#define PLATFORM_READ_ENTIRE_FILE(name) Buffer name( char const* filename, MemoryArena* arena )
typedef PLATFORM_READ_ENTIRE_FILE(PlatformReadEntireFileFunc);
#define PLATFORM_WRITE_ENTIRE_FILE(name) bool name( char const* filename, Array<Buffer> const& chunks )
typedef PLATFORM_WRITE_ENTIRE_FILE(PlatformWriteEntireFileFunc);

#define PLATFORM_CURRENT_TIME_MILLIS(name) f64 name()
typedef PLATFORM_CURRENT_TIME_MILLIS(PlatformCurrentTimeMillis);

#define PLATFORM_PRINT(name) void name( const char *fmt, ... )
typedef PLATFORM_PRINT(PlatformPrintFunc);
#define PLATFORM_PRINT_VA(name) void name( const char *fmt, va_list args )
typedef PLATFORM_PRINT_VA(PlatformPrintVAFunc);


struct PlatformAPI
{
    static const sz PointerSize;

    PlatformAlloc* Alloc;
    PlatformFree* Free;
    PlatformGetAbsolutePathFunc* GetAbsolutePath;
    PlatformReadEntireFileFunc* ReadEntireFile;
    PlatformWriteEntireFileFunc* WriteEntireFile;
    PlatformCurrentTimeMillis* CurrentTimeMillis;
    PlatformPrintFunc* Print;
    PlatformPrintFunc* Error;
    PlatformPrintVAFunc* PrintVA;
    PlatformPrintVAFunc* ErrorVA;

#if 0
#if !RELEASE
    DebugPlatformFreeFileMemoryFunc* DEBUGFreeFileMemory;
    DebugPlatformListAllAssetsFunc* DEBUGListAllAssets;
    DebugPlatformJoinPathsFunc* DEBUGJoinPaths;
    DebugPlatformGetParentPathFunc* DEBUGGetParentPath;
    // FIXME Remove. Replace with passed elapsed time in GameInput

    bool DEBUGquit;
#endif

    PlatformAddNewJobFunc* AddNewJob;
    PlatformCompleteAllJobsFunc* CompleteAllJobs;
    PlatformJobQueue* hiPriorityQueue;
    //PlatformJobQueue* loPriorityQueue;
    // NOTE Includes the main thread! (0)
    i32 coreThreadsCount;

    PlatformAllocateOrUpdateTextureFunc* AllocateOrUpdateTexture;
    PlatformDeallocateTextureFunc* DeallocateTexture;
#endif
};
extern PlatformAPI globalPlatform;

