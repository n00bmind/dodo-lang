
struct MemoryArena;

#define PLATFORM_ALLOC(name) void* name( sz sizeBytes, u32 flags )
typedef PLATFORM_ALLOC(PlatformAlloc);
#define PLATFORM_FREE(name) void name( void* memoryBlock )
typedef PLATFORM_FREE(PlatformFree);
#define PLATFORM_READ_ENTIRE_FILE(name) Buffer name( char const* filename, MemoryArena* arena )
typedef PLATFORM_READ_ENTIRE_FILE(PlatformReadEntireFileFunc);
#define PLATFORM_PRINT(name) void name( const char *fmt, ... )
typedef PLATFORM_PRINT(PlatformPrintFunc);


#define PLATFORM_PATH_MAX 1024

struct PlatformAPI
{
    PlatformAlloc* Alloc;
    PlatformFree* Free;
    PlatformReadEntireFileFunc* ReadEntireFile;
    PlatformPrintFunc* Print;
    PlatformPrintFunc* Error;

#if 0
#if !RELEASE
    DebugPlatformFreeFileMemoryFunc* DEBUGFreeFileMemory;
    DebugPlatformWriteEntireFileFunc* DEBUGWriteEntireFile;
    DebugPlatformListAllAssetsFunc* DEBUGListAllAssets;
    DebugPlatformJoinPathsFunc* DEBUGJoinPaths;
    DebugPlatformGetParentPathFunc* DEBUGGetParentPath;
    // FIXME Remove. Replace with passed elapsed time in GameInput
    DebugPlatformCurrentTimeMillis* DEBUGCurrentTimeMillis;

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

