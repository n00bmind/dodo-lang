
struct PlatformReadFileResult
{
    void *contents;
    i32 contentSize;
};

#define PLATFORM_READ_ENTIRE_FILE(name) PlatformReadFileResult name( char const* filename, MemoryArena* arena )
typedef PLATFORM_READ_ENTIRE_FILE(PlatformReadEntireFileFunc);
#define PLATFORM_PRINT(name) void name( const char *fmt, ... )
typedef PLATFORM_PRINT(PlatformPrintFunc);


#define PLATFORM_PATH_MAX 1024

struct PlatformAPI
{
    PlatformPrintFunc* Print;
    PlatformPrintFunc* Error;
    PlatformReadEntireFileFunc* ReadEntireFile;

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

