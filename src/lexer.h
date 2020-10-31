
struct InternString
{
    char const* data;
    i32 length;
    u32 hash;
};

struct InternStringBuffer
{
    MemoryArena arena;
    // TODO This should be a _growable_ hashtable (with linear probing!)?
    BucketArray<InternString> entries;
};

