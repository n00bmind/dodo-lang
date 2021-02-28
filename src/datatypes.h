template <typename T>
struct BucketArray;


/////     DYNAMIC ARRAY    /////

// Actually, a reference to an array of any size and type somewhere in memory, so more like a 'buffer view' as it's called in do-lang
// (although we have semantics for Push/Pop/Remove etc. so it is not read-only)

template <typename T>
struct Array
{
    T* data;
    i32 count;
    i32 capacity;


    Array()
    {
        data = nullptr;
        count = 0;
        capacity = 0;
    }

    // NOTE All newly allocated arrays start empty
    Array( MemoryArena* arena, i32 capacity_, MemoryParams params = DefaultMemoryParams() )
    {
        ASSERT( capacity_ >= 0 );
        data = PUSH_ARRAY( arena, T, capacity_, params );
        count = 0;
        capacity = capacity_;
    }

    // NOTE All arrays initialized from a buffer start with count equal to the full capacity
    Array( T* buffer, i32 bufferCount )
    {
        ASSERT( buffer && bufferCount > 0 );
        data = buffer;
        count = bufferCount;
        capacity = bufferCount;
    }

    Array( BucketArray<T> const& buckets, MemoryArena* arena, MemoryParams params = DefaultMemoryParams() )
        : Array( arena, buckets.count, params )
    {
        buckets.CopyTo( this );
    }

    T*       begin()         { return data; }
    const T* begin() const   { return data; }
    T*       end()           { return data + count; }
    const T* end() const     { return data + count; }

    bool operator ==( Array<T> const& other ) const
    {
        return count == other.count && PEQUAL( data, other.data, count * SIZEOF(T) );
    }


    void Resize( i32 new_count )
    {
        ASSERT( new_count >= 0 && new_count <= capacity );
        count = new_count;
    }

    void ResizeToCapacity()
    {
        count = capacity;
    }

    void Clear()
    {
        count = 0;
    }

    i32 Available() const
    {
        return capacity - count;
    }

    T& operator[]( int i )
    {
        ASSERT( i >= 0 && i < count );
        return data[i];
    }

    const T& operator[]( int i ) const
    {
        ASSERT( i >= 0 && i < count );
        return data[i];
    }

    T& Last()
    {
        ASSERT( count > 0 );
        return data[count - 1];
    }

    const T& Last() const
    {
        ASSERT( count > 0 );
        return data[count - 1];
    }

    // TODO Not too sure about this!
    explicit operator bool() const
    {
        return data != nullptr;
    }

    T* PushEmpty( bool clear = true )
    {
        ASSERT( count < capacity );
        T* result = data + count++;
        if( clear )
            *result = {};

        return result;
    }

    T* Push( const T& item )
    {
        T* slot = PushEmpty( false );
        *slot = item;

        return slot;
    }

    T Pop()
    {
        ASSERT( count > 0 );
        --count;
        return data[count];
    }

    void Remove( T* item )
    {
        ASSERT( count > 0 );

        sz index = item - data;
        ASSERT( index >= 0 && index < count );

        *item = Last();
        --count;
    }

    T* Find( const T& item )
    {
        T* result = nullptr;
        for( int i = 0; i < count; ++i )
        {
            if( data[i] == item )
            {
                result = &data[i];
                break;
            }
        }

        return result;
    }

    T const* Find( const T& item ) const
    {
        T* slot = ((Array<T>*)this)->Find( item );
        return slot;
    }

    bool Contains( T const& item ) const
    {
        T const* slot = Find( item );
        return slot != nullptr;
    }

    // Deep copy
    Array<T> Clone( MemoryArena* arena ) const
    {
        Array<T> result( arena, count );
        PCOPY( data, result.data, count * SIZEOF(T) );
        result.count = count;
        return result;
    }

    void CopyTo( Array<T>* out ) const
    {
        ASSERT( out->capacity >= count );
        PCOPY( data, out->data, count * SIZEOF(T) );
        out->count = count;
    }

    void CopyTo( T* buffer ) const
    {
        PCOPY( data, buffer, count * SIZEOF(T) );
    }

    void CopyFrom( const T* buffer, int count_ )
    {
        ASSERT( count_ > 0 && capacity >= count_ );
        PCOPY( buffer, data, count_ * SIZEOF(T) );
    }
};


/////     BUCKET ARRAY     /////

template <typename T>
bool EqualityComparator( T const& a, T const& b )
{
    return a == b;
}


template <typename T>
struct BucketArray
{
    using ComparatorFunc = bool (*)( T const& a, T const& b );

    // TODO Rewrite using Arrays with prev/next pointers as buckets
    struct Bucket
    {
        T* data;
        i32 count;
        i32 capacity;

        Bucket *next;
        Bucket *prev;

        Bucket()
            : data( nullptr )
            , count( 0 )
            , capacity( 0 )
        {}

        Bucket( MemoryArena* arena, i32 capacity_, MemoryParams params )
        {
            ASSERT( capacity_ > 0 );
            data = PUSH_ARRAY( arena, T, capacity_, params );
            capacity = capacity_;
            count = 0;
            next = prev = nullptr;
        }

        void Clear()
        {
            count = 0;
            next = prev = nullptr;
        }
    };

    // Stupid C++ shit to have both a const and non-const iterator without writing it twice
    // TODO Get rid of this
    template <bool Flag, typename IsTrue, typename IsFalse>
    struct Choose;

    template <typename IsTrue, typename IsFalse>
    struct Choose<true, IsTrue, IsFalse>
    {
        typedef IsTrue type;
    };
    template <typename IsTrue, typename IsFalse>
    struct Choose<false, IsTrue, IsFalse>
    {
        typedef IsFalse type;
    };

    template<bool IsConst>
    struct Idx
    {
        typedef typename Choose<IsConst, T const&, T&>::type ValueRef;
        typedef typename Choose<IsConst, Bucket const*, Bucket*>::type BucketPtr;

        // TODO Should probably just default to const and do a const_cast in the main class methods when needed
        BucketPtr base;
        i32 index;

        Idx( BucketPtr base_ = nullptr, i32 index_ = 0 )
            : base( base_ )
            , index( index_ )
        {}

        // FIXME Additionally check that base is not in the free list
        bool IsValid() const
        {
            return base && index >= 0 && index < base->count;
        }

        explicit operator bool() const
        {
            return base && index >= 0 && index < base->count;
        }

        operator ValueRef() const
        {
            ASSERT( IsValid() );
            return base->data[index];
        }

        bool operator ==( Idx<IsConst> const& other ) const
        {
            return base == other.base && index == other.index;
        }

        bool operator !=( Idx<IsConst> const& other ) const
        {
            return base != other.base || index != other.index;
        }

        ValueRef operator *() const
        {
            ASSERT( IsValid() );
            return base->data[index];
        }

        void Next()
        {
            if( index < base->count - 1 )
                index++;
            else
            {
                base = base->next;
                index = 0;
            }
        }

        Idx& operator ++()
        {
            Next();
            return *this;
        }

        Idx operator ++( int )
        {
            Idx result(*this);
            Next();

            return result;
        }

        void Prev()
        {
            if( index > 0 )
                index--;
            else
            {
                base = base->prev;
                index = base ? base->count - 1 : 0;
            }
        }

        Idx& operator --()
        {
            Prev();
            return *this;
        }

        Idx operator --( int )
        {
            Idx result(*this);
            Prev();

            return result;
        }
    };


    Bucket first;
    Bucket* last;
    Bucket* firstFree;

    MemoryArena* arena;
    MemoryParams memoryParams;

    i32 count;


    BucketArray()
    {}

    BucketArray( MemoryArena* arena_, i32 bucketSize, MemoryParams params = DefaultMemoryParams() )
        : first( arena_, bucketSize, params )
    {
        count = 0;
        last = &first;
        firstFree = nullptr;
        arena = arena_;
        memoryParams = params;
    }

    BucketArray( BucketArray<T> && other )
        : first( other.first )
        , last( other.last )
        , firstFree( other.firstFree )
        , arena( other.arena )
        , memoryParams( other.memoryParams )
        , count( other.count )
    {
        other.first = {};
        other.last = nullptr;
        other.firstFree = nullptr;
        other.arena = nullptr;
        other.memoryParams = {};
        other.count = 0;
    }

    // Disallow implicit copying
    BucketArray( const BucketArray& ) = delete;
    BucketArray& operator =( const BucketArray& ) = delete;

    T* PushEmpty( bool clear = true )
    {
        if( last->count == last->capacity )
            AddEmptyBucket();

        count++;
        T* result = &last->data[last->count++];
        if( clear )
            *result = {};

        return result;
    }

    T* Push( const T& item )
    {
        T* slot = PushEmpty( false );
        *slot = item;
        return slot;
    }

    T Remove( const Idx<false>& index )
    {
        ASSERT( count > 0 );
        ASSERT( index.IsValid() );

        // TODO Maybe add support for removing and shifting inside a bucket and make a separate RemoveSwap() method
        T result = (T&)index;

        ASSERT( last->count > 0 );
        // If index is not pointing to last item, find last item and swap it
        if( index.base != last || index.index != last->count - 1 )
        {
            T& lastItem = last->data[last->count - 1];
            (T&)index = lastItem;
        }

        last->count--;
        if( last->count == 0 && last != &first )
        {
            // Empty now, so place it at the beginning of the free list
            last->next = firstFree;
            firstFree = last;

            last = last->prev;
        }

        count--;
        return result;
    }

    T Pop()
    {
        T result = Remove( Last() );
        return result;
    }

    // Inclusive
    // TODO Need a way to convert between index const-ness when it makes sense, otherwise this IsConst shit is shit
    void PopUntil( const Idx<false>& index )
    {
        while( index.IsValid() )
            Pop();
    }

    // TODO Untested
#if 0
    // Not particularly fast
    T const& At( int index ) const
    {
        ASSERT( index >= 0 && index < count );

        Bucket const* base = &first;
        const int bucketSize = first.count;

        while( index >= bucketSize )
        {
            index -= bucketSize;
            base = base->next;
        }

        return base->data[index];
    }
    T& At( int index )
    {
        return const_cast<BucketArray const*>(this)->At( index );
    }

    void Grow( int newCount )
    {
        // TODO This can be used to create gaps, which might be a problem in other situations?
        if( newCount > count )
        {
            Bucket const* base = &first;
            const int bucketSize = first.count;

            int index = newCount;
            while( index >= bucketSize )
            {
                index -= bucketSize;
                if( base == last )
                    AddEmptyBucket();
                base = base->next;
            }
        }
    }

    void Resize( int newCount )
    {
        if( newCount > count )
            Grow( newCount );
        //else
            // TODO Add invalidated buckets to freelist?

        count = newCount;
    }
#endif

    T* Find( T const& item, ComparatorFunc cmp = EqualityComparator )
    {
        T* result = nullptr;

        auto idx = First();
        while( idx )
        {
            if( cmp( item, *idx ) )
                break;

            idx.Next();
        }

        if( idx )
            result = &*idx;

        return result;
    }

    T const* Find( T const& item, ComparatorFunc cmp = EqualityComparator ) const
    {
        T* result = ((BucketArray<T>*)this)->Find( item, cmp );
        return result;
    }

    bool Contains( T const& item, ComparatorFunc cmp = EqualityComparator ) const
    {
        T const* result = Find( item, cmp );
        return result != nullptr;
    }

    void AppendFrom( T const* buffer, int bufferCount )
    {
        int totalCopied = 0;
        Bucket*& bucket = last;

        while( totalCopied < bufferCount )
        {
            int remaining = bufferCount - totalCopied;
            int available = bucket->capacity - bucket->count;

            int copied = Min( remaining, available );
            PCOPY( buffer + totalCopied, bucket->data + bucket->count, copied * SIZEOF(T) );
            totalCopied += copied;

            bucket->count += copied;
            if( bucket->count == bucket->capacity )
                AddEmptyBucket();
        }

        count += totalCopied;
    }

    void CopyTo( T* buffer, sz bufferCount ) const
    {
        ASSERT( count <= bufferCount );

        const Bucket* bucket = &first;
        while( bucket )
        {
            PCOPY( bucket->data, buffer, bucket->count * SIZEOF(T) );
            buffer += bucket->count;
            bucket = bucket->next;
        }
    }

    void CopyTo( Array<T>* array ) const
    {
        ASSERT( count <= array->capacity );
        array->Resize( count );

        T* buffer = array->data;
        const Bucket* bucket = &first;
        while( bucket )
        {
            PCOPY( bucket->data, buffer, bucket->count * SIZEOF(T) );
            buffer += bucket->count;
            bucket = bucket->next;
        }
    }

    Array<T> ToArray( MemoryArena* arena_ ) const
    {
        Array<T> result( arena_, count );
        result.ResizeToCapacity();
        CopyTo( &result );

        return result;
    }

    void Clear()
    {
        if( last != &first )
        {
            // Place all chained buckets in the free list
            last->next = firstFree;
            firstFree = first.next;
        }

        first.Clear();
        count = 0;
        last = &first;
    }

    Idx<false> First()          { return { &first, 0 }; }
    Idx<true>  First() const    { return { &first, 0 }; }
    Idx<false> Last()           { return { last, last->count - 1 }; }
    Idx<true>  Last()  const    { return { last, last->count - 1 }; }
    Idx<false> End()            { return { last, last->count }; }
    Idx<true>  End()   const    { return { last, last->count }; }

    T& operator[]( const Idx<false>& idx )
    {
        ASSERT( idx.IsValid() );
        return (T&)idx;
    }

    const T& operator[]( const Idx<true>& idx ) const
    {
        ASSERT( idx.IsValid() );
        return (T const&)idx;
    }

    bool operator ==( Array<T> const& other ) const
    {
        if( count != other.count)
            return false;

        int i = 0;
        auto idx = First(); 
        while( idx )
        {
            T const& e = *idx;
            if( e != other[i++] )
                return false;

            idx.Next();
        }

        return true;
    }

private:

    void AddEmptyBucket()
    {
        Bucket* newBucket;
        if( firstFree )
        {
            newBucket = firstFree;
            firstFree = firstFree->next;
            newBucket->Clear();
        }
        else
        {
            newBucket = PUSH_STRUCT( arena, Bucket, memoryParams );
            *newBucket = Bucket( arena, first.capacity, memoryParams );
        }
        newBucket->prev = last;
        last->next = newBucket;

        last = newBucket;
    }
};


/////     HASHTABLE     /////

/*
After watching how Bitwise handles open addressing for pointer hashtables, I think I want to keep separate data structures
for pointer-sized data vs. the rest, when generalizing this data structure to hold any type of data.

So, for pointers we'd have something like:
*/

struct PtrHashtable
{
    struct Item
    {
        void* key;
        void* value;
    };

    Item* items;
    i32 count;
    i32 capacity;
};

/*
We're trying to optimize for the "hole-in-1" approach here, assuming a sufficiently good hash function, and we'll also keep the
occupancy quite low by doing an ASSERT( count * 2 < capacity ) before each insertion, so for the few times that we don't hit
the correct entry on the first try, it makes sense to keep the values inlined with the keys, as the correct result will be hopefully
just a couple entries away and hence will still be in the same cache line, so we'll _hopefully_ still only have one cache miss even if
we fail the first hit.

Now, for generic keys & values, that picture changes significantly, and having both keys & values inlined is now quite probably bad
for cache efficiency. Here then, we'd have them separate, which means a minimum of _two_ cache misses per lookup: */

template <typename T>
struct ItemHashtable
{
    u64* keys;
    T* values;

    i32 count;
    i32 capacity;
};

/*
So, for the generic case (and to be able to validate these assumptions through proper testing) we're going to want
to be able to have either keys + values as separate or as interleaved.

The problem is how to do this without having to write the code twice and without affecting performance.
We could somehow templatize an internal structure that dictates the layout, based on the size of K and V types, but then all access
would be through pass-through methods in that structure, which I guess is fine as long as they're inlined?
*/

template <typename K>
u64 DefaultHashFunc( K const& key )
{
    return Hash64( &key, I32( SIZEOF(K) ) );
}
template <typename K>
bool DefaultEqFunc( K const& a, K const& b )
{
    return a == b;
}

template <typename K>
const K ZeroKey = K();

enum HashTableFlags
{
    HTF_None = 0,
    HTF_FixedSize = 0x1,    // For stable pointers to content, must provide expectedSize as appropriate
};

// NOTE Type K must have a default constructor, and all keys must be different to the default-constructed value
template <typename K, typename V, typename Allocator>
struct Hashtable
{
    struct Item
    {
        K const& key;
        V& value;
    };

    using HashFunc = u64 (*)( K const& key );
    using KeysEqFunc = bool (*)( K const& a, K const& b );

    K* keys;
    V* values;

    Allocator* allocator;
    HashFunc hashFunc;
    KeysEqFunc eqFunc;
    i32 count;
    i32 capacity;
    u32 flags;


    Hashtable()
        : keys( nullptr )
        , values( nullptr )
        , allocator( nullptr )
        , hashFunc( nullptr )
        , eqFunc( nullptr )
        , count( 0 )
        , capacity( 0 )
        , flags( 0 )
    {}

    Hashtable( Allocator* allocator_, int expectedSize = 0, u32 flags_ = 0, HashFunc hashFunc_ = DefaultHashFunc<K>,
               KeysEqFunc eqFunc_ = DefaultEqFunc<K> )
        : keys( nullptr )
        , values( nullptr )
        , allocator( allocator_ )
        , hashFunc( hashFunc_ )
        , eqFunc( eqFunc_ )
        , count( 0 )
        , capacity( 0 )
        , flags( flags_ )
    {
        if( expectedSize )
            Grow( NextPowerOf2( 2 * expectedSize ) );
    }

    V* Get( K const& key )
    {
        ASSERT( !eqFunc( key, ZeroKey<K> ) );

        if( count == 0 )
            return nullptr;

        u64 hash = hashFunc( key );
        u32 i = hash & (capacity - 1);

        ASSERT( count < capacity );
        for( ;; )
        {
            if( eqFunc( keys[i], key ) )
                return &values[i];
            else if( eqFunc( keys[i], ZeroKey<K> ) )
                return nullptr;

            i++;
            if( i == U32( capacity ) )
                i = 0;
        }
    }

    V* PutEmpty( K const& key, const bool clear = true )
    {
        ASSERT( !eqFunc( key, ZeroKey<K> ) );

        // Super conservative but easy to work with
        if( 2 * count >= capacity )
            Grow( 2 * capacity );
        ASSERT( 2 * count < capacity );

        u64 hash = hashFunc( key );
        u32 i = hash & (capacity - 1);

        ASSERT( count < capacity );
        for( ;; )
        {
            if( eqFunc( keys[i], ZeroKey<K> ) )
            {
                keys[i] = key;
                if( clear )
                    INIT( values[i] ) V();
                ++count;
                return &values[i];
            }
            else if( eqFunc( keys[i], key ) )
            {
                if( clear )
                    INIT( values[i] ) V();
                return &values[i];
            }

            i++;
            if( i == U32( capacity ) )
                i = 0;
        }
    }

    V* Put( K const& key, V const& value )
    {
        V* result = PutEmpty( key, false );
        ASSERT( result );

        *result = value;
        return result;
    }

    // TODO Deleting based on Robid Hood or similar

    template <typename E>
    struct BaseIterator
    {
        Hashtable const& table;
        K* current;

        BaseIterator( Hashtable const& table_ )
            : table( table_ )
        {
            current = table.keys - 1;
            Next();
        }

        explicit operator bool() const { return current != nullptr; }

        virtual E operator *() const = 0;

        BaseIterator& operator ++()
        {
            Next();
            return *this;
        }

    private:
        void Next()
        {
            do
            {
                current++;
            }
            while( current < table.keys + table.capacity && 
                table.eqFunc( *current, ZeroKey<K> ) );

            if( current >= table.keys + table.capacity )
                current = nullptr;
        }
    };

    struct ItemIterator : public BaseIterator<Item>
    {
        ItemIterator( Hashtable const& table_ )
            : BaseIterator( table_ )
        {}

        Item operator *() const override
        {
            ASSERT( current );
            V& currentValue = table.values[ current - table.keys ];
            Hashtable::Item result = { *current, currentValue };
            return result;
        }
    };

    struct KeyIterator : public BaseIterator<K const&>
    {
        KeyIterator( Hashtable const& table_ )
            : BaseIterator( table_ )
        {}

        K const& operator *() const override
        {
            ASSERT( current );
            return *current;
        }
    };

    struct ValueIterator : public BaseIterator<V&>
    {
        ValueIterator( Hashtable const& table_ )
            : BaseIterator( table_ )
        {}

        V& operator *() const override
        {
            ASSERT( current );
            V& currentValue = table.values[ current - table.keys ];
            return currentValue;
        }
    };

    ItemIterator Items() { return ItemIterator( *this ); }
    KeyIterator Keys() { return KeyIterator( *this ); }
    ValueIterator Values() { return ValueIterator( *this ); }

private:
    void Grow( int newCapacity )
    {
        ASSERT( !(flags & HTF_FixedSize) || !capacity );

        newCapacity = Max( newCapacity, 16 );
        ASSERT( IsPowerOf2( newCapacity ) );

        int oldCapacity = capacity;
        K* oldKeys = keys;
        V* oldValues = values;

        count = 0;
        capacity = newCapacity;
        void* newMemory = ALLOC_SIZE( allocator, capacity * (SIZEOF(K) + SIZEOF(V)), NoClear() );
        keys   = (K*)newMemory;
        values = (V*)((u8*)newMemory + capacity * SIZEOF(K));

        for( int i = 0; i < capacity; ++i )
            INIT( keys[i] ) K();
        for( int i = 0; i < oldCapacity; ++i )
        {
            if( !eqFunc( oldKeys[i], ZeroKey<K> ) )
                Put( oldKeys[i], oldValues[i] );
        }

        FREE( allocator, oldKeys ); // Handles both
    }
};


/////     RING BUFFER    /////
// Circular buffer backed by an array with a stable maximum size (no allocations after init).
// Default behaviour is similar to a FIFO queue where new items are Push()ed onto a virtual "head" cursor,
// while Pop() returns the item at the "tail" (oldest). Can also remove newest item with PopHead() for a LIFO stack behaviour.
// Head and tail will wrap around when needed and can never overlap.
// Can be iterated both from tail to head (oldest to newest) or the other way around.

template <typename T>
struct RingBuffer
{
    Array<T> buffer;
    i32 onePastHeadIndex;       // Points to next slot to write
    i32 tailIndex;


    RingBuffer()
    { }

    RingBuffer( i32 capacity_, MemoryArena* arena, MemoryParams params = DefaultMemoryParams() )
        : buffer( arena, capacity_, params )
        , onePastHeadIndex( 0 )
        , tailIndex( 0 )
    {
        buffer.ResizeToCapacity();
    }

    static RingBuffer FromArray( Array<T> const& a )
    {
        int itemCount = a.count;

        RingBuffer result = {};
        // Make space for one more at the end
        result.buffer.Resize( itemCount + 1 );
        result.buffer.CopyFrom( a );

        result.onePastHeadIndex = itemCount;
        result.tailIndex = 0;

        return result;
    }

    void Clear()
    {
        onePastHeadIndex = 0;
        tailIndex = 0;
    }

    void ClearToZero()
    {
        Clear();
        PZERO( buffer.data, buffer.count * sizeof(T) );
    }

    T* PushEmpty( bool clear = true )
    {
        T* result = buffer.data + onePastHeadIndex++;

        if( onePastHeadIndex == buffer.capacity )
            onePastHeadIndex = 0;
        if( onePastHeadIndex == tailIndex )
        {
            tailIndex++;
            if( tailIndex == buffer.capacity )
                tailIndex = 0;
        }

        if( clear )
            PZERO( result, sizeof(T) );

        return result;
    }

    T* Push( const T& item )
    {
        T* result = PushEmpty( false );
        *result = item;
        return result;
    }

    T Pop()
    {
        ASSERT( Count() > 0 );
        int prevTailIndex = tailIndex;
        if( tailIndex != onePastHeadIndex )
        {
            tailIndex++;
            if( tailIndex == buffer.capacity )
                tailIndex = 0;
        }

        return buffer.data[tailIndex];
    }

    T PopHead()
    {
        ASSERT( Count() > 0 );
        int headIndex = onePastHeadIndex;
        if( tailIndex != onePastHeadIndex )
        {
            onePastHeadIndex--;
            if( onePastHeadIndex < 0 )
                onePastHeadIndex = buffer.capacity - 1;
            headIndex = onePastHeadIndex;
        }

        return buffer.data[headIndex];
    }

    int Count() const
    {
        int count = onePastHeadIndex - tailIndex;
        if( count < 0 )
            count += buffer.capacity;

        return count;
    }

    bool Contains( const T& item ) const
    {
        return buffer.Contains( item );
    }

    T& FromHead( int offset = 0 )
    {
        ASSERT( offset >= 0, "Only positive offsets" );
        ASSERT( Count() > offset, "Not enough items in buffer" );
        int index = IndexFromHeadOffset( -offset );
        return buffer.data[index];
    }

    T const& FromHead( int offset = 0 ) const
    {
        T& result = ((RingBuffer*)this)->FromHead( offset );
        return (T const&)result;
    }


    struct IteratorInfo
    {
    protected:
        RingBuffer<T>& b;
        T* current;
        i32 currentIndex;
        bool forward;

    public:
        IteratorInfo( RingBuffer& buffer_, bool forward_ = true )
            : b( buffer_ )
            , forward( forward_ )
        {
            currentIndex = forward ? b.tailIndex : b.IndexFromHeadOffset( 0 );
            current = b.Count() ? &b.buffer.data[currentIndex] : nullptr;
        }

        T* operator * () { return current; }
        T* operator ->() { return current; }

        // Prefix
        inline IteratorInfo& operator ++()
        {
            if( current )
            {
                if( forward )
                {
                    currentIndex++;
                    if( currentIndex >= b.buffer.capacity )
                        currentIndex = 0;

                    current = (currentIndex == b.onePastHeadIndex) ? nullptr : &b.buffer.data[currentIndex];
                }
                else
                {
                    currentIndex--;
                    if( currentIndex < 0 )
                        currentIndex = b.buffer.capacity - 1;

                    current = (currentIndex < b.tailIndex) ? nullptr : &b.buffer.data[currentIndex];
                }
            }

            return *this;
        }
    };
    IteratorInfo Iterator( bool forward = true )
    {
        return IteratorInfo( *this, forward );
    }

private:
    int IndexFromHeadOffset( int offset )
    {
        int result = onePastHeadIndex + offset - 1;
        while( result < 0 )
            result += buffer.capacity;
        while( result >= buffer.capacity )
            result -= buffer.capacity;

        return result;
    }
};


void TestHashtable()
{
    persistent LazyAllocator lazyAllocator;
    Hashtable<void*, void*, LazyAllocator> table( &lazyAllocator );

    const int N = 1024 * 1024;
    for( int i = 1; i < N; ++i )
        table.Put( (void*)i, (void*)(i + 1) );
    for( int i = 1; i < N; ++i )
        ASSERT( *table.Get( (void*)i ) == (void*)(i + 1) );
}

