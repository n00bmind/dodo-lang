
/////     DYNAMIC ARRAY    /////

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

    T*       begin()         { return data; }
    const T* begin() const   { return data; }
    T*       end()           { return data + count; }
    const T* end() const     { return data + count; }

    bool operator ==( Array<T> const& other ) const
    {
        return count == other.count && PEQUAL( data, other.data, count * sizeof(T) );
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

    // Deep copy
    Array<T> Copy( MemoryArena* arena ) const
    {
        Array<T> result( arena, count );
        PCOPY( data, result.data, count * sizeof(T) );
        result.count = count;
        return result;
    }

    void CopyTo( Array<T>* out ) const
    {
        ASSERT( out->capacity >= count );
        PCOPY( data, out->data, count * sizeof(T) );
        out->count = count;
    }

    void CopyTo( T* buffer ) const
    {
        PCOPY( data, buffer, count * sizeof(T) );
    }

    void CopyFrom( const T* buffer, int count_ )
    {
        ASSERT( count_ > 0 );
        count = count_;
        PCOPY( buffer, data, count * sizeof(T) );
    }

    bool Contains( const T& item ) const
    {
        bool result = false;
        for( int i = 0; i < count; ++i )
        {
            if( data[i] == item )
            {
                result = true;
                break;
            }
        }

        return result;
    }

    void Clear()
    {
        count = 0;
    }

    i32 Available() const
    {
        return capacity - count;
    }
};


/////     BUCKET ARRAY     /////

template <typename T>
struct BucketArray
{
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

    typedef bool (*Comparator)( T const& a, T const& b );

    T* Find( T const& it, Comparator cmp )
    {
        T* result = nullptr;

        auto idx = First();
        while( idx )
        {
            if( cmp( it, *idx ) )
                break;

            idx.Next();
        }

        if( idx )
            result = &*idx;

        return result;
    }

    void CopyTo( T* buffer, int bufferCount ) const
    {
        ASSERT( count <= bufferCount );

        const Bucket* bucket = &first;
        while( bucket )
        {
            PCOPY( bucket->data, buffer, bucket->count * sizeof(T) );
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
            PCOPY( bucket->data, buffer, bucket->count * sizeof(T) );
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

    T const& At( int i ) const
    {
        ASSERT( i >= 0 && i < count );

        Bucket const* base = &first;
        int index = i;

        while( index >= base->count )
        {
            index -= base->count;
            base = base->next;
        }

        return base->data[index];
    }

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
    return Hash64( &key, sizeof(K) );
}
template <typename K>
bool DefaultEqFunc( K const& a, K const& b )
{
    return a == b;
}

template <typename K>
const K ZeroKey = K();

// NOTE Type K must have a default constructor, and all keys must be different to this default-constructed value
template <typename K, typename V, typename Allocator>
struct Hashtable
{
    using HashFunc = u64 (*)( K const& key );
    using KeysEqFunc = bool (*)( K const& a, K const& b );

    K* keys;
    V* values;

    Allocator* allocator;
    HashFunc hashFunc;
    KeysEqFunc eqFunc;
    i32 count;
    i32 capacity;


    Hashtable()
        : keys( nullptr )
        , values( nullptr )
        , allocator( nullptr )
        , hashFunc( nullptr )
        , eqFunc( nullptr )
        , count( 0 )
        , capacity( 0 )
    {}

    Hashtable( Allocator* allocator_, int expectedSize = 0, HashFunc hashFunc_ = DefaultHashFunc<K>,
               KeysEqFunc eqFunc_ = DefaultEqFunc<K> )
        : keys( nullptr )
        , values( nullptr )
        , allocator( allocator_ )
        , hashFunc( hashFunc_ )
        , eqFunc( eqFunc_ )
        , count( 0 )
        , capacity( 0 )
    {
        if( expectedSize )
            Grow( 2 * expectedSize );
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

    V* Put( K const& key, V const& value )
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
                values[i] = value;
                ++count;
                return &values[i];
            }
            else if( eqFunc( keys[i], key ) )
            {
                values[i] = value;
                return &values[i];
            }

            i++;
            if( i == U32( capacity ) )
                i = 0;
        }
    }

    // TODO Deleting based on Robid Hood or similar

private:
    void Grow( int newCapacity )
    {
        newCapacity = Max( newCapacity, 16 );
        ASSERT( IsPowerOf2( newCapacity ) );

        int oldCapacity = capacity;
        K* oldKeys = keys;
        V* oldValues = values;

        count = 0;
        capacity = newCapacity;
        void* newMemory = ALLOC_SIZE( allocator, capacity * (sizeof(K) + sizeof(V)), NoClear() );
        keys   = (K*)newMemory;
        values = (V*)((u8*)newMemory + capacity * sizeof(K));

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

