
/////     BUFFER    /////

struct Buffer
{
    void* data;
    sz size;
};

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
            PZERO( result, sizeof(T) );

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

        Idx( BucketPtr base_, i32 index_ )
            : base( base_ )
            , index( index_ )
        {}

        explicit operator bool() const
        {
            return IsValid();
        }

        bool IsValid() const
        {
            return base && index >= 0 && index < base->count;
        }

        operator ValueRef() const
        {
            ASSERT( IsValid() );
            return base->data[index];
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

        Idx operator ++( int _ )
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

        Idx operator --( int _ )
        {
            Idx result(*this);
            Prev();

            return result;
        }
    };


    Bucket first;
    Bucket* last;
    Bucket* firstFree;
    i32 count;

    MemoryArena* arena;
    MemoryParams memoryParams;


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

    T* PushEmpty()
    {
        if( last->count == last->capacity )
            AddEmptyBucket();

        count++;
        return &last->data[last->count++];
    }

    T* Push( const T& item )
    {
        T* slot = PushEmpty();
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
        T result = Remove( { last, last->count - 1 } );
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

    Idx<false> First()
    {
        return { &first, 0 };
    }

    Idx<true> First() const
    {
        Idx<true> result( &first, 0 );
        return result;
    }

    Idx<false> Last()
    {
        return { last, last->count - 1 };
    }

    Idx<true> Last() const
    {
        return { last, last->count - 1 };
    }

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

    // TODO Remove (use above?)
    T& operator[]( int i )
    {
        // My love for C++ grows and grows unbounded
        return (T&)At( i );
    }

    T const& operator[]( int i ) const
    {
        return At( i );
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

