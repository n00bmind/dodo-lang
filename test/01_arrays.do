
main :: ( argc: int, argv: **i8 ) -> int
{
    // Passing any array to a function as a view
    PrintBuffer :: ( b: [*]int )
    {
        for( i in 0..b.count )
            printf( 'Item %d is %d\n', i, b[i] );
    }

    // Static arrays
    nums1: []int = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    PrintBuffer( nums1 );

    // Can initialize only some elements (the rest will be zero)

    // Can initialize using an 'open' range, which will fill the array with the same value

    nums2: [3]int = { 42, -10, 1328 };
    PrintBuffer( nums2 );

    // Creating a buffer view from an array
    nums3: [*]int = nums1;
    PrintBuffer( nums3 );

    // Can be done straight from a literal too
    nums4: [*]int = { ..100 };
    PrintBuffer( nums4 );

    // Passing any array where a pointer to the same type is expected just works
    nums5: *int = nums2;

    // Creating a buffer view from pointer and count
    nums6: [*]int = { nums5, nums2.count };
    PrintBuffer( nums6 );


/*
// Still parsed (resolved too?)
#if 0
{
    // TODO
    // Sbuffer struct
    sbuffer<T> :: struct
    {
        data: [*]T;
        capacity: i32;
        alloc: &Allocator;
    }






    // TODO
    // ?? Sbuffer in the language
    // TODO How do we associate this concept with the allocator it needs to get its memory?
    numsBuffer: [..]int;

    for( i in 0..10 )
        numsBuffer.Push( 10 - i );
    printf( 'Sbuffer count: %d, capacity: %d\n', numsBuffer.count, numsBuffer.capacity );

    for( i in 0..10 )
    {
        if( i % 2 )
            numsBuffer.Remove( i );
    }
    printf( 'Sbuffer count: %d, capacity: %d\n', numsBuffer.count, numsBuffer.capacity );
}
*/
}
