/* TODO Varargs*/
#foreign printf :: ( fmt: string, i: int, v: int );
#foreign puts :: ( s: string );

main :: ( argc: int, argv: **i8 ) -> int
{
    // Passing any array to a function as a view
    PrintBuffer :: ( b: [*]int )
    {
        puts( '-----\n' );
        for( i in 0 .. b.count )
            printf( 'Item %d is %d\n', i, b[i] );
        puts( '\n' );
    }

    // Static arrays
    nums1: []int = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    PrintBuffer( nums1 );

    // Can initialize only some elements (the rest will be zero)
    nums2: []int = { [9] = 42 };
    PrintBuffer( nums2 );

    // TODO Can initialize using one or more indexed ranges
    /*nums3: []int = { [0..3] = 42, [10..13] = -1 };*/
    /*PrintBuffer( nums3 );*/

    // Can initialize using an 'open' range (as init value), which will fill the array with the same value
    // TODO What about data types other than int?
    // TODO If using a variable instead of a literal, the syntax becomes ambiguous with the one used to iterate over arrays in a for loop
    /*nums4: [5]int = { ..100 };*/
    /*PrintBuffer( nums4 );*/

    // Open ranges do require an explicitly sized array
    // TODO #expecterror({errors..}) expecting the given compiler error(s) to appear (will be swallowed) or produce a new error otherwise
    /*nums5: []int = { ..100 };*/
    /*PrintBuffer( nums5 );*/

    // Creating a buffer view from an array
    nums6: [*]int = nums1;
    PrintBuffer( nums6 );

    // Can be done straight from a literal too, which will point it to a value on the stack
    nums7: [*]int = { 42, -10, 1328 };
    PrintBuffer( nums7 );

    // Passing any array where a pointer to the same type is expected just works
    pointTo2: *int = nums2;
    // Same with a buffer view
    pointTo6: *int = nums6;

    // Creating a buffer view from pointer and count
    nums8: [*]int = { pointTo6, nums6.count };
    PrintBuffer( nums8 );

    // TODO Print addresses of things
    // TODO Array of arrays, buffer of buffers?

/*
// TODO Should be still parsed (resolved too?)
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
    return 0;
}
