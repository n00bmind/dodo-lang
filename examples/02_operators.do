
#foreign printf :: ( fmt: string, args: any... );

main :: ()
{
    ////////////////////////////////////////////
    // ARITHMETIC

    // As mentioned, only (signed) ints and floats can be part of arithmetic expressions
    a, b, c :int = 9, 4;

    c = a + b;
    printf( "a + b = %d \n", c );
    expect( c == 13 );

    c = a - b;
    printf( "a - b = %d \n", c );
    expect( c == 5 );

    c = a * b;
    printf( "a * b = %d \n", c );
    expect( c == 36 );

    c = a / b;
    printf( "a / b = %d \n", c );
    expect( c == 2 );

    c = a % b;
    printf( "a %% b = %d \n", c );
    expect( c == 1 );


    x, y, z :f32 = 12.5, 1.44;

    z = x + y;
    printf( "x + y = %f \n", z );
    // TODO ~= ?
    //expect( z == 13 );

    z = x - y;
    printf( "x - y = %f \n", z );
    //expect( z == 5 );

    z = x * y;
    printf( "x * y = %f \n", z );
    //expect( z == 36 );

    z = x / y;
    printf( "x / y = %f \n", z );
    //expect( z == 2 );

    // Integer-to-float promotion when both together in an operation works as you'd expect
    z = a + y;
    printf( "a + y = %f \n", z );

    // Conversion back to integer requires a cast though
    #expecterror { c = x - b; }
    c = <int> ( x - b );
    expect( c == 8 );

    ////////////////////////////////////////////
    // ASSIGNMENT

    c = a;
    printf( "c = %d\n", c );
    expect( c == 9 );

    c += a;
    printf( "c = %d\n", c );
    expect( c == 18 );

    c -= a;
    printf( "c = %d\n", c );
    expect( c == 9 );

    c *= a;
    printf( "c = %d\n", c );
    expect( c == 81 );

    c /= a;
    printf( "c = %d\n", c );
    expect( c == 9 );

    c %= a;
    printf( "c = %d\n", c );
    expect( c == 0 );

    // TODO Add missing ones

    // There's also multiple assignment using comma expressions
    a, b, c = 5, 5, 10;
    result :bool;

    ////////////////////////////////////////////
    // EQUALITY / INEQUALITY

    result = a == b;
    printf( "%d == %d is %d \n", a, b, result );
    expect( result == true );

    result = a == c;
    printf( "%d == %d is %d \n", a, c, result );
    expect( result == false );

    result = a > b;
    printf( "%d > %d is %d \n", a, b, result );
    expect( result == false );

    result = a > c;
    printf( "%d > %d is %d \n", a, c, result );
    expect( result == false );

    result = a < b;
    printf( "%d < %d is %d \n", a, b, result );
    expect( result == false );

    result = a < c;
    printf( "%d < %d is %d \n", a, c, result );
    expect( result == true );

    result = a != b;
    printf( "%d != %d is %d \n", a, b, result );
    expect( result == false );

    result = a != c;
    printf( "%d != %d is %d \n", a, c, result );
    expect( result == true );

    result = a >= b;
    printf( "%d >= %d is %d \n", a, b, result );
    expect( result == true );

    result = a >= c;
    printf( "%d >= %d is %d \n", a, c, result );
    expect( result == false );

    result = a <= b;
    printf( "%d <= %d is %d \n", a, b, result );
    expect( result == true );

    result = a <= c;
    printf( "%d <= %d is %d \n", a, c, result );
    expect( result == true );

    ////////////////////////////////////////////
    // LOGICAL
    //a, b, c = 5, 5, 10;
    //result :bool;

    // TODO Can we get rid entirely of &&, || and ! ?
    // The results should be equivalent, we'd just need to take care of short-cutting?
    // The main issue I think would be what to do in the presence of both bools and non-bools

    result = (a == b) && (c > b);
    printf( "(a == b) && (c > b) is %d \n", result );
    expect( result == true );

    result = (a == b) && (c < b);
    printf( "(a == b) && (c < b) is %d \n", result );
    expect( result == false );

    result = (a == b) || (c < b);
    printf( "(a == b) || (c < b) is %d \n", result );
    expect( result == true );

    result = (a != b) || (c < b);
    printf( "(a != b) || (c < b) is %d \n", result );
    expect( result == false );

    result = !(a != b);
    printf( "!(a != b) is %d \n", result );
    expect( result == true );

    result = !(a == b);
    printf( "!(a == b) is %d \n", result );
    expect( result == false );

    ////////////////////////////////////////////
    // BITWISE

    a, b = 12, 25;
    c = a & b;
    printf( "%d & %d = %d \n", a, b, c );
    expect( c == 8 );

    c = a | b;
    printf( "%d | %d = %d \n", a, b, c );
    expect( c == 29 );

    c = a ^ b;
    printf( "%d ^ %d = %d \n", a, b, c );
    expect( c == 21 );

    c = ~-a;
    printf( "~-%d = %d\n", a, c );
    expect( c == 11 );

    c = ~b;
    printf( "~%d = %d\n", b, c );
    expect( c == -26 );

    // TODO What's our behaviour on over/underflow?
    // TODO Should we have an option to enable assertions on overflow similar to ABC?
    a = 212;
    for( i in 0..32 )
    {
        c = a >> i;
        printf( "%d >> %d = %d\n", a, i, c );
    }
    for( i in 0..32 )
    {
        c = a << i;
        printf( "%d << %d = %d\n", a, i, c );
    }

}
