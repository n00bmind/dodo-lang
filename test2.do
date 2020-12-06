
example_test0 :: () -> int
{
    return fact_rec0( 10 ) == fact_iter0( 10 );
}

IntOrPtr0 :: struct
{
    i: int;
    p: *int;
}

f0 :: ()
{
    u: IntOrPtr0 = { 42, 42 as *int };
    u.i = 0;
    u.p = 0 as *int;
}

iter0: int;

Vector0 :: struct
{
    x, y: int;
}

fact_iter0 :: ( n: int ) -> int
{
    r := 1;
    for( i: 2..n )
    {
        r *= i;
    }
    return r;
}

fact_rec0 :: ( n: int ) -> int
{
    if( n == 0 )
    {
        return 1;
    }
    else
    {
        return n * fact_rec0( n - 1 );
    }
}

n0 :: 1 + sizeof(p0);

p0: *T0;

T0 :: struct
{
    a: [n0]int;
}


example_test1 :: () -> int
{
    return fact_rec1( 10 ) == fact_iter1( 10 );
}

IntOrPtr1 :: struct
{
    i: int;
    p: *int;
}

f1 :: ()
{
    u: IntOrPtr1 = { 42, 42 as *int };
    u.i = 0;
    u.p = 0 as *int;
}

iter1: int;

Vector1 :: struct
{
    x, y: int;
}

fact_iter1 :: ( n: int ) -> int
{
    r := 1;
    for( i: 2..n )
    {
        r *= i;
    }
    return r;
}

fact_rec1 :: ( n: int ) -> int
{
    if( n == 0 )
    {
        return 1;
    }
    else
    {
        return n * fact_rec1( n - 1 );
    }
}

n1 :: 1 + sizeof(p1);

p1: *T1;

T1 :: struct
{
    a: [n1]int;
}


example_test2 :: () -> int
{
    return fact_rec2( 10 ) == fact_iter2( 10 );
}

IntOrPtr2 :: struct
{
    i: int;
    p: *int;
}

f2 :: ()
{
    u: IntOrPtr2 = { 42, 42 as *int };
    u.i = 0;
    u.p = 0 as *int;
}

iter2: int;

Vector2 :: struct
{
    x, y: int;
}

fact_iter2 :: ( n: int ) -> int
{
    r := 1;
    for( i: 2..n )
    {
        r *= i;
    }
    return r;
}

fact_rec2 :: ( n: int ) -> int
{
    if( n == 0 )
    {
        return 1;
    }
    else
    {
        return n * fact_rec2( n - 1 );
    }
}

n2 :: 1 + sizeof(p2);

p2: *T2;

T2 :: struct
{
    a: [n2]int;
}


example_test3 :: () -> int
{
    return fact_rec3( 10 ) == fact_iter3( 10 );
}

IntOrPtr3 :: struct
{
    i: int;
    p: *int;
}

f3 :: ()
{
    u: IntOrPtr3 = { 42, 42 as *int };
    u.i = 0;
    u.p = 0 as *int;
}

iter3: int;

Vector3 :: struct
{
    x, y: int;
}

fact_iter3 :: ( n: int ) -> int
{
    r := 1;
    for( i: 2..n )
    {
        r *= i;
    }
    return r;
}

fact_rec3 :: ( n: int ) -> int
{
    if( n == 0 )
    {
        return 1;
    }
    else
    {
        return n * fact_rec3( n - 1 );
    }
}

n3 :: 1 + sizeof(p3);

p3: *T3;

T3 :: struct
{
    a: [n3]int;
}


example_test4 :: () -> int
{
    return fact_rec4( 10 ) == fact_iter4( 10 );
}

IntOrPtr4 :: struct
{
    i: int;
    p: *int;
}

f4 :: ()
{
    u: IntOrPtr4 = { 42, 42 as *int };
    u.i = 0;
    u.p = 0 as *int;
}

iter4: int;

Vector4 :: struct
{
    x, y: int;
}

fact_iter4 :: ( n: int ) -> int
{
    r := 1;
    for( i: 2..n )
    {
        r *= i;
    }
    return r;
}

fact_rec4 :: ( n: int ) -> int
{
    if( n == 0 )
    {
        return 1;
    }
    else
    {
        return n * fact_rec4( n - 1 );
    }
}

n4 :: 1 + sizeof(p4);

p4: *T4;

T4 :: struct
{
    a: [n4]int;
}


example_test5 :: () -> int
{
    return fact_rec5( 10 ) == fact_iter5( 10 );
}

IntOrPtr5 :: struct
{
    i: int;
    p: *int;
}

f5 :: ()
{
    u: IntOrPtr5 = { 42, 42 as *int };
    u.i = 0;
    u.p = 0 as *int;
}

iter5: int;

Vector5 :: struct
{
    x, y: int;
}

fact_iter5 :: ( n: int ) -> int
{
    r := 1;
    for( i: 2..n )
    {
        r *= i;
    }
    return r;
}

fact_rec5 :: ( n: int ) -> int
{
    if( n == 0 )
    {
        return 1;
    }
    else
    {
        return n * fact_rec5( n - 1 );
    }
}

n5 :: 1 + sizeof(p5);

p5: *T5;

T5 :: struct
{
    a: [n5]int;
}


example_test6 :: () -> int
{
    return fact_rec6( 10 ) == fact_iter6( 10 );
}

IntOrPtr6 :: struct
{
    i: int;
    p: *int;
}

f6 :: ()
{
    u: IntOrPtr6 = { 42, 42 as *int };
    u.i = 0;
    u.p = 0 as *int;
}

iter6: int;

Vector6 :: struct
{
    x, y: int;
}

fact_iter6 :: ( n: int ) -> int
{
    r := 1;
    for( i: 2..n )
    {
        r *= i;
    }
    return r;
}

fact_rec6 :: ( n: int ) -> int
{
    if( n == 0 )
    {
        return 1;
    }
    else
    {
        return n * fact_rec6( n - 1 );
    }
}

n6 :: 1 + sizeof(p6);

p6: *T6;

T6 :: struct
{
    a: [n6]int;
}


example_test7 :: () -> int
{
    return fact_rec7( 10 ) == fact_iter7( 10 );
}

IntOrPtr7 :: struct
{
    i: int;
    p: *int;
}

f7 :: ()
{
    u: IntOrPtr7 = { 42, 42 as *int };
    u.i = 0;
    u.p = 0 as *int;
}

iter7: int;

Vector7 :: struct
{
    x, y: int;
}

fact_iter7 :: ( n: int ) -> int
{
    r := 1;
    for( i: 2..n )
    {
        r *= i;
    }
    return r;
}

fact_rec7 :: ( n: int ) -> int
{
    if( n == 0 )
    {
        return 1;
    }
    else
    {
        return n * fact_rec7( n - 1 );
    }
}

n7 :: 1 + sizeof(p7);

p7: *T7;

T7 :: struct
{
    a: [n7]int;
}


example_test8 :: () -> int
{
    return fact_rec8( 10 ) == fact_iter8( 10 );
}

IntOrPtr8 :: struct
{
    i: int;
    p: *int;
}

f8 :: ()
{
    u: IntOrPtr8 = { 42, 42 as *int };
    u.i = 0;
    u.p = 0 as *int;
}

iter8: int;

Vector8 :: struct
{
    x, y: int;
}

fact_iter8 :: ( n: int ) -> int
{
    r := 1;
    for( i: 2..n )
    {
        r *= i;
    }
    return r;
}

fact_rec8 :: ( n: int ) -> int
{
    if( n == 0 )
    {
        return 1;
    }
    else
    {
        return n * fact_rec8( n - 1 );
    }
}

n8 :: 1 + sizeof(p8);

p8: *T8;

T8 :: struct
{
    a: [n8]int;
}


example_test9 :: () -> int
{
    return fact_rec9( 10 ) == fact_iter9( 10 );
}

IntOrPtr9 :: struct
{
    i: int;
    p: *int;
}

f9 :: ()
{
    u: IntOrPtr9 = { 42, 42 as *int };
    u.i = 0;
    u.p = 0 as *int;
}

iter9: int;

Vector9 :: struct
{
    x, y: int;
}

fact_iter9 :: ( n: int ) -> int
{
    r := 1;
    for( i: 2..n )
    {
        r *= i;
    }
    return r;
}

fact_rec9 :: ( n: int ) -> int
{
    if( n == 0 )
    {
        return 1;
    }
    else
    {
        return n * fact_rec9( n - 1 );
    }
}

n9 :: 1 + sizeof(p9);

p9: *T9;

T9 :: struct
{
    a: [n9]int;
}

