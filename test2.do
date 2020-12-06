example_test :: () -> int
{
    return fact_rec( 10 ) == fact_iter( 10 );
}

IntOrPtr :: struct
{
    i: int;
    p: *int;
}

f :: ()
{
    u: IntOrPtr = { 42, 42 as *int };
    u.i = 0;
    u.p = 0 as *int;
}

iter: int;

Vector :: struct
{
    x, y: int;
}

fact_iter :: ( n: int ) -> int
{
    r := 1;
    for( i: 2..n )
    {
        r *= i;
    }
    return r;
}

fact_rec :: ( n: int ) -> int
{
    if( n == 0 )
    {
        return 1;
    }
    else
    {
        return n * fact_rec( n - 1 );
    }
}

