
// TODO 
#foreign printf :: ( fmt: string, args: any... );

// TODO
main :: ()
{
    // Inner functions
    Factorial :: ( x :int ) -> int
    {
        return x ? x * Factorial( x - 1 ) : 1;
    }

    y := Factorial( 10 );
    printf( "y = %d\n", y );
    expect( y == 3628800 );

    // Lambdas
    doMoarStuff := ( x1, x2 :int ) -> int
    {
        return x1 * x2;
    }

    y = doMoarStuff( 3, 5 );
    printf( "y = %d\n", y );
    expect( y == 15 );

    // Optional arguments
    PrintStuff :: ( a :string, b :string = "World" )
    {
        printf( "%s %s", a, b );
    }

    PrintStuff( "Hello" );
    PrintStuff( "Hello", "Sailor" );

    // Multiple return values
    Values :: () -> int, int
    {
        return 10, 42;
    }

    a, b := Values();

    // Varargs
    PrintManyThings :: ( things :string... )
    {
        // 'things' is literally a buffer of string
        printf( "Got %d things: ", things#count );

        for( t in ..things )
            printf( " %s", t );
        printf( "\n" );
    }

    PrintManyThings( "skrillex", "42", "techno", "cowabunga" );

    // Named arguments
    PrintStuff( a = "Ahoy", b = "sensei!" );
}

