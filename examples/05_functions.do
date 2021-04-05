
#foreign printf :: ( string, any... );

main :: ()
{
    // Inner functions
    Factorial :: ( x: int ) -> int
    {
        return x ? x * Factorial( x - 1 ) : 1;
    }

    // Function 'pointer'
    doStuff := Factorial;

    y := doStuff( 10 );
    printf( "y = %d\n", y );
    expect( y == 3628800 );

    // Lambda expression stored in a variable
    // TODO Do we wanna just add a 'func' keyword to avoid backtracking during parsing?
    doMoarStuff := ( x1: int, x2: int ) -> int
    {
        // FIXME 
        #debugbreak
        return x1 * x2;
    };

    y = doMoarStuff( 3, 5 );
    printf( "y = %d\n", y );
    expect( y == 15 );

    // Can be called in place
    ()
    {
        printf( "Hello lambda!" );
    }();

    // Optional arguments
    PrintStuff :: ( a: string, b: string = "World" )
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
    printf( "a = %d, b = %d\n", a, b );

    // Varargs
    PrintManyThings :: ( things: string... )
    {
        // 'things' is literally a buffer of string
        printf( "Got %d things: ", things#length );

        for( t in ..things )
            printf( " %s", t );
        printf( "\n" );
    }

    PrintManyThings( "skrillex", "42", "techno", "cowabunga" );

    // Named arguments
    PrintStuff( a = "Ahoy", b = "sensei!" );

    y = doMoarStuff( x2 = 3, x1 = 9 );
    printf( "y = %d\n", y );
    expect( y == 27 );

    // TODO Function overloads
}

