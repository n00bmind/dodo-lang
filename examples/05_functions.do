
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
        return x1 - x2;
    };

    y = doMoarStuff( 3, 5 );
    printf( "y = %d\n", y );
    expect( y == -2 );

    // Can be called in place
    ()
    {
        printf( "Hello lambda!\n" );
    }();

    // Optional arguments
    PrintStuff :: ( a: string, b: string = "World" )
    {
        printf( "%s %s\n", a, b );
    }

    PrintStuff( "Hello" );
    PrintStuff( "Hello", "Sailor" );

    // Multiple return values
    Values :: () -> int, int, int
    {
        return 10, 42, 100;
    }

    a, b, c := Values();
    printf( "a = %d, b = %d, c = %d\n", a, b, c );

    // Varargs
    PrintManyThings :: ( things: string... )
    {
        // 'things' is literally a buffer of string
        printf( "Got %lld things: ", things#length );

        for( t in ..things )
            printf( " %s", t );
        printf( "\n" );
    }

    PrintManyThings( "skrillex", "42", "techno", "cowabunga" );

    // Named arguments
    PrintStuff( b = "sensei!", a = "Ahoy" );

    y = doMoarStuff( x2 = 3, x1 = 9 );
    printf( "y = %d\n", y );
    expect( y == 6 );

    // TODO Function overloads
}

