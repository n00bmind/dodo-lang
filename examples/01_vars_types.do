
#foreign printf :: ( fmt: string, args: any... );

main :: ()
{
    // Declarations use Sean Barret's syntax
    number :int = 0x42;
    printf( "number = %d\n", number );

    char: b8 = '\'';
    printf( "char = '%c' (%d)\n", char, char );

    real :f32 = -2.0;
    printf( "real = %f\n", real );

    // Strings are quite different from C, they're essentially a buffer pointing to an array of bytes
    // (buffers will be explained better in a later example)
    greeting :string = "Hello Sailor!";
    // Oh, and they're also readonly
    #expecterror{ greeting[6] = 'T'; }
    char = greeting[6];
    printf( "char = '%c' (%d)\n", char, char );

    // They're null terminated so they can be passed to C easily
    printf( "greeting = '%s'\n", greeting );
    // They _also_ have a length field so we never have to be searching for a terminator
    printf( "greeting.length = %d\n", greeting.length );
    // TODO Use a flag to mark which ones are null terminated and which arent?

    // Constants use a second : instead of =
    // They're actual compile-time constexprs, so they don't take any space
    pi :: 3.145926;
    // TODO How to show maximum precision here?
    printf( "pi = %f\n", pi );

    // Can declare several vars in one go
    a, b, c := 1, 2, 3;
    printf( "a = %d, b = %d, c = %d\n", a, b, c );

    // They all need to either have a explicit typespec, or be given a value
    // (anything not given an explicit initial value is zero initialized)
    #expecterror { x, y, z := 0, 1; }
    x, y, z :int = 0, 1;
    printf( "x = %d, y = %d, z = %d\n", a, b, c );

    // TODO Add alot more unary and binary expressions with all the various allowed types

    ///////////////////////////////////////////

    // There's no unsigned arithmetic type, all integer arithmetic is signed

    // There is however a 'bits' type (unsigned), for storing things like flags, hashes and whatnot
    hash :b32 = 0xabcdef00;
    printf( "hash = %x (%u)\n", hash, hash );

    // Conversion to integer and back is only allowed via casting (although literals of any sign auto convert to both)
    #expecterror { noCanDo :i32 = hash; }
    yesCanDo :i32 = <i32>hash;

    // Booleans are a special kind of bits type. Its natural size is 8 bits, although only the least significant is used
    fact := true;

    // Any other scalar type can convert implicity to boolean, the zero value converts to false, any other value is true
    fact = pi;
    printf( "fact = %s\n", fact ? "true" : "false" );

    // TODO Do bools implicitly convert to int?


    ///////////////////////////////////////////

    // Pointers use a similar format to C
    // TODO Does pi have an address?
    //p :*int = &pi;
    p :*int = &a;
    pp :**int = &p;

    printf( "&a = %x\n", p );
    printf( "&p = %x\n", pp );
    printf( "*p = %d\n", *p );

    expect( *p == a );

    *p = 10;
    printf( "a = %d\n", a );
    expect( a == 10 );

    // TODO Pointer arithmetic


    ///////////////////////////////////////////

    // There's also the 'any' type, which can hold any type of value, and which has information about which particular type
    // it's holding
    // TODO 
    

    ///////////////////////////////////////////
    // TODO Casting and subtype promotions


    ///////////////////////////////////////////
    // TODO Value ranges (#max & #min)
}

