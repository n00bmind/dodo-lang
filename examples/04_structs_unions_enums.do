
#foreign printf :: ( fmt: string, args: any... );

main :: ()
{
    // Structs can be nested
    Matrix :: struct
    {
        Vector :: struct
        {
            x, y, z: f32;
        }

        r :[3]Vector;
    }
    
    // FIXME 
    #expect( Matrix.r#offset == Matrix.Vector.z#offset );

    // TODO Make a rule whereby you can omit the field names when providing values for all fields?
    // (and if a new field gets added to the struct you get an error in the existing initializers)
    m :Matrix = { .r = { { .x = 1, .y = 0, .z = 0 }, { .x = 0, .y = 1, .z = 0 }, { .x = 0, .y = 0, .z = 1 } } };
    // Referring to inner types also uses the '.' operator
    r :Matrix.Vector = { .x = 1, .y = 2, .z = 3 };

    m.r[1] = r;
    printf( "m =\n" );
    printf( "{ %f, %f, %f }\n", m.r[0].x, m.r[0].y, m.r[0].z );
    printf( "{ %f, %f, %f }\n", m.r[1].x, m.r[1].y, m.r[1].z );
    printf( "{ %f, %f, %f }\n", m.r[2].x, m.r[2].y, m.r[2].z );

    // Inner structs and unions can also be anonymous like in C
    // TODO 

    // TODO Bitfield members


    // FIXME 
    //#expect_error
    {
        A :: struct
        {
            B :: struct
            { x: int; }

            B :int;         // Duplicate symbol
        }
    }

    #expect_error
    {
        Token :: struct
        {
            Sub :: struct
            {
                //#debug_break
                subtoken: Token;    // Completion cycle
                ptr: *void;
            }
            kind: int;
            sub: Sub;
        }
    }

    ///////////////////////////////////////////
    // Unions are also similar to C
    ConstValue :: union
    {
        ptr :*void = null;
        integer :i64;
        real :f32;
        byte :b8;
    }

    // Only one of the fields can have a default initializer
    // Likewise with provided initializers in a declaration
    value: ConstValue = { .real = 3.14 };

    #expect_error
    {
        ConstValueInit :: union
        {
            ptr :*void = null;
            integer :i64;
            real :f32 = 1.0;
            byte :b8;
        }
    }
    #expect_error
    value2 :ConstValue = { .integer = 1, .real = 0.5 };

    ///////////////////////////////////////////
    // Simple enums
    // All simple enums are actually an alias for some underlying readonly value
    // 
    // There's always an implicit first item None/0 for values not matching the declared items? (or for when explicitly set)
    // TODO Think about all the initialization modes and interdependencies here
    TokenKind :: enum
    {
        Int = 10,
        Float = 20,
        String = 30,
        Identifier = 40,
    }

    // When no type is declared, int is assumed
    tkk :TokenKind;
    printf( "TokenKind size = %lld\n", tkk#size );

    // They're also initialized according to the same rules as everybody else
    printf( "tkk = %d\n", tkk );
    // Access to members is of course qualified with a dot
    tkk = TokenKind.String;
    printf( "tkk = %d\n", tkk );
    // They implicitly convert to/from their underlying value type
    // TODO Do we want to optionally enforce at debug-time all values to belong to the declared set?
    //tokeKind = 25;

    // Printing the declared name instead of the value of enum members is extremely easy
    printf( "tkk = '%s'\n", tkk#name );
    // It's easy to iterate over all declared values, which is also true for all aggregate types
    // This is left to the meta-programming example a bit later

    // TODO Example of an enum float

    // Simple enums with an underlying bits subtype are considered flags, hence follow a few special rules
    // For starters, when values are not specified, they won't be consecutive integer values but values where a single bit
    // is progressively shifted to the left.
    // TODO Enum flags
    TokenFlags :: enum<b32>
    {
        PostfixOp,
        UnaryOp,
        AddOp,
        MulOp,
        CmpOp,
        AssignOp,
    }

    // TODO Custom declared values are allowed as normal. All values are kept sorted, so when getting an enum item from its value
    // in can be found by a simple binary search (do the same for all enums, in fact).

    // Enums can be much more powerful though, when 'combined' with aggregates

    ///////////////////////////////////////////
    // Enum Structs
    // The struct can be declared externally and then used as the underlying type of the enum, or declared directly inside of it
    // The values given to enum items are readonly like for all enums (all items must be given a value for non-scalar enums)
    // That however doesn't prevent you from having a constant pointer to some writable struct for storage

    // TODO 
    /*
    State :: enum
    {
        struct
        {
            prev, next: *State;
            kind: TokenKind;
            condition: ( int ) -> bool;
        }
        Start       = { null, State1, TokenKind.Int, null },
        State1      = { Start, State2, TokenKind.String, null },
        State2      = { State1, Tranform, TokenKind.Int, null },
        Transform   = { State2, End, TokenKind.Float, null },
        End         = { Transform, null, TokenKind.None, null },
    }
    */



    ///////////////////////////////////////////
    // Enum Unions (aka tagged unions)

    // TODO 

}
