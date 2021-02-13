
// TODO 
#foreign printf :: ( fmt: string, args: any... );

main :: ()
{
    ///////////////////////////////////////////
    // Struct declarations are similar to C's
    Vector :: struct
    {
        x :f32;
        y :f32 = 1;
        z :f32 = 2;
    }

    // However initialization works differently

    // A struct with an initilizer expression will override any default initializer in the struct declaration
    // (for the fields included in the initializer)
    v :Vector = { .x = 10, .z = 1000 };
    printf( "v = { %f, %f, %f }\n", v.x, v.y, v.z );
    // A struct with no explicit initializer will use the default initializers in the declaration
    // Any fields with no default initializer will be zero initialized
    w :Vector;
    printf( "w = { %f, %f, %f }\n", w.x, w.y, w.z );

    // Field access always uses '.' even for pointers to structs
    pv := &v;
    printf( "v = { %f, %f, %f }\n", pv.x, pv.y, pv.z );

    // TODO Bitfield members

    // Structs can be nested
    Matrix :: struct
    {
        Vector :: struct
        {
            a, b, c := f32;
        }

        r :[3]Vector;
    }

    // TODO Make a rule whereby you can omit the field names when providing values for all fields?
    // (and if a new field gets added to the struct you get an error in the existing initializers)
    m :Matrix = { .r = { { .x = 1, .y = 0, .z = 0 }, { .x = 0, .y = 1, .z = 0 }, { .x = 0, .y = 0, .z = 1 } } };
    // Referring to inner types also uses the '.' operator
    r :Matrix.Vector = { 1, 2, 3 };

    m.r[1] = r;
    printf( "m =\n" );
    printf( "{ %f, %f, %f }\n", m.r[0].x, m.r[0].y, m.r[0].z );
    printf( "{ %f, %f, %f }\n", m.r[1].x, m.r[1].y, m.r[1].z );
    printf( "{ %f, %f, %f }\n", m.r[2].x, m.r[2].y, m.r[2].z );

    #expecterror
    {
        A :: struct
        {
            B :: struct
            { x: int }

            B :int;
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

    // Only one of the fields can have a default initializer though
    // Likewise with provided initializers in a declaration
    value :ConstValue = { .real = 3.14 };
    #expecterror value2 :ConstValue = { .integer = 1, .real = 0.5 };

    ///////////////////////////////////////////
    // Simple enums
    // TODO Think about all the initialization modes we want here
    TokenKind :: enum
    {
        None;
        Int = 10;
        Float = 20;
        String = 30;
        Identifier = 40;
    }

    // All simple enums are actually an alias for some underlying scalar type
    // When no type is declared, int is assumed, hence their size is 8 bytes
    tkk :TokenKind;
    printf( "TokenKind size = %d\n", tkk#size );

    // They're also initialized according to the same rules as everybody else
    printf( "tkk = %d\n", tkk );
    // Access to members is of course qualified
    tkk = TokenKind.String;
    printf( "tkk = %d\n", tkk );
    // They implicitly convert to/from their underlying type
    // TODO Do we want to optionally enforce at debug-time all values to belong to the declared set?
    //tokeKind = 25;

    // Printing the declared name instead of the value of enum members is extremely easy
    printf( "tkk = '%s'\n", tkk#enum_name );
    // It's easy to iterate over all declared values, which is also true for all aggregate types
    // This is left to the meta-programming example a bit later

    // TODO Example of an enum float

    // Simple enums with an underlying bits subtype are considered flags, hence follow a few special rules
    // For starters, when a value is not declared, they don't get consecutive integer values but values where a single bit
    // is progressively shifted to the left.
    // TODO Enum flags
    TokenFlags :: enum<b32>
    {
        None;
        PostfixOp;
        UnaryOp;
        AddOp;
        MulOp;
        CmpOp;
        AssignOp;
    }

    // TODO It's an error to declare more values than bits available in the underlying type
    // TODO How to deal with both declared and non-declared values
    // TODO Likewise synthetic values

    // Enums can be much more powerful though, when 'combined' with aggregates

    ///////////////////////////////////////////
    // Enum Structs

    // TODO 


    ///////////////////////////////////////////
    // Enum Unions (aka tagged unions)

    // TODO 

}
