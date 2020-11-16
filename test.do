
enum Types : i32
{
    // Still not sure what I want to do about the unsigned/signed stuff
    i8 := 0,
    i16,
    i32,
    i64,
    f32,
    f64,
    str,
    buf,    // ?
    fun,
}


struct EnumValue
{
    displayName: str;
    intensity:   i32;
    flags:       i32;
}

enum TeaTypes : EnumValue
{
    // Attribute names can be omited IIF you provide ALL of them (do the same for functions?)
    // (in fact maybe even do something like in Python where there is a kind of "correspondence" between function args
    // and struct fields, and you can trivially pack/unpack structs as function args)
    //
    // AActually, not sure if I would allow this, as fields can sometimes be re-ordered for packing reasons, and that can
    // easily lead to errors!
    Chai := { displayName = "Chai", intensity = 70 },
    EarlGrey := { displayName = "Earl Gray", intensity = 90 },
    Matcha := { displayName = "Matcha", intensity = 30 },
}



PrintAllTeaTypes :: ()
{
    // Could also be 'for i = 0, it : TeaTypes.items'
    for( i : 0..TeaTypes.items.count )
    {
        it := TeaTypes.items[i];
        print( "{i} - {it.displayName} has an intensity of {it.intensity}\n" );
    }

    switch( input )
    {
    case 1:
    {

    }
    case 2: bla;
    case default: ble;
    }
}
