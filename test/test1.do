
n :: 1 + sizeof(p);
p : *T;
u := *p;
T :: struct
{
    a: [n] int;
}

r := &tmp.a;
tmp: T;
s: [n+m]int = { 0, 1, 2 };
m :: sizeof(tmp.a);
sum := n + m;
q := &sum;
sx :: sizeof(x);
x: R;
R :: struct
{
    s: *S;
}

S :: struct
{
    r: [sx] R;
}
// TODO Do we want to just ignore these?
//;

U :: struct
{
    a: [3] int;
}

uu: U = { { 0 } };
a, b, c :: 0;
//b := 1;                                  // Redeclared
add :: (v: Vector, w: Vector) -> Vector { return { v.x + w.x, v.y + w.y }; }
vecResult := add_func( { 1, 2 }, { 3, 4 } );
add_func := add;
Vector :: struct { x, y: int; }
// TODO Check this actually works for all cond types
three := 1 ? 2 : 3;
ptr := &s[1 + 1];
// TODO Do we want to disallow indexing pointers (unchecked) so it feels less safe than indexing arrays (always checked)?
ptrItem := ptr[-1];
bin := 1000 / (2 + 3 * 5) << 10;
aptr: *int = -s[3] as *int;

f :: ()
{
    sum += 1;
}

h :: (y: int) -> int
{
    if(y)
    {
        return -y;
    }
    else
    {
        return 1;
    }
}

