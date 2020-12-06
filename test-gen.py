#!py3
from string import Template

template_str = """
example_test$index :: () -> int
{
    return fact_rec$index( 10 ) == fact_iter$index( 10 );
}

IntOrPtr$index :: struct
{
    i: int;
    p: *int;
}

f$index :: ()
{
    u: IntOrPtr$index = { 42, 42 as *int };
    u.i = 0;
    u.p = 0 as *int;
}

iter$index: int;

Vector$index :: struct
{
    x, y: int;
}

fact_iter$index :: ( n: int ) -> int
{
    r := 1;
    for( i: 2..n )
    {
        r *= i;
    }
    return r;
}

fact_rec$index :: ( n: int ) -> int
{
    if( n == 0 )
    {
        return 1;
    }
    else
    {
        return n * fact_rec$index( n - 1 );
    }
}

n$index :: 1 + sizeof(p$index);

p$index: *T$index;

T$index :: struct
{
    a: [n$index]int;
}
"""


if __name__ == '__main__':
    template = Template(template_str)

    for i in range(10):
        print(template.substitute({'index': i}))
