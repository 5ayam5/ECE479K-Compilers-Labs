class A
{
    a : Int <- 0;
    b : Int <- func();
    c : Int;

    func() : Int
    {
        {
            c <- 5;
            3;
        }
    };

    getc() : Int
    {
        c
    };
};

class Main inherits IO
{
    a : A <- new A;

    main() : Object
    {
        out_int(a.getc())
    };
};