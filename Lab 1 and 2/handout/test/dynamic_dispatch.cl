class A
{
    test() : Bool
    {
        true
    };
};

class B inherits A
{
    test() : Bool
    {
        false
    };
};

class Main inherits IO
{
    a : A <- new B;

    main() : Object
    {
        {
            if a.test() then out_string("true") else out_string("false") fi;
            out_string("Hello World\n");
            a.test();
        }
    };
};