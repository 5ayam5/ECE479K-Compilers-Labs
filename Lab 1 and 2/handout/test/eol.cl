class Main inherits IO
{
    a : Object;
    b : Object;
    c : Object <- new Object;
    d : Object <- c;

    print(bool : Bool) : Object
    {
        if (bool) then {
            out_string("true\n");
        } else {
            out_string("false\n");
        } fi
    };

    main() : Object
    {
        {
            print(case 1 of x : Object => case 1 of y : Object => x = y; esac; esac);
            print(a = b);
            print(c = d);
            print(a = c);
        }
    };
};