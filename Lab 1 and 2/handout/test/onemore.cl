class Main inherits IO
{
    str : String;
    a : Object;

    func(x : Object) : Object
    {
        {
            a <- x;
        }
    };

    main() : Object
    {
        {
            if isvoid a then out_string("a is void\n") else out_string("a is not void\n") fi;
            func(3);
            if isvoid a then out_string("a is void\n") else out_string("a is not void\n") fi;
            let x : Int <- 2, y : Object, z : Object, w : Object in
            {
                y <- x.copy();
                w <- if 3 < 2 then z <- y.copy() else x fi;
                case w of a : Int => out_int(a + x); esac;
                out_string(str.concat("Hello, World!"));
            };
        }
    };
};