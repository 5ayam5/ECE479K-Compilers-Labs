class Main inherits IO
{
    x : Object;
    y : Object <- new Object;

    getX() : Object
    {
        x
    };

    main() : Object
    {
        {
            if (isvoid x) then out_string("x is void\n") else out_string("x is not void\n") fi;
            if (isvoid y) then out_string("y is void\n") else out_string("y is not void\n") fi;
            if (isvoid 2) then out_string("2 is void\n") else out_string("2 is not void\n") fi;
            if (isvoid false) then out_string("false is void\n") else out_string("false is not void\n") fi;
            if (isvoid new String) then out_string("new String is void\n") else out_string("new String is not void\n") fi;
            if (isvoid new Main) then out_string("new Main is void\n") else out_string("new Main is not void\n") fi;
            if (isvoid getX()) then out_string("getX() is void\n") else out_string("getX() is not void\n") fi;
        }
    };
};