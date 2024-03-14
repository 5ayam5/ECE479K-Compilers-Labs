class Main inherits IO
{
    get() : Object
    {
        let x : Int <- 100 in case x of x : Object => x; x : Int => x; esac
    };

    main() : Object
    {
        out_int(case get() of x : Int => x; esac)
    };
};