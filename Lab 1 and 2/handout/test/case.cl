class Main inherits IO
{
    main() : Object
    {
        let x : Int <- 100 in case x of x : String => x; x : Bool => x; esac
    };
};