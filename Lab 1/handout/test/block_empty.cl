class Main {
    main() : Int {
        let x : Int <- 10, y : Int in {
            -- while (0 < x) loop {
            --     x <- x - 1;
            --     y <- y + 1;
            -- } pool;
            -- y;
            if (x < 0) then y <- (x * x + x * 10 - x * 4) else y <- (x - 1 / x + 4 * x) fi;
        }
    };
};