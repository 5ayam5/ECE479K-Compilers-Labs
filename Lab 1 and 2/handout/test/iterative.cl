class Main {
    main() : Int {
        let x : Int <- 31, y : Int <- 1 in {
            while (0 < x) loop {
                y <- y * 2;
                x <- x - 1;
            } pool;
            y;
        }
    };
};