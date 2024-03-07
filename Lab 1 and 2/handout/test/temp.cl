class Main {
  main(): Int {
      let x: Int <- 1 in
      let y: Int <- 2 in
      {
        while x < 3 loop 
          if x < 2 then
            let x : Int <- 10 in
            {
              y <- x;
            }
            else
            {
              y <- x
            }
            fi;
            x <- y;
        pool;
        x;
      }

  };
};