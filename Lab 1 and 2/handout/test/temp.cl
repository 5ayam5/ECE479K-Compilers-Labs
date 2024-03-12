class A
{
  x : Int;

  getA() : A
  {
    self
  };
};

class Test
{
  main : Main;

  run() : Bool
  {
    main.main()
  };
};

class B
{
  a : A;

  getB() : B
  {
    {
      test();
      self;
    }
  };

  getConstString() : String
  {
    "const string this is\n"
  };

  test() : Bool
  {
    true
  };
};

class C inherits B
{
  y : String;

  getC() : C
  {
    self
  };

  test() : Bool
  {
    false
  };
};

class Main inherits IO
{
  c : C <- new C;

  test(b : B) : Bool
  {
    b.test()
  };

  main() : Bool
  {
    {
      -- c@B.test();
      test(c);
      -- c = c;
      -- let b : B <- c in b.test();
    }
  };
};