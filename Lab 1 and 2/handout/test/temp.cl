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

  run() : Int
  {
    {
      main.main();
      0;
    }
  };
};

class B
{
  a : A <- new A;

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
  x : Int;

  test(b : B) : Bool
  {
    b.test()
  };

  main() : Bool
  {
    {
      if c.test() then out_string("true\n") else out_string("false\n") fi;
      c.test();
      out_string("true\n");
      let x : Int <- 12 in x;
      out_int(x);
      self;
      test(c);
      if c = c then out_string("c = c\n") else out_string("c != c\n") fi;
      let x : Int <- 023 in out_int(x);
      if let b : B <- new C in b@B.test() then out_string("true\n") else out_string("false\n") fi;
      if "c".concat("b") = "cb" then out_string("true\n") else out_string("false\n") fi;
      true;
    }
  };
};