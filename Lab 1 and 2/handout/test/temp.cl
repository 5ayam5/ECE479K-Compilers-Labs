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
      if c.test() then self.out_string("true\n") else self.out_string("false\n") fi;
      c.test(); -- TODO: fix this
      self.out_string("true\n");
      let x : Int <- in_int() in x; -- TODO: fix this
      self.out_int(x);
      self;
      test(c); -- TODO: fix this
      if c = c then self.out_string("c = c\n") else self.out_string("c != c\n") fi;
      let x : Int <- 12 in self.out_int(x);
      if let b : B <- new C in b@B.test() then self.out_string("true\n") else self.out_string("false\n") fi;
      "c".concat("b") = "a"; -- TODO: fix this
      true;
    }
  };
};