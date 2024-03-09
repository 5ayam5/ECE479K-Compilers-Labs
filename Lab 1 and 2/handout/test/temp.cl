class A
{
  x : Int;

  getA() : A
  {
    self
  };
};

class B
{
  a : A;

  getB() : B
  {
    self
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
  main() : Int
  {
    0
  };
};