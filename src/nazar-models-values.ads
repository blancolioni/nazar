package Nazar.Models.Values is

   type Model_Value_Interface is interface;

   function Image
     (Value : Model_Value_Interface)
      return String
      is abstract;

   generic
      type Integral is range <>;
   function To_Integral_Value
     (X : Integral)
      return Model_Value_Interface'Class;

   generic
      type Real is digits <>;
   function To_Real_Value
     (X : Real)
      return Model_Value_Interface'Class;

   function To_Value
     (X : Integer)
      return Model_Value_Interface'Class;

   function To_Value
     (S : String)
     return Model_Value_Interface'Class;

end Nazar.Models.Values;
