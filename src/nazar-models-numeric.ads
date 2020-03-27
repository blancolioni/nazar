with Nazar.Interfaces.Numeric;

package Nazar.Models.Numeric is

   type Nazar_Float_Model_Record is
     new Nazar_Model_Record
     and Nazar.Interfaces.Numeric.Nazar_Float_Interface
   with private;

   type Nazar_Float_Model is access all Nazar_Float_Model_Record'Class;

   procedure Initialize
     (Model          : in out Nazar_Float_Model_Record'Class;
      Min, Max, Step : Nazar_Float;
      Initial_Value  : Nazar_Float);

   function Nazar_Float_Model_New
     (Min, Max, Step : Nazar_Float;
      Initial_Value  : Nazar_Float)
      return Nazar_Float_Model;

   procedure Set_Current
     (Model : in out Nazar_Float_Model_Record'Class;
      Value : Nazar_Float);

private

   type Nazar_Float_Model_Record is
     new Nazar_Model_Record
     and Nazar.Interfaces.Numeric.Nazar_Float_Interface with
      record
         Min, Max, Current : Nazar_Float := 0.0;
         Step              : Nazar_Float := 1.0;
      end record;

   overriding function Class_Name
     (Model : Nazar_Float_Model_Record)
      return String
   is ("nazar-float-model");

   overriding function Minimum
     (Model  : Nazar_Float_Model_Record)
      return Nazar_Float
   is (Model.Min);

   overriding function Maximum
     (Model  : Nazar_Float_Model_Record)
      return Nazar_Float
   is (Model.Max);

   overriding function Step
     (Model  : Nazar_Float_Model_Record)
      return Nazar_Float
   is (Model.Step);

   overriding function Current
     (Model  : Nazar_Float_Model_Record)
      return Nazar_Float
   is (Model.Current);

end Nazar.Models.Numeric;
