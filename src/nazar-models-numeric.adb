package body Nazar.Models.Numeric is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Model          : in out Nazar_Float_Model_Record'Class;
      Min, Max, Step : Nazar_Float;
      Initial_Value  : Nazar_Float)
   is
   begin
      Model.Min := Min;
      Model.Max := Max;
      Model.Step := Step;
      Model.Current := Initial_Value;
   end Initialize;

   ---------------------------
   -- Nazar_Float_Model_New --
   ---------------------------

   function Nazar_Float_Model_New
     (Min, Max, Step : Nazar_Float;
      Initial_Value  : Nazar_Float)
      return Nazar_Float_Model
   is
   begin
      return new Nazar_Float_Model_Record'
        (Nazar_Model_Record with
         Min        => Min,
         Max        => Max,
         Step       => Step,
         Current    => Initial_Value);
   end Nazar_Float_Model_New;

   -----------------
   -- Set_Current --
   -----------------

   procedure Set_Current
     (Model : in out Nazar_Float_Model_Record'Class;
      Value : Nazar_Float)
   is
   begin
      Model.Current := Value;
      Model.Notify_Observers;
   end Set_Current;

end Nazar.Models.Numeric;
