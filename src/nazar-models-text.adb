package body Nazar.Models.Text is

   --------------------------
   -- Nazar_Text_Model_New --
   --------------------------

   function Nazar_Text_Model_New
     (Initial_Text : String)
      return Nazar_Text_Model
   is
   begin
      return new Nazar_Text_Model_Record'
        (Nazar_Model_Record with
           Text => Ada.Strings.Unbounded.To_Unbounded_String (Initial_Text));
   end Nazar_Text_Model_New;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Model : in out Nazar_Text_Model_Record'Class;
      Text  : String)
   is
   begin
      Model.Text :=
        Ada.Strings.Unbounded.To_Unbounded_String (Text);
      Model.Notify_Observers;
   end Set_Text;

end Nazar.Models.Text;
