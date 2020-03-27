with Nazar.Interfaces.Text;

package Nazar.Models.Text is

   type Nazar_Text_Model_Record is
     new Nazar_Model_Record
     and Nazar.Interfaces.Text.Text_Interface
   with private;

   type Nazar_Text_Model is access all Nazar_Text_Model_Record'Class;

   function Nazar_Text_Model_New
     (Initial_Text : String)
      return Nazar_Text_Model;

   procedure Set_Text
     (Model : in out Nazar_Text_Model_Record'Class;
      Text  : String);

private

   type Nazar_Text_Model_Record is
     new Nazar_Model_Record
     and Nazar.Interfaces.Text.Text_Interface with
      record
         Text : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Class_Name
     (Model : Nazar_Text_Model_Record)
      return String
   is ("nazar-text-model");

   overriding function Get_Text
     (Model  : Nazar_Text_Model_Record)
      return String
   is (Ada.Strings.Unbounded.To_String (Model.Text));

end Nazar.Models.Text;
