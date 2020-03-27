private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Indefinite_Vectors;

with Nazar.Interfaces.Hierarchy;
with Nazar.Interfaces.Text;

package Nazar.Models.Scope is

   type Root_Scope_Model is
     new Nazar_Model_Record
     and Nazar.Interfaces.Text.Text_Interface
   with private;

   type Nazar_Scope_Model is access all Root_Scope_Model'Class;

   function Scope_Model
     (Root_Node     : Nazar.Interfaces.Hierarchy.Node_Reference_Class;
      Default_Scope : String)
      return Nazar_Scope_Model;

   function Current_Scope
     (Model : Root_Scope_Model)
      return String;

   function Current_Node
     (Model : Root_Scope_Model)
      return Nazar.Interfaces.Hierarchy.Node_Reference_Class;

   function Get_Node
     (Model : Root_Scope_Model;
      Path  : String)
      return Nazar.Interfaces.Hierarchy.Node_Reference_Interface'Class;

   function Change_Scope
     (Model : in out Root_Scope_Model;
      Path  : String)
      return Boolean;

private

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   package Node_Reference_Holders is
     new Ada.Containers.Indefinite_Holders
       (Nazar.Interfaces.Hierarchy.Node_Reference_Class,
        Nazar.Interfaces.Hierarchy."=");

   type Root_Scope_Model is
     new Nazar_Model_Record
     and Nazar.Interfaces.Text.Text_Interface with
      record
         Is_Valid      : Boolean := False;
         Root          : Node_Reference_Holders.Holder;
         Default_Path  : String_Vectors.Vector;
         Active_Path   : String_Vectors.Vector;
      end record;

   overriding function Class_Name
     (Model : Root_Scope_Model)
      return String
   is ("nazar-scope-model");

   overriding function Get_Text
     (Model  : Root_Scope_Model)
      return String
   is (Root_Scope_Model'Class (Model).Current_Scope);

   procedure Set_Parent_Scope
     (Model : in out Root_Scope_Model);

   procedure Set_Child_Scope
     (Model      : in out Root_Scope_Model;
      Child_Name : String)
     with Pre => Current_Node (Model).Get.Has_Child (Child_Name);

   function Is_Valid
     (Model : Root_Scope_Model)
      return Boolean
   is (Model.Is_Valid);

end Nazar.Models.Scope;
