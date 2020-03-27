private with WL.String_Maps;

with Nazar.Interfaces.Text;

package Nazar.Models.Environment is

   type Root_Environment_Model is
     new Nazar.Interfaces.Text.Text_Environment_Interface with private;

   type Nazar_Environment_Model is access all Root_Environment_Model'Class;

   function Get_Environment_Value
     (Model : Root_Environment_Model;
      Name  : String)
      return String;

   procedure Set_Environment_Value
     (Model : in out Root_Environment_Model;
      Name  : String;
      Value : String);

   procedure Iterate
     (Model : Root_Environment_Model;
      Process : not null access
        procedure (Name, Value : String));

private

   package String_Maps is
     new WL.String_Maps (String);

   type Root_Environment_Model is
     new Nazar.Interfaces.Text.Text_Environment_Interface with
      record
         Map : String_Maps.Map;
      end record;

   overriding function Get_Value
     (From : Root_Environment_Model;
      Name : String)
      return String;

   overriding procedure Set_Value
     (To    : in out Root_Environment_Model;
      Name  : String;
      Value : String);

end Nazar.Models.Environment;
