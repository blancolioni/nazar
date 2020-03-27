package Nazar.Interfaces.Text is

   type Text_Interface is interface;

   function Get_Text
     (From : Text_Interface) return String
      is abstract;

   type Text_Environment_Interface is interface;

   function Get_Value
     (From : Text_Environment_Interface;
      Name : String)
      return String
     is abstract;

   procedure Set_Value
     (To    : in out Text_Environment_Interface;
      Name  : String;
      Value : String)
   is abstract;

   function Expand_Environment
     (Environment : Text_Environment_Interface'Class;
      Source_Text : String)
      return String;

end Nazar.Interfaces.Text;
