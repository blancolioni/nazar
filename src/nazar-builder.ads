private with WL.String_Maps;

with Nazar.Views;

package Nazar.Builder is

   type Nazar_Creator_Interface is interface;

   function Create_View
     (Creator : Nazar_Creator_Interface;
      Name    : String)
      return Nazar.Views.Nazar_View
      is abstract;

   type Nazar_Builder_Record is tagged private;

   subtype Nazar_Builder_Class is Nazar_Builder_Record'Class;

   type Nazar_Builder is access all Nazar_Builder_Record'Class;

   function Nazar_Builder_New
     (Creator     : Nazar_Creator_Interface'Class;
      Config_Path : String)
      return Nazar_Builder;

   procedure Initialize
     (Builder     : in out Nazar_Builder_Record'Class;
      Creator     : Nazar_Creator_Interface'Class;
      Config_Path : String);

   procedure Add_View
     (Builder : in out Nazar_Builder_Record'Class;
      Name    : String;
      View    : not null access Nazar.Views.Nazar_View_Record'Class);

   function Get_View
     (Builder : Nazar_Builder_Record'Class;
      Name    : String)
      return Nazar.Views.Nazar_View;

   function Has_View
     (Builder : Nazar_Builder_Record'Class;
      Name    : String)
      return Boolean;

private

   package View_Maps is
     new WL.String_Maps (Nazar.Views.Nazar_View, Nazar.Views."=");

   type Nazar_Builder_Record is tagged
      record
         Map : View_Maps.Map;
      end record;

end Nazar.Builder;
