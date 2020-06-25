private with Ada.Strings.Unbounded;
private with Nazar.Names;

with WL.Guids;

with Nazar.Interfaces.Observable;
with Nazar.Interfaces.Properties;

with Nazar.Models;
with Nazar.Signals;

package Nazar.Views is

   type Nazar_View_Interface is interface
     and Nazar_Object_Interface
     and Nazar.Signals.Signal_Source_Interface
     and Nazar.Signals.Signal_Dispatch_Interface
     and Nazar.Interfaces.Properties.Property_Container_Interface;

   procedure Show (View : in out Nazar_View_Interface) is abstract;

   procedure Model_Changed (View : in out Nazar_View_Interface) is abstract;

   function Model
     (View : Nazar_View_Interface)
      return Nazar.Models.Nazar_Model
   is abstract;

   procedure Set_Model
     (View  : not null access Nazar_View_Interface;
      Model : not null access Nazar.Models.Nazar_Model_Record'Class)
   is abstract;

   type Nazar_View_Record is
     abstract new Nazar_View_Interface
   with private;

   subtype Nazar_View_Class is Nazar_View_Record'Class;

   type Nazar_View is access all Nazar_View_Record'Class;

   procedure Initialize
     (View : in out Nazar_View_Record);

   overriding function Guid
     (View : Nazar_View_Record)
      return WL.Guids.Guid;

   overriding function Name
     (View : Nazar_View_Record)
      return String;

   overriding procedure Set_Name
     (View  : in out Nazar_View_Record;
      Name  : String);

   overriding procedure Declare_Properties
     (View : in out Nazar_View_Record);

   overriding function Model
     (View : Nazar_View_Record)
      return Nazar.Models.Nazar_Model;

   overriding function Add_Handler
     (View        : in out Nazar_View_Record;
      Signal      : Nazar.Signals.Signal_Type;
      Source      : Nazar.Signals.Signal_Source_Interface'Class;
      User_Data   : Nazar.Signals.User_Data_Interface'Class;
      Handler     : Nazar.Signals.Signal_Handler_Interface'Class)
      return Nazar.Signals.Handler_Id;

   overriding procedure Emit
     (View        : Nazar_View_Record;
      Source      : Nazar.Signals.Signal_Source_Interface'Class;
      Signal      : Nazar.Signals.Signal_Type;
      Signal_Data : Nazar.Signals.Signal_Data_Interface'Class);

   overriding procedure Set_Model
     (View  : not null access Nazar_View_Record;
      Model : not null access Nazar.Models.Nazar_Model_Record'Class);

   procedure Observe
     (View : not null access Nazar_View_Record;
      Item : not null access
        Nazar.Interfaces.Observable.Observable_Interface'Class);

   type Configure_Callback is access
     function (View : not null access Nazar_View_Record'Class;
               Width, Height : Nazar_Float)
               return Boolean;

   procedure On_Configure
     (View : not null access Nazar_View_Record;
      Handler : Configure_Callback);

private

   type Nazar_View_Record is
     abstract new Nazar.Interfaces.Properties.Root_Property_Container
     and Nazar_View_Interface with
      record
         Id         : WL.Guids.Guid := WL.Guids.New_Guid;
         View_Name  : Ada.Strings.Unbounded.Unbounded_String :=
                        Nazar.Names.Next_Name;
         Dispatch   : Nazar.Signals.Signal_Handler_Container;
         Base_Model : Nazar.Models.Nazar_Model;
      end record;

   overriding function Name
     (View : Nazar_View_Record)
      return String
   is (Ada.Strings.Unbounded.To_String (View.View_Name));

   overriding function Guid
     (View : Nazar_View_Record)
      return WL.Guids.Guid
   is (View.Id);

   overriding function Model
     (View : Nazar_View_Record)
      return Nazar.Models.Nazar_Model
   is (View.Base_Model);

end Nazar.Views;
