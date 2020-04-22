private with Ada.Strings.Unbounded;
private with Nazar.Names;

with Nazar.Interfaces.Observable;
with Nazar.Interfaces.Properties;

package Nazar.Models is

   type Nazar_Model_Record is
     abstract new Nazar.Interfaces.Properties.Property_Container_Interface
     and Nazar.Interfaces.Observable.Observable_Interface
     and Nazar_Object_Interface
   with private;

   subtype Nazar_Model_Class is Nazar_Model_Record'Class;

   type Nazar_Model is access all Nazar_Model_Record'Class;

   overriding function Name
     (Model : Nazar_Model_Record)
      return String;

   overriding procedure Set_Name
     (Model : in out Nazar_Model_Record;
      Name  : String);

   procedure Initialize
     (Model : in out Nazar_Model_Record);

   overriding procedure Add_Observer
     (Model    : in out Nazar_Model_Record;
      Observer : Nazar.Interfaces.Observable.Observer_Interface'Class);

   overriding procedure Remove_Observer
     (Model    : in out Nazar_Model_Record;
      Observer : Nazar.Interfaces.Observable.Observer_Interface'Class);

   overriding procedure Notify_Observers
     (Model    : Nazar_Model_Record);

   procedure Reload
     (Model : in out Nazar_Model_Record)
   is null;

   function Null_Model return Nazar_Model_Record'Class;

private

   type Nazar_Model_Record is
     abstract new Nazar.Interfaces.Properties.Root_Property_Container
     and Nazar.Interfaces.Observable.Observable_Interface
     and Nazar_Object_Interface with
      record
         Id         : WL.Guids.Guid := WL.Guids.New_Guid;
         Model_Name : Ada.Strings.Unbounded.Unbounded_String :=
           Nazar.Names.Next_Name;
         Observable : Nazar.Interfaces.Observable.Nazar_Observable_Record;
      end record;

   overriding function Guid
     (Model : Nazar_Model_Record)
      return WL.Guids.Guid
   is (Model.Id);

   overriding function Name
     (Model : Nazar_Model_Record)
      return String
   is (Ada.Strings.Unbounded.To_String (Model.Model_Name));

end Nazar.Models;
