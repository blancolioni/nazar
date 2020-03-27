package body Nazar.Models is

   type Null_Model_Record is
     new Nazar_Model_Record with null record;

   overriding function Class_Name
     (Model : Null_Model_Record)
      return String
   is ("null-model");

   ------------------
   -- Add_Observer --
   ------------------

   overriding procedure Add_Observer
     (Model    : in out Nazar_Model_Record;
      Observer : Nazar.Interfaces.Observable.Observer_Interface'Class)
   is
   begin
      Model.Observable.Add_Observer (Observer);
   end Add_Observer;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Model : in out Nazar_Model_Record)
   is
   begin
      Model.Declare_Property ("identifier", "");
   end Initialize;

   ----------------------
   -- Notify_Observers --
   ----------------------

   overriding procedure Notify_Observers
     (Model    : Nazar_Model_Record)
   is
   begin
      Model.Observable.Notify_Observers;
   end Notify_Observers;

   ----------------
   -- Null_Model --
   ----------------

   function Null_Model return Nazar_Model_Record'Class is
   begin
      return Null_Model_Record'
        (Nazar.Interfaces.Properties.Root_Property_Container with
         Id        => WL.Guids.Null_Guid,
         Model_Name => <>,
         Observable => <>);
   end Null_Model;

   ---------------------
   -- Remove_Observer --
   ---------------------

   overriding procedure Remove_Observer
     (Model    : in out Nazar_Model_Record;
      Observer : Nazar.Interfaces.Observable.Observer_Interface'Class)
   is
   begin
      Model.Observable.Remove_Observer (Observer);
   end Remove_Observer;

   --------------
   -- Set_Name --
   --------------

   overriding procedure Set_Name
     (Model : in out Nazar_Model_Record;
      Name  : String)
   is
   begin
      Model.Model_Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Name;

end Nazar.Models;
