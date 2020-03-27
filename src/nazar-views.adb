with Nazar.Interfaces.Observable;

with Nazar.Logging;

package body Nazar.Views is

   type View_Observer is
     new Nazar.Interfaces.Observable.Observer_Interface with
      record
         View : Nazar_View;
      end record;

   overriding procedure Notify
     (Observer : View_Observer);

   -----------------
   -- Add_Handler --
   -----------------

   overriding function Add_Handler
     (View        : in out Nazar_View_Record;
      Signal      : Nazar.Signals.Signal_Type;
      Source      : Nazar.Signals.Signal_Source_Interface'Class;
      User_Data   : Nazar.Signals.User_Data_Interface'Class;
      Handler     : Nazar.Signals.Signal_Handler_Interface'Class)
      return Nazar.Signals.Handler_Id
   is
   begin
      return View.Dispatch.Add_Handler
        (Signal, Source, User_Data, Handler);
   end Add_Handler;

   ------------------------
   -- Declare_Properties --
   ------------------------

   overriding procedure Declare_Properties
     (View : in out Nazar_View_Record)
   is
   begin
      View.Declare_Property ("name", "");
      View.Declare_Property ("attach-left", 0);
      View.Declare_Property ("attach-top", 0);
      View.Declare_Property ("attach-right", 1);
      View.Declare_Property ("attach-bottom", 1);
      View.Declare_Property ("expand", True);
      View.Declare_Property ("pack-end", False);
   end Declare_Properties;

   ----------
   -- Emit --
   ----------

   overriding procedure Emit
     (View        : Nazar_View_Record;
      Source      : Nazar.Signals.Signal_Source_Interface'Class;
      Signal      : Nazar.Signals.Signal_Type;
      Signal_Data : Nazar.Signals.Signal_Data_Interface'Class)
   is
   begin
      View.Dispatch.Emit (Source, Signal, Signal_Data);
   end Emit;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View : in out Nazar_View_Record)
   is
   begin
      Nazar_View_Record'Class (View).Declare_Properties;
   end Initialize;

   ------------
   -- Notify --
   ------------

   overriding procedure Notify
     (Observer : View_Observer)
   is
   begin
      Observer.View.Model_Changed;
   end Notify;

   ------------------
   -- On_Configure --
   ------------------

   procedure On_Configure
     (View    : not null access Nazar_View_Record;
      Handler : Configure_Callback)
   is null;

   ---------------
   -- Set_Model --
   ---------------

   overriding procedure Set_Model
     (View  : not null access Nazar_View_Record;
      Model : not null access Nazar.Models.Nazar_Model_Record'Class)
   is
      use type Nazar.Models.Nazar_Model;
      Observer : constant View_Observer :=
        View_Observer'(View => Nazar_View (View));
   begin
      Nazar.Logging.Log
        (View.all, "set model to " & Model.Class_Name & " " & Model.Name);

      if View.Base_Model /= null then
         View.Base_Model.Remove_Observer (Observer);
         View.Base_Model := null;
      end if;

      View.Base_Model := Nazar.Models.Nazar_Model (Model);
      View.Base_Model.Add_Observer (Observer);

      Nazar_View_Record'Class (View.all).Model_Changed;
   end Set_Model;

   --------------
   -- Set_Name --
   --------------

   overriding procedure Set_Name
     (View  : in out Nazar_View_Record;
      Name  : String)
   is
   begin
      View.View_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Name;

end Nazar.Views;
