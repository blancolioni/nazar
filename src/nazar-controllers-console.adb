package body Nazar.Controllers.Console is

   procedure Handle_Command
     (Command_Line : String;
      User_Data    : Nazar.Signals.User_Data_Interface'Class);

   --------------------
   -- Handle_Command --
   --------------------

   procedure Handle_Command
     (Command_Line : String;
      User_Data    : Nazar.Signals.User_Data_Interface'Class)
   is
      Controller : Nazar_Console_Controller_Record'Class renames
        Nazar_Console_Controller_Record'Class (User_Data);
   begin
      Controller.Model.Execute_Command_Line (Command_Line);
   end Handle_Command;

   -------------------
   -- Start_Console --
   -------------------

   procedure Start_Console
     (Controller : in out Nazar_Console_Controller_Record;
      Model      : not null access Controller_Model;
      View       : not null access Controller_View)
   is
   begin
      Controller.Model := Model_Access (Model);
      Controller.View  := View_Access (View);
      View.Set_Model (Model);
      View.On_Command (Handle_Command'Access, Controller);
      View.Show;
   end Start_Console;

end Nazar.Controllers.Console;
