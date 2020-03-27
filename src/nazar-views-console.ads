private with Ada.Strings.Unbounded;

package Nazar.Views.Console is

   type Console_View_Interface is interface and Nazar_View_Interface;

   type Nazar_Console_View is access all Console_View_Interface'Class;

   type Command_Callback is access
     procedure (Command_Line : String;
                User_Data    : Nazar.Signals.User_Data_Interface'Class);

   procedure On_Command
     (View      : in out Console_View_Interface'Class;
      Handler   : Command_Callback;
      User_Data : Nazar.Signals.User_Data_Interface'Class);

   function Signal_Command return Nazar.Signals.Signal_Type;

   procedure Emit_Command_Signal
     (View    : Console_View_Interface'Class;
      Command : String);

   type Command_Signal_Data is
     new Nazar.Signals.Signal_Data_Interface with private;

   function Command_Line
     (Signal_Data : Command_Signal_Data'Class)
      return String;

private

   type Command_Signal_Data is
     new Nazar.Signals.Signal_Data_Interface with
      record
         Command : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   function Command_Line
     (Signal_Data : Command_Signal_Data'Class)
      return String
   is (Ada.Strings.Unbounded.To_String (Signal_Data.Command));

end Nazar.Views.Console;
