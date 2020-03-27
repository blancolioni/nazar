package body Nazar.Views.Console is

   type Command_Signal_Handler is
     new Nazar.Signals.Signal_Handler_Interface with
      record
         Callback : Nazar.Views.Console.Command_Callback;
      end record;

   overriding function Handle
     (Handler     : Command_Signal_Handler;
      Source      : Nazar.Signals.Signal_Source_Interface'Class;
      Signal_Data : Nazar.Signals.Signal_Data_Interface'Class;
      User_Data   : Nazar.Signals.User_Data_Interface'Class)
      return Boolean;

   -------------------------
   -- Emit_Command_Signal --
   -------------------------

   procedure Emit_Command_Signal
     (View    : Console_View_Interface'Class;
      Command : String)
   is
   begin
      View.Emit (View, Signal_Command,
                 Command_Signal_Data'
                   (Command =>
                      Ada.Strings.Unbounded.To_Unbounded_String (Command)));
   end Emit_Command_Signal;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Handler     : Command_Signal_Handler;
      Source      : Nazar.Signals.Signal_Source_Interface'Class;
      Signal_Data : Nazar.Signals.Signal_Data_Interface'Class;
      User_Data   : Nazar.Signals.User_Data_Interface'Class)
      return Boolean
   is
      pragma Unreferenced (Source);
      Command_Data : Nazar.Views.Console.Command_Signal_Data'Class renames
        Nazar.Views.Console.Command_Signal_Data'Class (Signal_Data);
   begin
      Handler.Callback (Command_Data.Command_Line, User_Data);
      return True;
   end Handle;

   ----------------
   -- On_Command --
   ----------------

   procedure On_Command
     (View      : in out Console_View_Interface'Class;
      Handler   : Nazar.Views.Console.Command_Callback;
      User_Data : Nazar.Signals.User_Data_Interface'Class)
   is
      Id : constant Nazar.Signals.Handler_Id :=
        View.Add_Handler
          (Signal    => Nazar.Views.Console.Signal_Command,
           Source    => View,
           User_Data => User_Data,
           Handler   => Command_Signal_Handler'(Callback => Handler));
   begin
      pragma Unreferenced (Id);
   end On_Command;

   --------------------
   -- Signal_Command --
   --------------------

   function Signal_Command return Nazar.Signals.Signal_Type is
   begin
      return Nazar.Signals.Signal ("signal-command");
   end Signal_Command;

end Nazar.Views.Console;
