package body Nazar.Views.Button is

   type Activate_Signal_Data is
     new Nazar.Signals.Signal_Data_Interface with null record;

   type Activate_Signal_Handler is
     new Nazar.Signals.Signal_Handler_Interface with
      record
         Callback : Activate_Callback;
      end record;

   overriding function Handle
     (Handler     : Activate_Signal_Handler;
      Source      : Nazar.Signals.Signal_Source_Interface'Class;
      Signal_Data : Nazar.Signals.Signal_Data_Interface'Class;
      User_Data   : Nazar.Signals.User_Data_Interface'Class)
      return Boolean;

   --------------------------
   -- Emit_Activate_Signal --
   --------------------------

   procedure Emit_Activate_Signal
     (View     : Button_View_Interface'Class)
   is
   begin
      View.Emit (View, Signal_Activate,
                 Activate_Signal_Data'
                   (null record));
   end Emit_Activate_Signal;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Handler     : Activate_Signal_Handler;
      Source      : Nazar.Signals.Signal_Source_Interface'Class;
      Signal_Data : Nazar.Signals.Signal_Data_Interface'Class;
      User_Data   : Nazar.Signals.User_Data_Interface'Class)
      return Boolean
   is
      pragma Unreferenced (Source, Signal_Data);
   begin
      Handler.Callback (User_Data);
      return True;
   end Handle;

   ----------------
   -- On_Activate --
   ----------------

   procedure On_Activate
     (View      : in out Button_View_Interface'Class;
      Handler   : Nazar.Views.Button.Activate_Callback;
      User_Data : Nazar.Signals.User_Data_Interface'Class)
   is
      Id : constant Nazar.Signals.Handler_Id :=
        View.Add_Handler
          (Signal    => Nazar.Views.Button.Signal_Activate,
           Source    => View,
           User_Data => User_Data,
           Handler   => Activate_Signal_Handler'(Callback => Handler));
   begin
      pragma Unreferenced (Id);
   end On_Activate;

   --------------------
   -- Signal_Activate --
   --------------------

   function Signal_Activate return Nazar.Signals.Signal_Type is
   begin
      return Nazar.Signals.Signal ("signal-activate");
   end Signal_Activate;

end Nazar.Views.Button;
