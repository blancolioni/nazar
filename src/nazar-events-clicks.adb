package body Nazar.Events.Clicks is

   type Click_Signal_Handler is
     new Nazar.Signals.Signal_Handler_Interface with
      record
         Callback : Click_Callback;
      end record;

   overriding function Handle
     (Handler     : Click_Signal_Handler;
      Source      : Nazar.Signals.Signal_Source_Interface'Class;
      Signal_Data : Nazar.Signals.Signal_Data_Interface'Class;
      User_Data   : Nazar.Signals.User_Data_Interface'Class)
      return Boolean;

   -----------------------
   -- Emit_Click_Signal --
   -----------------------

   procedure Emit_Click_Signal
     (Container : Nazar.Signals.Signal_Dispatch_Interface'Class;
      Source    : Nazar.Signals.Signal_Source_Interface'Class;
      X, Y      : Nazar_Float;
      Click     : Button_Click_Type)
   is
   begin
      Container.Emit
        (Source, Signal_Click,
         Click_Event_Signal_Data'
           (X => X, Y => Y, Button => Left, Click => Click));
   end Emit_Click_Signal;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Handler     : Click_Signal_Handler;
      Source      : Nazar.Signals.Signal_Source_Interface'Class;
      Signal_Data : Nazar.Signals.Signal_Data_Interface'Class;
      User_Data   : Nazar.Signals.User_Data_Interface'Class)
      return Boolean
   is
      pragma Unreferenced (Source);
      Click_Data : Click_Event_Signal_Data'Class renames
                     Click_Event_Signal_Data'Class (Signal_Data);
   begin
      Handler.Callback (Click_Data.X, Click_Data.Y,
                        Click_Data.Button, Click_Data.Click,
                        User_Data);
      return True;
   end Handle;

   --------------
   -- On_Click --
   --------------

   procedure On_Click
     (Container : in out Nazar.Signals.Signal_Dispatch_Interface'Class;
      Source    : Nazar.Signals.Signal_Source_Interface'Class;
      Handler   : Click_Callback;
      User_Data : Nazar.Signals.User_Data_Interface'Class)
   is
      Id : constant Nazar.Signals.Handler_Id :=
             Container.Add_Handler
               (Signal    => Signal_Click,
                Source    => Source,
                User_Data => User_Data,
                Handler   => Click_Signal_Handler'(Callback => Handler));
   begin
      pragma Unreferenced (Id);
   end On_Click;

   ------------------
   -- Signal_Click --
   ------------------

   function Signal_Click return Nazar.Signals.Signal_Type is
   begin
      return Nazar.Signals.Signal ("signal-click");
   end Signal_Click;

end Nazar.Events.Clicks;
