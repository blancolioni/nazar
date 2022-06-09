package Nazar.Events.Clicks is

   type Mouse_Button_Type is (Left, Right);

   type Button_Click_Type is (Single, Double, Triple);

   type Button_Press_Type is (Press, Release);

   type Mouse_Event_Signal_Data is
     new Event_Signal_Data with private;

   function X (This : Mouse_Event_Signal_Data) return Nazar_Float;
   function Y (This : Mouse_Event_Signal_Data) return Nazar_Float;

   type Click_Event_Signal_Data is
     new Mouse_Event_Signal_Data with private;

   function Button (This : Click_Event_Signal_Data) return Mouse_Button_Type;

   type Click_Callback is access
     procedure (X, Y      : Nazar_Float;
                Button    : Mouse_Button_Type;
                Click     : Button_Click_Type;
                User_Data : Nazar.Signals.User_Data_Interface'Class);

   procedure On_Click
     (Container : in out Nazar.Signals.Signal_Dispatch_Interface'Class;
      Source    : Nazar.Signals.Signal_Source_Interface'Class;
      Handler   : Click_Callback;
      User_Data : Nazar.Signals.User_Data_Interface'Class);

   procedure Emit_Click_Signal
     (Container : Nazar.Signals.Signal_Dispatch_Interface'Class;
      Source    : Nazar.Signals.Signal_Source_Interface'Class;
      X, Y      : Nazar_Float;
      Click     : Button_Click_Type);

   function Signal_Click return Nazar.Signals.Signal_Type;

private

   type Mouse_Event_Signal_Data is
     new Event_Signal_Data with
      record
         X, Y : Nazar_Float;
      end record;

   function X (This : Mouse_Event_Signal_Data) return Nazar_Float
   is (This.X);

   function Y (This : Mouse_Event_Signal_Data) return Nazar_Float
   is (This.Y);

   type Click_Event_Signal_Data is
     new Mouse_Event_Signal_Data with
      record
         Button : Mouse_Button_Type;
         Click  : Button_Click_Type;
      end record;

   function Button
     (This : Click_Event_Signal_Data)
      return Mouse_Button_Type
   is (This.Button);

end Nazar.Events.Clicks;
