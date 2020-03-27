package Nazar.Views.Buttons is

   type Button_View_Interface is interface;

   type Nazar_Button_View is access all Button_View_Interface'Class;

   type Activate_Callback is access
     procedure (User_Data : Nazar.Signals.User_Data_Interface'Class);

   procedure On_Activate
     (View      : in out Button_View_Interface'Class;
      Handler   : Activate_Callback;
      User_Data : Nazar.Signals.User_Data_Interface'Class);

   function Signal_Activate return Nazar.Signals.Signal_Type;

   procedure Emit_Activate_Signal
     (View : Button_View_Interface'Class);

end Nazar.Views.Buttons;
