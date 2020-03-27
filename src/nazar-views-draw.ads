package Nazar.Views.Draw is

   type Draw_View_Interface is interface and Nazar_View_Interface;

   type Nazar_Draw_View is access all Draw_View_Interface'Class;

end Nazar.Views.Draw;
