package Nazar.Views.Draw is

   type Draw_View_Interface is interface and Nazar_View_Interface;

   function Viewport
     (Draw_View : Draw_View_Interface)
      return Rectangle
      is abstract;

   procedure Set_Viewport
     (Draw_View : in out Draw_View_Interface;
      Viewport  : Rectangle)
   is abstract;

   type Nazar_Draw_View is access all Draw_View_Interface'Class;

end Nazar.Views.Draw;
