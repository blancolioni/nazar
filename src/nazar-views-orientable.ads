package Nazar.Views.Orientable is

   type Nazar_Orientation is (Horizontal, Vertical);

   type Nazar_Orientable_View_Interface is interface
     and Nazar.Views.Nazar_View_Interface;

   subtype Nazar_Orientable_View_Class is
     Nazar_Orientable_View_Interface'Class;

   type Nazar_Orientable_View is
     access all Nazar_Orientable_View_Interface'Class;

   function Orientation
     (View : Nazar_Orientable_View_Interface)
      return Nazar_Orientation
      is abstract;

   procedure Set_Orientation
     (View        : in out Nazar_Orientable_View_Interface;
      Orientation : Nazar_Orientation)
   is abstract;

end Nazar.Views.Orientable;
