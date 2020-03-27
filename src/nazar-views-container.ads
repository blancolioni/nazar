package Nazar.Views.Container is

   type Nazar_Container_View_Interface is interface
     and Nazar_View_Interface;

   subtype Nazar_Container_View_Class is
     Nazar_Container_View_Interface'Class;

   type Nazar_Container_View is
     access all Nazar_Container_View_Interface'Class;

   procedure Append
     (View : in out Nazar_Container_View_Interface;
      Child : not null access Nazar_View_Record'Class)
   is abstract;

end Nazar.Views.Container;
