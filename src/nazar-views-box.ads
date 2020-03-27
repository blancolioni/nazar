with Nazar.Views.Container;
with Nazar.Views.Orientable;

package Nazar.Views.Box is

   type Box_View_Interface is interface
     and Nazar.Views.Container.Nazar_Container_View_Interface
     and Nazar.Views.Orientable.Nazar_Orientable_View_Interface;

   subtype Box_View_Class is Box_View_Interface'Class;

   type Nazar_Box_View is access all Box_View_Interface'Class;

end Nazar.Views.Box;
