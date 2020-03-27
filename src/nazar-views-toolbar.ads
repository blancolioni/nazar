with Nazar.Views.Container;

package Nazar.Views.Toolbar is

   type Toolbar_View_Interface is interface
     and Nazar.Views.Container.Container_View_Interface;

   subtype Toolbar_View_Class is Toolbar_View_Interface'Class;

   type Nazar_Toolbar_View is access all Toolbar_View_Interface'Class;

end Nazar.Views.Toolbar;
