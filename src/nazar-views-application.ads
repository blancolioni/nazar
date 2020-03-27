with Nazar.Views.Container;

package Nazar.Views.Application is

   type Nazar_Application_View_Interface is interface
     and Nazar.Views.Container.Nazar_Container_View_Interface;

   subtype Nazar_Application_View_Class is
     Nazar_Application_View_Interface'Class;

   type Nazar_Application_View is
     access all Nazar_Application_View_Interface'Class;

end Nazar.Views.Application;
