package Nazar.Views.Scale is

   type Nazar_Scale_View_Interface is interface and Nazar_View_Interface;

   subtype Nazar_Scale_View_Class is Nazar_Scale_View_Interface'Class;

   type Nazar_Scale_View is access all Nazar_Scale_View_Interface'Class;

end Nazar.Views.Scale;
