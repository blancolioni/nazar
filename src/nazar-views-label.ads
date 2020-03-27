package Nazar.Views.Label is

   type Label_View_Interface is interface and Nazar_View_Interface;

   subtype Label_View_Class is Label_View_Interface'Class;

   type Nazar_Label_View is access all Label_View_Interface'Class;

end Nazar.Views.Label;
