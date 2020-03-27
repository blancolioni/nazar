package Nazar.Controllers is

   type Nazar_Controller_Interface is interface;

   subtype Nazar_Controller_Class is Nazar_Controller_Interface'Class;

   type Nazar_Controller is access all Nazar_Controller_Interface'Class;

end Nazar.Controllers;
