package Nazar.Views.Table is

   type Nazar_Table_View_Interface is interface and Nazar_View_Interface;

   type Nazar_Table_View is access all Nazar_Table_View_Interface'Class;

end Nazar.Views.Table;
