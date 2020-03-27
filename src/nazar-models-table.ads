with Nazar.Interfaces.Table;

package Nazar.Models.Table is

   type Nazar_Table_Model_Record is
     abstract new Nazar_Model_Record
     and Nazar.Interfaces.Table.Nazar_Table_Interface
   with private;

   type Nazar_Table_Model is access all Nazar_Table_Model_Record'Class;

   overriding function Class_Name
     (Model : Nazar_Table_Model_Record)
      return String;

private

   type Nazar_Table_Model_Record is
     abstract new Nazar_Model_Record
     and Nazar.Interfaces.Table.Nazar_Table_Interface
   with record
      null;
   end record;

   overriding function Class_Name
     (Model : Nazar_Table_Model_Record)
      return String
   is ("nazar-table-model");

end Nazar.Models.Table;
