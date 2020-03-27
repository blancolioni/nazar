with Nazar.Interfaces.Table;
with Nazar.Models.Table;
with Nazar.Values;

package Nazar.Models.Array_Table is

   type Nazar_Array_Table_Model_Record is
     abstract new Nazar.Models.Table.Nazar_Table_Model_Record
   with private;

   subtype Nazar_Array_Table_Model_Class is
     Nazar_Array_Table_Model_Record'Class;

   function Row_Count
     (Container : Nazar_Array_Table_Model_Record)
      return Natural
      is abstract;

   function Element
     (Container   : Nazar_Array_Table_Model_Record;
      Row, Column : Positive)
      return Nazar.Values.Nazar_Value
      is abstract
     with Pre'Class => Row <= Container.Row_Count
     and then Column <= Container.Column_Count;

   overriding function First_Row
     (Container : Nazar_Array_Table_Model_Record)
      return Nazar.Interfaces.Table.Row_Cursor_Interface'Class;

private

   type Nazar_Array_Table_Model_Record is
     abstract new Nazar.Models.Table.Nazar_Table_Model_Record
   with record
      null;
   end record;

end Nazar.Models.Array_Table;
