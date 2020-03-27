package body Nazar.Models.Array_Table is

   type Row_Cursor is new Nazar.Interfaces.Table.Row_Cursor_Interface with
      record
         Table     : access constant Nazar_Array_Table_Model_Class;
         Row_Index : Natural := 0;
      end record;

   overriding function Has_Element
     (Position : Row_Cursor)
      return Boolean
   is (Position.Row_Index > 0);

   overriding procedure Next
     (Position : in out Row_Cursor);

   type Cell_Cursor is new Nazar.Interfaces.Table.Cell_Cursor_Interface with
      record
         Table      : access constant Nazar_Array_Table_Model_Class;
         Row_Index  : Natural := 0;
         Cell_Index : Natural := 0;
      end record;

   overriding function Has_Element
     (Position : Cell_Cursor)
      return Boolean
   is (Position.Row_Index > 0 and then Position.Cell_Index > 0);

   overriding procedure Next
     (Position : in out Cell_Cursor);

   overriding function Value
     (Position : Cell_Cursor)
      return Nazar.Values.Nazar_Value
   is (Position.Table.Element (Position.Row_Index, Position.Cell_Index));

   overriding function First_Cell
     (Row : Row_Cursor)
      return Nazar.Interfaces.Table.Cell_Cursor_Interface'Class
   is (Cell_Cursor'(Table      => Row.Table,
                    Row_Index  => Row.Row_Index,
                    Cell_Index => Natural'Min (1, Row.Table.Column_Count)));

   ---------------
   -- First_Row --
   ---------------

   overriding function First_Row
     (Container : Nazar_Array_Table_Model_Record)
      return Nazar.Interfaces.Table.Row_Cursor_Interface'Class
   is
      Class : Nazar_Array_Table_Model_Class renames
                Nazar_Array_Table_Model_Class (Container);
      Cursor : Row_Cursor := (Container'Unchecked_Access, 0);
   begin
      if Class.Row_Count > 0 then
         Cursor.Row_Index := 1;
      end if;
      return Cursor;
   end First_Row;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Position : in out Row_Cursor)
   is
   begin
      Position.Row_Index := Position.Row_Index + 1;
      if Position.Row_Index > Position.Table.Row_Count then
         Position.Row_Index := 0;
      end if;
   end Next;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Position : in out Cell_Cursor)
   is
   begin
      Position.Cell_Index := Position.Cell_Index + 1;
      if Position.Cell_Index > Position.Table.Column_Count then
         Position.Cell_Index := 0;
      end if;
   end Next;

end Nazar.Models.Array_Table;
