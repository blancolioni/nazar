with Nazar.Values;

package Nazar.Interfaces.Table is

   type Nazar_Table_Interface is interface;

   function Column_Count
     (Table : Nazar_Table_Interface)
      return Natural
      is abstract;

   function Column_Name
     (Table : Nazar_Table_Interface;
      Column_Index : Positive)
      return String
      is abstract
     with Pre'Class => Column_Index <= Table.Column_Count;

   function Column_Heading
     (Table        : Nazar_Table_Interface;
      Column_Index : Positive)
      return String
      is abstract
     with Pre'Class => Column_Index <= Table.Column_Count;

   function Column_Type
     (Table        : Nazar_Table_Interface;
      Column_Index : Positive)
      return Nazar.Values.Nazar_Value_Type
      is abstract
     with Pre'Class => Column_Index <= Table.Column_Count;

   type Table_Cursor_Interface is interface;

   function Has_Element (Position : Table_Cursor_Interface) return Boolean
                         is abstract;

   procedure Next (Position : in out Table_Cursor_Interface)
   is abstract
     with Pre'Class => Position.Has_Element;

   type Row_Cursor_Interface is interface and Table_Cursor_Interface;

   function First_Row (Table : Nazar_Table_Interface)
                       return Row_Cursor_Interface'Class
                       is abstract;

   type Cell_Cursor_Interface is interface and Table_Cursor_Interface;

   function First_Cell
     (Row : Row_Cursor_Interface)
      return Cell_Cursor_Interface'Class
      is abstract;

   function Value (Position : Cell_Cursor_Interface)
                   return Nazar.Values.Nazar_Value
                   is abstract;

end Nazar.Interfaces.Table;
