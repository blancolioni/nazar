with Nazar.Logging;

package body Nazar.Models.Layout is

   ------------
   -- Attach --
   ------------

   procedure Attach
     (Layout      : in out Root_Layout_Model;
      Child       :        not null access Nazar_Object_Interface'Class;
      Left, Right :        Natural; Top, Bottom : Natural)
   is
   begin
      Nazar.Logging.Log
        (Layout,
         "attach " & Child.Class_Name & " " & Child.Name
         & " at" & Left'Image & Right'Image
         & Top'Image & Bottom'Image);
      Layout.Cells.Append
        (Cell_Contents'
           (Item   => Cell_Item (Child),
            Left   => Left,
            Right  => Right,
            Top    => Top,
            Bottom => Bottom));
      Layout.Notify_Observers;
   end Attach;

   ----------------------
   -- Iterate_Children --
   ----------------------

   procedure Iterate_Children
     (Layout  : Root_Layout_Model;
      Process : not null access procedure
        (Child       : not null access Nazar_Object_Interface'Class;
         Left, Right : Natural; Top, Bottom : Natural))
   is
   begin
      for Cell of Layout.Cells loop
         Process (Cell.Item, Cell.Left, Cell.Right, Cell.Top, Cell.Bottom);
      end loop;
   end Iterate_Children;

   ----------------------
   -- Layout_Model_New --
   ----------------------

   function Layout_Model_New return Nazar_Layout_Model is
   begin
      return Model : constant Nazar_Layout_Model :=
        new Root_Layout_Model
      do
         Model.Initialize;
      end return;
   end Layout_Model_New;

end Nazar.Models.Layout;
