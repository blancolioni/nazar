private with Ada.Containers.Doubly_Linked_Lists;

package Nazar.Models.Layout is

   type Root_Layout_Model is new Nazar_Model_Record with private;

   type Nazar_Layout_Model is access all Root_Layout_Model'Class;

   function Layout_Model_New return Nazar_Layout_Model;

   procedure Attach
     (Layout      : in out Root_Layout_Model;
      Child       : not null access Nazar_Object_Interface'Class;
      Left, Right : Natural;
      Top, Bottom : Natural);

   function Contains
     (Layout : Root_Layout_Model;
      Child  : not null access Nazar_Object_Interface'Class)
      return Boolean;

   procedure Iterate_Children
     (Layout : Root_Layout_Model;
      Process : not null access
        procedure (Child       : not null access Nazar_Object_Interface'Class;
                   Left, Right : Natural;
                   Top, Bottom : Natural));

private

   type Cell_Item is access all Nazar_Object_Interface'Class;

   type Cell_Contents is
      record
         Item        : Cell_Item;
         Left, Right : Natural;
         Top, Bottom : Natural;
      end record;

   package Cell_Content_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Cell_Contents);

   type Root_Layout_Model is new Nazar_Model_Record with
      record
         Cells : Cell_Content_Lists.List;
      end record;

   overriding function Class_Name
     (Model : Root_Layout_Model)
      return String
   is ("nazar-layout-model");

   function Contains
     (Layout : Root_Layout_Model;
      Child  : not null access Nazar_Object_Interface'Class)
      return Boolean
   is (for some Cell of Layout.Cells => Cell.Item = Cell_Item (Child));

end Nazar.Models.Layout;
