private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;

with Nazar.Measurements;
with Nazar.Views.Container;

package Nazar.Views.Layout is

   type Layout_View_Interface is interface
     and Nazar.Views.Container.Nazar_Container_View_Interface;

   type Nazar_Layout_View is access all Layout_View_Interface'Class;

   type Layout_Container is private;

   function Container
     (Layout : Layout_View_Interface)
      return Layout_Container
      is abstract;

   procedure Update_Container
     (Layout : in out Layout_View_Interface;
      Update : not null access
        procedure (Container : in out Layout_Container))
   is abstract;

   function Column_Count
     (Layout : Layout_View_Interface'Class)
      return Natural;

   function Row_Count
     (Layout : Layout_View_Interface'Class)
      return Natural;

   procedure Set_Column_Width
     (Layout       : in out Layout_View_Interface'Class;
      Column_Index : Positive;
      Width        : Nazar.Measurements.Nazar_Measurement);

   procedure Set_Row_Height
     (Layout    : in out Layout_View_Interface'Class;
      Row_Index : Positive;
      Height    : Nazar.Measurements.Nazar_Measurement);

   function Contains
     (Layout : Layout_View_Interface'Class;
      Item   : not null access constant Nazar_View_Record'Class)
      return Boolean;

   procedure Delete
     (Layout : in out Layout_View_Interface'Class;
      Item   : not null access Nazar_View_Record'Class)
     with Pre => Layout.Contains (Item);

   procedure Insert
     (Layout : in out Layout_View_Interface'Class;
      Item   : not null access Nazar_View_Record'Class)
     with Pre => not Layout.Contains (Item);

   procedure Iterate
     (Layout : Layout_View_Interface'Class;
      Process : not null access
        procedure (Item : Nazar_View));

private

   package Layout_Measurement_Vectors is
     new Ada.Containers.Vectors
       (Positive,
        Nazar.Measurements.Nazar_Measurement,
        Nazar.Measurements."=");

   type Cell_Contents is
      record
         View        : Nazar_View;
      end record;

   package Cell_Content_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Cell_Contents);

   type Layout_Container is
      record
         Column_Sizes : Layout_Measurement_Vectors.Vector;
         Row_Sizes    : Layout_Measurement_Vectors.Vector;
         Contents     : Cell_Content_Lists.List;
      end record;

   function Column_Count
     (Layout : Layout_View_Interface'Class)
      return Natural
   is (Layout.Container.Column_Sizes.Last_Index);

   function Row_Count
     (Layout : Layout_View_Interface'Class)
      return Natural
   is (Layout.Container.Row_Sizes.Last_Index);

end Nazar.Views.Layout;
