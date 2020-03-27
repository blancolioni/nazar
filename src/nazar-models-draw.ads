private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Nazar.Colors;
with Nazar.Draw_Operations;

package Nazar.Models.Draw is

   type Root_Draw_Model is
     new Nazar_Model_Record
   with private;

   type Nazar_Draw_Model is access all Root_Draw_Model'Class;

   procedure Clear
     (Model : in out Nazar_Draw_Model);

   function Background_Color
     (Model : Root_Draw_Model)
      return Nazar.Colors.Nazar_Color;

   function Bounding_Box
     (Model : Root_Draw_Model'Class)
      return Rectangle;

   procedure Set_Bounding_Box
     (Model : in out Root_Draw_Model'Class;
      Box   : Rectangle);

   procedure Iterate_Operations
     (Model : Root_Draw_Model;
      Process : not null access
        procedure (Operation : Nazar.Draw_Operations.Draw_Operation));

   procedure Move_To
     (Model   : in out Root_Draw_Model;
      World_X : Nazar_Float;
      World_Y : Nazar_Float);

   procedure Line_To
     (Model   : in out Root_Draw_Model;
      World_X : Nazar_Float;
      World_Y : Nazar_Float);

   procedure Circle
     (Model  : in out Root_Draw_Model;
      Radius : Nazar_Float);

   procedure Set_Fill
     (Model   : in out Root_Draw_Model;
      Fill    : Boolean);

   procedure Render
     (Model    : in out Root_Draw_Model;
      Preserve : Boolean := False);

   procedure Set_Color
     (Model : in out Root_Draw_Model;
      Color : Nazar.Colors.Nazar_Color);

   procedure Set_Color
     (Model   : in out Root_Draw_Model'Class;
      R, G, B : Nazar_Unit_Float;
      Alpha   : Nazar_Unit_Float := 1.0);

   procedure Save_State (Model : in out Root_Draw_Model);
   procedure Restore_State (Model : in out Root_Draw_Model);

   procedure Render
     (Model    : Root_Draw_Model;
      Context  : in out Nazar.Draw_Operations.Draw_Context;
      Renderer : in out Nazar.Draw_Operations.Root_Render_Type'Class);

private

   package Draw_Operation_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Nazar.Draw_Operations.Draw_Operation,
        Nazar.Draw_Operations."=");

   type Root_Draw_Model is
     new Nazar_Model_Record with
      record
         Bounding_Box : Rectangle;
         Ops          : Draw_Operation_Lists.List;
      end record;

   overriding function Class_Name
     (Model : Root_Draw_Model)
      return String
   is ("nazar-draw-model");

   function Bounding_Box
     (Model : Root_Draw_Model'Class)
      return Rectangle
   is (Model.Bounding_Box);

end Nazar.Models.Draw;
