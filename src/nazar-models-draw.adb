with Nazar.Trigonometry;

with Nazar.Logging;

package body Nazar.Models.Draw is

   ----------------------
   -- Background_Color --
   ----------------------

   function Background_Color
     (Model : Root_Draw_Model)
      return Nazar.Colors.Nazar_Color
   is
      pragma Unreferenced (Model);
   begin
      return (0.0, 0.0, 0.0, 1.0);
   end Background_Color;

   ------------
   -- Circle --
   ------------

   procedure Circle (Model : in out Root_Draw_Model; Radius : Nazar_Float) is
   begin
      Model.Ops.Append
        (Nazar.Draw_Operations.Arc
           (Radius      => Radius,
            Start_Angle => Nazar.Trigonometry.From_Degrees (0.0),
            End_Angle   => Nazar.Trigonometry.From_Degrees (360.0)));
   end Circle;

   -----------
   -- Clear --
   -----------

   procedure Clear (Model : in out Root_Draw_Model) is
   begin
      Model.Ops.Clear;
   end Clear;

   ---------------
   -- Draw_Text --
   ---------------

   procedure Draw_Text
     (Model : in out Root_Draw_Model;
      Text  : String)
   is
   begin
      Model.Ops.Append
        (Nazar.Draw_Operations.Text (Text));
   end Draw_Text;

   -----------
   -- Image --
   -----------

   procedure Image
     (Model         : in out Root_Draw_Model;
      Resource_Name : String;
      Width, Height : Nazar_Float)
   is
   begin
      Model.Ops.Append
        (Nazar.Draw_Operations.Image
           (Resource_Name => Resource_Name,
            Width         => Width,
            Height        => Height,
            Rotation      => Nazar.Trigonometry.From_Degrees (0.0)));
   end Image;

   ------------------------
   -- Iterate_Operations --
   ------------------------

   procedure Iterate_Operations
     (Model   : Root_Draw_Model;
      Process : not null access procedure
        (Operation : Nazar.Draw_Operations.Draw_Operation))
   is
   begin
      for Op of Model.Ops loop
         Process (Op);
      end loop;
   end Iterate_Operations;

   -------------
   -- Line_To --
   -------------

   procedure Line_To
     (Model   : in out Root_Draw_Model;
      World_X : Nazar_Float;
      World_Y : Nazar_Float)
   is
   begin
      Model.Ops.Append
        (Nazar.Draw_Operations.Move
           (Nazar.Draw_Operations.World_Position (World_X, World_Y),
            Paint => True));
   end Line_To;

   -------------
   -- Move_To --
   -------------

   procedure Move_To
     (Model   : in out Root_Draw_Model;
      World_X : Nazar_Float;
      World_Y : Nazar_Float)
   is
   begin
      Model.Ops.Append
        (Nazar.Draw_Operations.Move
           (Nazar.Draw_Operations.World_Position (World_X, World_Y),
            Paint => False));
   end Move_To;

   ------------------
   -- Queue_Render --
   ------------------

   procedure Queue_Render (Model : in out Root_Draw_Model) is
   begin
      Model.Render_Queued := True;
   end Queue_Render;

   ------------
   -- Render --
   ------------

   procedure Render
     (Model    : Root_Draw_Model;
      Context  : in out Nazar.Draw_Operations.Draw_Context;
      Renderer : in out Nazar.Draw_Operations.Root_Render_Type'Class)
   is
      procedure Do_Render;

      ---------------
      -- Do_Render --
      ---------------

      procedure Do_Render is
      begin
         Nazar.Logging.Log (Model,
                            "rendering" & Model.Ops.Length'Image
                            & " draw primitives");

         Renderer.Start_Draw (Context);
         for Op of Model.Ops loop
            Renderer.Draw (Op);
         end loop;
         Renderer.End_Draw;

         Nazar.Logging.Log (Model, "done");
      end Do_Render;

   begin
      Do_Render;
   end Render;

   ------------
   -- Render --
   ------------

   procedure Render
     (Model    : in out Root_Draw_Model;
      Preserve : Boolean := False)
   is
   begin
      Model.Ops.Append
        (Nazar.Draw_Operations.Render (Preserve));
   end Render;

   -------------------
   -- Restore_State --
   -------------------

   procedure Restore_State (Model : in out Root_Draw_Model) is
   begin
      Model.Ops.Append (Nazar.Draw_Operations.Restore_State);
   end Restore_State;

   ----------------
   -- Save_State --
   ----------------

   procedure Save_State (Model : in out Root_Draw_Model) is
   begin
      Model.Ops.Append (Nazar.Draw_Operations.Save_State);
   end Save_State;

   ----------------------
   -- Set_Bounding_Box --
   ----------------------

   procedure Set_Bounding_Box
     (Model : in out Root_Draw_Model'Class;
      Box   : Rectangle)
   is
   begin
      Model.Bounding_Box := Box;
   end Set_Bounding_Box;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
     (Model : in out Root_Draw_Model;
      Color : Nazar.Colors.Nazar_Color)
   is
   begin
      Model.Ops.Append
        (Nazar.Draw_Operations.Set_Property
           (Nazar.Draw_Operations.Color_Property (Color)));
   end Set_Color;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
     (Model   : in out Root_Draw_Model'Class;
      R, G, B : Nazar_Unit_Float;
      Alpha   : Nazar_Unit_Float := 1.0)
   is
   begin
      Model.Set_Color ((R, G, B, Alpha));
   end Set_Color;

   --------------
   -- Set_Fill --
   --------------

   procedure Set_Fill
     (Model   : in out Root_Draw_Model;
      Fill    : Boolean)
   is
   begin
      Model.Ops.Append
        (Nazar.Draw_Operations.Set_Property
           (Nazar.Draw_Operations.Fill_Property (Fill)));
   end Set_Fill;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font
     (Model  : in out Root_Draw_Model;
      Family : String;
      Size   : Nazar_Float;
      Italic : Boolean;
      Bold   : Boolean)
   is
   begin
      Model.Ops.Append
        (Nazar.Draw_Operations.Set_Property
           (Nazar.Draw_Operations.Font_Property
                (Family, Size, Italic, Bold)));
   end Set_Font;

end Nazar.Models.Draw;
