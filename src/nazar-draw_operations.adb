with Ada.Numerics;

package body Nazar.Draw_Operations is

   ---------
   -- Arc --
   ---------

   function Arc
     (Radius      : Nazar_Float;
      Start_Angle : Nazar.Trigonometry.Angle;
      End_Angle   : Nazar.Trigonometry.Angle)
      return Draw_Operation
   is
   begin
      return (Arc, Radius, Start_Angle, End_Angle);
   end Arc;

   -----------------
   -- Check_State --
   -----------------

   procedure Check_State
     (Render : in out Root_Render_Type'Class)
   is
   begin
      if Render.Current.Changed (Color_Property) then
         Render.Set_Color (Render.Current.Current_Color);
         Render.Current.Changed (Color_Property) := False;
      end if;
   end Check_State;

   --------------------
   -- Color_Property --
   --------------------

   function Color_Property
     (Color : Nazar.Colors.Nazar_Color) return Draw_Property
   is
   begin
      return Draw_Property'
        (Primitive        => Color_Property,
         Color_Value      => Color);
   end Color_Property;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Render    : in out Root_Render_Type'Class;
      Operation : Draw_Operation)
   is
      Context : Draw_Context renames Render.Current;
   begin
      case Operation.Primitive is
         when Move =>
            declare
               X, Y : Nazar_Float;
            begin
               Get_Screen_Position
                 (Context, Operation.Destination, X, Y);
               if Operation.Paint then
                  Render.Check_State;
                  Render.Line_To (X, Y);
               else
                  Render.Move_To (X, Y);
               end if;
            end;

         when Arc =>
            Render.Check_State;
            declare
               Pi : constant := Ada.Numerics.Pi;
               Start : constant Nazar_Float :=
                 Trigonometry.To_Radians (Operation.Start_Angle);
               Finish : constant Nazar_Float :=
                 Trigonometry.To_Radians (Operation.End_Angle);
            begin
               Render.Arc
                 (Radius        => Operation.Radius,
                  Start_Radians => Start,
                  End_Radians   => (if Start = Finish
                                    then Finish + 2.0 * Pi
                                    else Finish));
            end;

         when Text =>
            if Context.Changed (Font_Property) then
               Render.Set_Font
                 (Family =>
                    Ada.Strings.Unbounded.To_String
                      (Context.Current_Font.Font_Family),
                  Size   => Context.Current_Font.Font_Size,
                  Italic => Context.Current_Font.Font_Italic,
                  Bold   => Context.Current_Font.Font_Bold);
               Context.Changed (Font_Property) := False;
            end if;

            if Context.Changed (Color_Property) then
               Render.Set_Color (Context.Current_Color);
               Context.Changed (Color_Property) := False;
            end if;

            Render.Text
              (Ada.Strings.Unbounded.To_String (Operation.Draw_Text));

         when Image =>
            Render.Image
              (Ada.Strings.Unbounded.To_String (Operation.Image_Resource),
               Operation.Image_Width, Operation.Image_Height,
               Operation.Image_Rotation);

         when Flush =>
            Render.Render_Current
              (Fill     => Context.Current_Fill,
               Preserve => Operation.Preserve);

         when State =>
            if Operation.Save then
               Render.Save_State;
            else
               Render.Restore_State;
            end if;

         when Property =>
            case Operation.Setting.Primitive is
               when No_Property =>
                  null;
               when Color_Property =>
                  declare
                     use type Nazar.Colors.Nazar_Color;
                  begin
                     if Context.Current_Color
                       /= Operation.Setting.Color_Value
                     then
                        Context.Current_Color := Operation.Setting.Color_Value;
                        Context.Changed (Color_Property) := True;
                     end if;
                  end;
               when Font_Property =>
                  if Context.Current_Font /= Operation.Setting then
                     Context.Current_Font := Operation.Setting;
                     Context.Changed (Font_Property) := True;
                  end if;

               when Line_Width_Property =>
                  null;
               when Fill_Property =>
                  if Context.Current_Fill /= Operation.Setting.Fill_Value then
                     Context.Current_Fill := Operation.Setting.Fill_Value;
                     Context.Changed (Fill_Property) := True;
                  end if;
            end case;
      end case;
   end Draw;

   --------------
   -- End_Draw --
   --------------

   procedure End_Draw
     (Render  : in out Root_Render_Type'Class)
   is
   begin
      Render.Current := Render.Saved.Last_Element;
      Render.Saved.Delete_Last;
   end End_Draw;

   -------------------
   -- Fill_Property --
   -------------------

   function Fill_Property (Fill : Boolean) return Draw_Property is
   begin
      return (Fill_Property, Fill);
   end Fill_Property;

   -------------------
   -- Font_Property --
   -------------------

   function Font_Property
     (Family : String;
      Size   : Nazar_Float;
      Italic : Boolean := False;
      Bold   : Boolean := False)
      return Draw_Property
   is
   begin
      return (Font_Property,
              Ada.Strings.Unbounded.To_Unbounded_String (Family),
              Size, Italic, Bold);
   end Font_Property;

   -------------------------
   -- Get_Screen_Position --
   -------------------------

   procedure Get_Screen_Position
     (Context : Draw_Context;
      World   : Draw_Position;
      X, Y    : out Nazar_Float)
   is
      Scale : constant Nazar_Float :=
        Nazar_Float'Min (Context.Target.W / Context.Viewport.W,
                         Context.Target.H / Context.Viewport.H);
   begin
      if World.World then
         X :=
           (World.X - Context.Viewport.X - Context.Viewport.W / 2.0) * Scale
           + Context.Target.W / 2.0;
         Y :=
           (World.Y - Context.Viewport.Y - Context.Viewport.H / 2.0) * Scale
           + Context.Target.H / 2.0;
      else
         X := World.X;
         Y := World.Y;
      end if;
   end Get_Screen_Position;

   -----------
   -- Image --
   -----------

   function Image
     (Resource_Name : String;
      Width, Height : Nazar_Float;
      Rotation      : Nazar.Trigonometry.Angle)
      return Draw_Operation
   is
   begin
      return Draw_Operation'
        (Primitive      => Image,
         Image_Resource =>
           Ada.Strings.Unbounded.To_Unbounded_String (Resource_Name),
         Image_Width    => Width,
         Image_Height   => Height,
         Image_Rotation => Rotation);
   end Image;

   ----------
   -- Move --
   ----------

   function Move
     (To    : Draw_Position;
      Paint : Boolean)
      return Draw_Operation
   is
   begin
      return (Move, To, Paint);
   end Move;

   ------------
   -- Render --
   ------------

   function Render
     (Preserve : Boolean)
      return Draw_Operation
   is
   begin
      return (Flush, Preserve);
   end Render;

   -------------------
   -- Restore_State --
   -------------------

   function Restore_State return Draw_Operation is
   begin
      return (State, False);
   end Restore_State;

   -------------------
   -- Restore_State --
   -------------------

   procedure Restore_State (Render : in out Root_Render_Type) is
   begin
      Render.Current := Render.Saved.Last_Element;
      Render.Saved.Delete_Last;
   end Restore_State;

   ----------------
   -- Save_State --
   ----------------

   function Save_State return Draw_Operation is
   begin
      return (State, True);
   end Save_State;

   ----------------
   -- Save_State --
   ----------------

   procedure Save_State (Render : in out Root_Render_Type) is
   begin
      Render.Saved.Append (Render.Current);
   end Save_State;

   ---------------------
   -- Screen_Position --
   ---------------------

   function Screen_Position
     (Screen_X, Screen_Y : Nazar_Float) return Draw_Position
   is
   begin
      return (False, Screen_X, Screen_Y);
   end Screen_Position;

   ------------------
   -- Set_Property --
   ------------------

   function Set_Property
     (Property : Draw_Property)
      return Draw_Operation is
   begin
      return (Draw_Operations.Property, Property);
   end Set_Property;

   ----------------
   -- Set_Target --
   ----------------

   procedure Set_Target
     (Context : in out Draw_Context;
      Width   : Nazar_Float;
      Height  : Nazar_Float)
   is
   begin
      Context.Target := (0.0, 0.0, Width, Height);
   end Set_Target;

   ------------------
   -- Set_Viewport --
   ------------------

   procedure Set_Viewport
     (Context  : in out Draw_Context;
      Viewport : Rectangle)
   is
   begin
      Context.Viewport := Viewport;
   end Set_Viewport;

   ----------------
   -- Start_Draw --
   ----------------

   procedure Start_Draw
     (Render  : in out Root_Render_Type'Class;
      Context : Draw_Context)
   is
   begin
      Render.Saved.Append (Render.Current);
      Render.Current := Context;
   end Start_Draw;

   function Text
     (S : String)
      return Draw_Operation
   is
   begin
      return Draw_Operation'
        (Primitive   => Text,
         Draw_Text   => Ada.Strings.Unbounded.To_Unbounded_String (S));
   end Text;

   --------------------
   -- World_Position --
   --------------------

   function World_Position
     (World_X, World_Y : Nazar_Float) return Draw_Position
   is
   begin
      return (True, World_X, World_Y);
   end World_Position;

end Nazar.Draw_Operations;
