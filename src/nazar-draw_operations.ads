private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

with Nazar.Colors;
with Nazar.Trigonometry;

package Nazar.Draw_Operations is

   type Draw_Position is private;

   function World_Position
     (World_X, World_Y : Nazar_Float)
      return Draw_Position;

   function Screen_Position
     (Screen_X, Screen_Y : Nazar_Float)
      return Draw_Position;

   type Draw_Context is private;

   procedure Get_Screen_Position
     (Context : Draw_Context;
      World   : Draw_Position;
      X, Y    : out Nazar_Float);

   procedure Set_Target
     (Context : in out Draw_Context;
      Width   : Nazar_Float;
      Height  : Nazar_Float);

   procedure Set_Viewport
     (Context  : in out Draw_Context;
      Viewport : Rectangle);

   function Get_Target_Width
     (Context : Draw_Context)
      return Nazar_Float;

   function Get_Target_Height
     (Context : Draw_Context)
      return Nazar_Float;

   function Get_Viewport
     (Context : Draw_Context)
      return Rectangle;

   type Draw_Property is private;

   function Color_Property
     (Color : Nazar.Colors.Nazar_Color)
      return Draw_Property;

   function Fill_Property
     (Fill : Boolean)
      return Draw_Property;

   function Font_Property
     (Family : String;
      Size   : Nazar_Float;
      Italic : Boolean := False;
      Bold   : Boolean := False)
      return Draw_Property;

   type Draw_Operation (<>) is private;

   function Move
     (To    : Draw_Position;
      Paint : Boolean)
      return Draw_Operation;

   function Arc
     (Radius      : Nazar_Float;
      Start_Angle : Nazar.Trigonometry.Angle;
      End_Angle   : Nazar.Trigonometry.Angle)
      return Draw_Operation;

   function Text
     (S : String)
      return Draw_Operation;

   function Image
     (Resource_Name : String;
      Width, Height : Nazar_Float;
      Rotation      : Nazar.Trigonometry.Angle)
      return Draw_Operation;

   function Render
     (Preserve : Boolean)
      return Draw_Operation;

   function Set_Property
     (Property : Draw_Property)
      return Draw_Operation;

   function Save_State return Draw_Operation;
   function Restore_State return Draw_Operation;

   type Root_Render_Type is abstract tagged private;

   procedure Move_To
     (Render : in out Root_Render_Type;
      X, Y   : Nazar_Float)
   is abstract;

   procedure Line_To
     (Render : in out Root_Render_Type;
      X, Y   : Nazar_Float)
   is abstract;

   procedure Text
     (Render : in out Root_Render_Type;
      S      : String)
   is abstract;

   procedure Arc
     (Render        : in out Root_Render_Type;
      Radius        : Nazar_Float;
      Start_Radians : Nazar_Float;
      End_Radians   : Nazar_Float)
   is abstract;

   procedure Image
     (Render        : in out Root_Render_Type;
      Resource_Name : String;
      Width, Height : Nazar_Float;
      Rotation      : Nazar.Trigonometry.Angle)
   is abstract;

   procedure Set_Color
     (Render : in out Root_Render_Type;
      Color  : Nazar.Colors.Nazar_Color)
   is abstract;

   procedure Set_Font
     (Render : in out Root_Render_Type;
      Family : String;
      Size   : Nazar_Float;
      Italic : Boolean;
      Bold   : Boolean)
   is abstract;

   procedure Render_Current
     (Render   : in out Root_Render_Type;
      Fill     : Boolean;
      Preserve : Boolean)
   is abstract;

   procedure Save_State
     (Render : in out Root_Render_Type);

   procedure Restore_State
     (Render : in out Root_Render_Type);

   procedure Start_Draw
     (Render  : in out Root_Render_Type'Class;
      Context : Draw_Context);

   procedure Draw
     (Render    : in out Root_Render_Type'Class;
      Operation : Draw_Operation);

   procedure End_Draw
     (Render  : in out Root_Render_Type'Class);

private

   type Draw_Position is
      record
         World : Boolean;
         X, Y  : Nazar_Float;
      end record;

   type Draw_Primitive is
     (Move, Arc, Text, Image, Flush, Property, State);

   type Draw_Property_Primitive is
     (No_Property,
      Color_Property,
      Line_Width_Property,
      Fill_Property,
      Font_Property);

   type Draw_Property (Primitive : Draw_Property_Primitive := No_Property) is
      record
         case Primitive is
            when No_Property =>
               null;
            when Color_Property =>
               Color_Value      : Nazar.Colors.Nazar_Color;
            when Line_Width_Property =>
               Line_Width_Value : Nazar_Float;
            when Fill_Property =>
               Fill_Value       : Boolean;
            when Font_Property =>
               Font_Family      : Ada.Strings.Unbounded.Unbounded_String;
               Font_Size        : Nazar_Float;
               Font_Italic      : Boolean;
               Font_Bold        : Boolean;
         end case;
      end record;

   type Property_Flags is array (Draw_Property_Primitive) of Boolean;

   type Draw_Context is
      record
         Target             : Rectangle := (0.0, 0.0, 1.0, 1.0);
         Viewport           : Rectangle := (0.0, 0.0, 1.0, 1.0);
         Current_Color      : Nazar.Colors.Nazar_Color := (1.0, 1.0, 1.0, 1.0);
         Current_Line_Width : Nazar_Float := 1.0;
         Current_Fill       : Boolean := False;
         Current_Font       : Draw_Property (Font_Property);
         Changed            : Property_Flags := (others => True);
      end record;

   function Get_Target_Width
     (Context : Draw_Context)
      return Nazar_Float
   is (Context.Target.W);

   function Get_Target_Height
     (Context : Draw_Context)
      return Nazar_Float
   is (Context.Target.H);

   function Get_Viewport
     (Context : Draw_Context)
      return Rectangle
   is (Context.Viewport);

   type Draw_Operation (Primitive : Draw_Primitive) is
      record
         case Primitive is
            when Move =>
               Destination    : Draw_Position;
               Paint          : Boolean;
            when Arc =>
               Radius         : Nazar_Float;
               Start_Angle    : Nazar.Trigonometry.Angle;
               End_Angle      : Nazar.Trigonometry.Angle;
            when Text =>
               Draw_Text      : Ada.Strings.Unbounded.Unbounded_String;
            when Image =>
               Image_Resource : Ada.Strings.Unbounded.Unbounded_String;
               Image_Width    : Nazar_Float;
               Image_Height   : Nazar_Float;
               Image_Rotation : Nazar.Trigonometry.Angle;
            when Flush =>
               Preserve       : Boolean;
            when Property =>
               Setting        : Draw_Property;
            when State =>
               Save           : Boolean;
         end case;
      end record;

   package Draw_Context_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Draw_Context);

   type Root_Render_Type is abstract tagged
      record
         Current : Draw_Context;
         Saved   : Draw_Context_Lists.List;
      end record;

   procedure Check_State
     (Render : in out Root_Render_Type'Class);

end Nazar.Draw_Operations;
