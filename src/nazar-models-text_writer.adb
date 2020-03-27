package body Nazar.Models.Text_Writer is

   procedure Check_Class
     (Model : in out Root_Text_Writer_Model'Class;
      Class : Nazar.Interfaces.Text_Writer.Text_Class)
     with Post => Nazar.Interfaces.Text_Writer."="
       (Model.Current_Class, Class);

   -----------------
   -- Check_Class --
   -----------------

   procedure Check_Class
     (Model : in out Root_Text_Writer_Model'Class;
      Class : Nazar.Interfaces.Text_Writer.Text_Class)
   is
      use type Nazar.Interfaces.Text_Writer.Text_Class;
   begin
      if Model.Current_Class /= Class
        and then Ada.Strings.Unbounded.Length (Model.Current_Text) > 0
      then
         Model.New_Line (Model.Current_Class);
      end if;
      Model.Current_Class := Class;
   end Check_Class;

   ----------------
   -- First_Line --
   ----------------

   function First_Line
     (Model : Root_Text_Writer_Model'Class)
      return Line_Cursor
   is
      pragma Unreferenced (Model);
   begin
      return Position : Line_Cursor;
   end First_Line;

   -----------
   -- Flush --
   -----------

   overriding procedure Flush
     (Model : in out Root_Text_Writer_Model)
   is
   begin
      if Ada.Strings.Unbounded.Length (Model.Current_Text) > 0 then
         Model.New_Line (Model.Current_Class);
      end if;
   end Flush;

   -------------------
   -- Iterate_Lines --
   -------------------

   procedure Iterate_Lines
     (Model   : Root_Text_Writer_Model'Class;
      Process : not null access
        procedure (Class : Nazar.Interfaces.Text_Writer.Text_Class;
                   Line : String))
   is
      Start : Line_Cursor;
   begin
      Model.Iterate_Lines (Start, Process);
   end Iterate_Lines;

   -------------------
   -- Iterate_Lines --
   -------------------

   procedure Iterate_Lines
     (Model   : Root_Text_Writer_Model'Class;
      Start   : in out Line_Cursor;
      Process : not null access
        procedure (Class : Nazar.Interfaces.Text_Writer.Text_Class;
                   Line : String))
   is
      It : Line_Lists.Cursor := Start.Position;
   begin
      if not Line_Lists.Has_Element (It) then
         It := Model.Lines.First;
      else
         It := Line_Lists.Next (It);
      end if;

      while Line_Lists.Has_Element (It) loop
         Start.Position := It;
         Process (Model.Lines (It).Class, Model.Lines (It).Text);
         Line_Lists.Next (It);
      end loop;
   end Iterate_Lines;

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line
     (Model : in out Root_Text_Writer_Model;
      Class : Nazar.Interfaces.Text_Writer.Text_Class)
   is
   begin
      Model.Check_Class (Class);

      declare
         use Ada.Strings.Unbounded;
         Text : constant String := To_String (Model.Current_Text);
      begin
         Model.Lines.Append
           (Line_Record'
              (Length => Text'Length,
               Class  => Class,
               Text   => Text));
         Model.Current_Text := Null_Unbounded_String;
      end;
   end New_Line;

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (Writer : in out Root_Text_Writer_Model;
      Class  : Nazar.Interfaces.Text_Writer.Text_Class;
      Text   : String)
   is
   begin
      Writer.Check_Class (Class);
      Ada.Strings.Unbounded.Append (Writer.Current_Text, Text);
   end Put;

end Nazar.Models.Text_Writer;
