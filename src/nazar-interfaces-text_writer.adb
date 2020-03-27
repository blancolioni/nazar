package body Nazar.Interfaces.Text_Writer is

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Writer : in out Text_Writer_Interface'Class) is
   begin
      Writer.New_Line (Standard_Text);
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (Writer : in out Text_Writer_Interface'Class;
                  Text   : String)
   is
   begin
      Writer.Put (Standard_Text, Text);
   end Put;

   ---------------
   -- Put_Error --
   ---------------

   procedure Put_Error
     (Writer  : in out Text_Writer_Interface'Class;
      Message : String)
   is
   begin
      Writer.Put_Line (Error_Text, Message);
   end Put_Error;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Writer : in out Text_Writer_Interface'Class;
      Class  : Text_Class;
      Line   : String)
   is
   begin
      Writer.Put (Class, Line);
      Writer.New_Line (Class);
   end Put_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Writer : in out Text_Writer_Interface'Class;
      Line   : String)
   is
   begin
      Writer.Put_Line (Standard_Text, Line);
   end Put_Line;

   ---------------
   -- Put_Lines --
   ---------------

   procedure Put_Lines
     (Writer    : in out Text_Writer_Interface'Class;
      Lines     : String_Lists.List)
   is
   begin
      for Line of Lines loop
         Writer.Put_Line (Line);
      end loop;
   end Put_Lines;

   ---------------
   -- Put_Lines --
   ---------------

   procedure Put_Lines
     (Writer    : in out Text_Writer_Interface'Class;
      Lines     : String;
      Separator : Character := Character'Val (10))
   is
      Start : Positive := Lines'First;
   begin
      for I in Lines'Range loop
         if Lines (I) = Separator then
            Writer.Put_Line (Lines (Start .. I - 1));
            Start := I + 1;
         end if;
      end loop;

      if Start <= Lines'Last then
         Writer.Put (Lines (Start .. Lines'Last));
      end if;
   end Put_Lines;

   ---------------
   -- Put_Names --
   ---------------

   procedure Put_Names
     (Writer          : in out Text_Writer_Interface'Class;
      Names           : String_Lists.List;
      Available_Width : Natural := 72;
      Max_Columns     : Positive := 12;
      Sorted          : Boolean := True;
      Down_First      : Boolean := True)
   is
      pragma Unreferenced (Down_First);

      package Sorting is
        new String_Lists.Generic_Sorting ("<");

      Count : constant Natural :=
        Natural (Names.Length);
      Ids          : String_Lists.List := Names;
      Longest      : Natural := 0;
      Cols         : Positive := 1;
      Col_Index    : Positive;
      Field_Width  : Positive;
      Total_Width  : Natural := 0;
      Single_Line  : Boolean := False;
   begin

      if Sorted then
         Sorting.Sort (Ids);
      end if;

      for Id of Ids loop
         if Id'Length > Longest then
            Longest := Id'Length;
         end if;
         Total_Width := Total_Width + Id'Length;
      end loop;

      if Longest = 0 then
         return;
      end if;

      if Total_Width + 2 * (Count - 1) < Available_Width then
         Single_Line := True;
      end if;

      if Single_Line then
         declare
            First : Boolean := True;
         begin
            for Id of Ids loop
               if First then
                  First := False;
               else
                  Writer.Put ("  ");
               end if;
               Writer.Put (Id);
            end loop;
            Writer.New_Line;
         end;
      else
         Cols := Natural'Max
           (Natural'Min (Available_Width / (Longest + 2), Max_Columns), 1);
         Field_Width := Available_Width / Cols;

         Col_Index := 1;

         for Id of Ids loop
            declare
               Field : String (1 .. Field_Width) := (others => ' ');
            begin
               Field (1 .. Id'Length) := Id;
               Writer.Put (Field);
            end;
            if Col_Index = Cols then
               Col_Index := 1;
               Writer.New_Line;
            else
               Col_Index := Col_Index + 1;
            end if;
         end loop;

         if Col_Index /= 1 then
            Writer.New_Line;
         end if;
      end if;
   end Put_Names;

end Nazar.Interfaces.Text_Writer;
