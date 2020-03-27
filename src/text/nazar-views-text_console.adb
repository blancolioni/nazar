with Ada.Text_IO;

with Nazar.Interfaces.Text_Writer;

package body Nazar.Views.Text_Console is

   procedure Put_Class_Line
     (Class : Nazar.Interfaces.Text_Writer.Text_Class;
      Line  : String);

   -------------------
   -- Model_Changed --
   -------------------

   overriding procedure Model_Changed
     (View : in out Root_Text_Console_View)
   is
   begin
      View.Console_Model.Iterate_Lines
        (Start   => View.Last_Line,
         Process => Put_Class_Line'Access);
   end Model_Changed;

   --------------------
   -- Put_Class_Line --
   --------------------

   procedure Put_Class_Line
     (Class : Nazar.Interfaces.Text_Writer.Text_Class;
      Line  : String)
   is
      use all type Nazar.Interfaces.Text_Writer.Text_Class;
   begin
      case Class is
         when Standard_Text =>
            Ada.Text_IO.Put_Line (Line);
         when Error_Text =>
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error, Line);
      end case;
   end Put_Class_Line;

   ----------
   -- Show --
   ----------

   overriding procedure Show
     (View : in out Root_Text_Console_View)
   is
   begin
      loop
         Ada.Text_IO.Put (View.Console_Model.Get_Prompt_Text);
         Ada.Text_IO.Flush;
         declare
            Line : constant String := Ada.Text_IO.Get_Line;
         begin
            if Line = "" then
               null;
            elsif Line = "exit" then
               exit;
            else
               View.Emit_Command_Signal
                 (Command => Line);
            end if;
         end;
      end loop;
   end Show;

   -----------------------
   -- Text_Console_View --
   -----------------------

   function Text_Console_View
     (Model : not null access Nazar.Models.Console.Root_Console_Model'Class)
      return Nazar_Text_Console_View
   is
   begin
      return View : constant Nazar_Text_Console_View :=
        new Root_Text_Console_View
      do
         View.Last_Line := Model.First_Line;
         View.Set_Model (Model);
      end return;
   end Text_Console_View;

end Nazar.Views.Text_Console;
