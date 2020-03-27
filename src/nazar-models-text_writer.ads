private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

with Nazar.Interfaces.Text_Writer;

package Nazar.Models.Text_Writer is

   type Root_Text_Writer_Model is
     new Nazar_Model_Record
     and Nazar.Interfaces.Text_Writer.Text_Writer_Interface
   with private;

   overriding procedure Put
     (Writer : in out Root_Text_Writer_Model;
      Class  : Nazar.Interfaces.Text_Writer.Text_Class;
      Text   : String);

   overriding procedure New_Line
     (Model : in out Root_Text_Writer_Model;
      Class  : Nazar.Interfaces.Text_Writer.Text_Class);

   overriding procedure Flush
     (Model : in out Root_Text_Writer_Model);

   procedure Iterate_Lines
     (Model : Root_Text_Writer_Model'Class;
      Process : not null access
        procedure (Class : Nazar.Interfaces.Text_Writer.Text_Class;
                   Line  : String));

   type Line_Cursor is private;

   function First_Line
     (Model : Root_Text_Writer_Model'Class)
      return Line_Cursor;

   procedure Iterate_Lines
     (Model   : Root_Text_Writer_Model'Class;
      Start   : in out Line_Cursor;
      Process : not null access
        procedure (Class : Nazar.Interfaces.Text_Writer.Text_Class;
                   Line  : String));

private

   type Line_Record (Length : Natural) is
      record
         Class : Nazar.Interfaces.Text_Writer.Text_Class;
         Text  : String (1 .. Length);
      end record;

   package Line_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Line_Record);

   type Line_Cursor is
      record
         Position : Line_Lists.Cursor := Line_Lists.No_Element;
      end record;

   type Root_Text_Writer_Model is
     new Nazar_Model_Record
     and Nazar.Interfaces.Text_Writer.Text_Writer_Interface with
      record
         Current_Class : Nazar.Interfaces.Text_Writer.Text_Class :=
           Nazar.Interfaces.Text_Writer.Standard_Text;
         Current_Text  : Ada.Strings.Unbounded.Unbounded_String;
         Lines         : Line_Lists.List;
      end record;

   overriding function Class_Name
     (Model : Root_Text_Writer_Model)
      return String
   is ("nazar-text-writer-model");

end Nazar.Models.Text_Writer;
