with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Nazar.Interfaces.Text_Writer is

   type Text_Class is (Standard_Text, Error_Text);

   type Text_Writer_Interface is interface;

   procedure Put
     (Writer : in out Text_Writer_Interface;
      Class  : Text_Class;
      Text   : String)
   is abstract;

   procedure New_Line
     (Model : in out Text_Writer_Interface;
      Class : Text_Class)
   is abstract;

   procedure Flush
     (Model : in out Text_Writer_Interface)
   is abstract;

   procedure Put
     (Writer : in out Text_Writer_Interface'Class;
      Text   : String);

   procedure New_Line
     (Writer : in out Text_Writer_Interface'Class);

   procedure Put_Line
     (Writer : in out Text_Writer_Interface'Class;
      Class  : Text_Class;
      Line   : String);

   procedure Put_Line
     (Writer : in out Text_Writer_Interface'Class;
      Line   : String);

   procedure Put_Error
     (Writer  : in out Text_Writer_Interface'Class;
      Message : String);

   procedure Put_Lines
     (Writer    : in out Text_Writer_Interface'Class;
      Lines     : String;
      Separator : Character := Character'Val (10));

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   procedure Put_Lines
     (Writer    : in out Text_Writer_Interface'Class;
      Lines     : String_Lists.List);

   procedure Put_Names
     (Writer          : in out Text_Writer_Interface'Class;
      Names           : String_Lists.List;
      Available_Width : Natural := 72;
      Max_Columns     : Positive := 12;
      Sorted          : Boolean := True;
      Down_First      : Boolean := True);

   function Line (Text : String) return String;

private

   function Line (Text : String) return String
   is (Text & Character'Val (10));

end Nazar.Interfaces.Text_Writer;
