with Nazar.Interfaces.Text_Writer;

package body Nazar.Models.Console.Commands is

   function Line (S : String) return String
                  renames Nazar.Interfaces.Text_Writer.Line;

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   type Cat_Command_Record is new Internal_Command with null record;

   overriding function Name
     (Command : Cat_Command_Record)
      return String
   is ("cat");

   overriding function Usage
     (Command : Cat_Command_Record)
      return String;

   overriding function Help
     (Command : Cat_Command_Record)
      return String;

   overriding function Check
     (Command   : Cat_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class)
      return Boolean;

   overriding procedure Execute
     (Command   : Cat_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class;
      Environment : not null access
        Nazar.Interfaces.Text.Text_Environment_Interface'Class;
      Writer    : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class);

   type Change_Scope_Command_Record is new Internal_Command with null record;

   overriding function Name
     (Command : Change_Scope_Command_Record)
      return String
   is ("cd");

   overriding function Usage
     (Command : Change_Scope_Command_Record)
      return String;

   overriding function Help
     (Command : Change_Scope_Command_Record)
      return String;

   overriding function Check
     (Command   : Change_Scope_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class)
      return Boolean;

   overriding procedure Execute
     (Command   : Change_Scope_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class;
      Environment : not null access
        Nazar.Interfaces.Text.Text_Environment_Interface'Class;
      Writer    : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class);

   type Echo_Command_Record is new Internal_Command with null record;

   overriding function Name
     (Command : Echo_Command_Record)
      return String
   is ("echo");

   overriding function Usage
     (Command : Echo_Command_Record)
      return String;

   overriding function Help
     (Command : Echo_Command_Record)
      return String;

   overriding function Check
     (Command   : Echo_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class)
      return Boolean;

   overriding procedure Execute
     (Command   : Echo_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class;
      Environment : not null access
        Nazar.Interfaces.Text.Text_Environment_Interface'Class;
      Writer    : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class);

   type List_Command_Record is new Internal_Command with null record;

   overriding function Name
     (Command : List_Command_Record)
      return String
   is ("ls");

   overriding function Usage
     (Command : List_Command_Record)
      return String;

   overriding function Help
     (Command : List_Command_Record)
      return String;

   overriding function Check
     (Command   : List_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class)
      return Boolean;

   overriding procedure Execute
     (Command   : List_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class;
      Environment : not null access
        Nazar.Interfaces.Text.Text_Environment_Interface'Class;
      Writer    : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class);

   -----------------
   -- Cat_Command --
   -----------------

   function Cat_Command
     (Scope : Nazar.Models.Scope.Nazar_Scope_Model)
      return Nazar.Interfaces.Commands.Command_Interface'Class
   is
   begin
      return Cat_Command_Record'(Name => +"cat", Scope => Scope);
   end Cat_Command;

   --------------------------
   -- Change_Scope_Command --
   --------------------------

   function Change_Scope_Command
     (Scope : Nazar.Models.Scope.Nazar_Scope_Model)
      return Nazar.Interfaces.Commands.Command_Interface'Class
   is
   begin
      return Change_Scope_Command_Record'(Name => +"cd", Scope => Scope);
   end Change_Scope_Command;

   -----------
   -- Check --
   -----------

   overriding function Check
     (Command   : Cat_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class)
      return Boolean
   is
      pragma Unreferenced (Command);
      function Binding_OK (Name, Value : String) return Boolean;

      ----------------
      -- Binding_OK --
      ----------------

      function Binding_OK (Name, Value : String) return Boolean is
         pragma Unreferenced (Value);
      begin
         return Name = "help" or else Name = "version";
      end Binding_OK;

   begin
      return Arguments.Check_Bindings (Binding_OK'Access);
   end Check;

   -----------
   -- Check --
   -----------

   overriding function Check
     (Command   : Change_Scope_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class)
      return Boolean
   is
      pragma Unreferenced (Command);
      function Binding_OK (Name, Value : String) return Boolean;

      ----------------
      -- Binding_OK --
      ----------------

      function Binding_OK (Name, Value : String) return Boolean is
         pragma Unreferenced (Value);
      begin
         return Name = "help" or else Name = "version";
      end Binding_OK;

   begin
      return Arguments.Check_Bindings (Binding_OK'Access)
        and then Arguments.Argument_Count = 1;
   end Check;

   -----------
   -- Check --
   -----------

   overriding function Check
     (Command   : Echo_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class)
      return Boolean
   is
      pragma Unreferenced (Command);
      function Binding_OK (Name, Value : String) return Boolean;

      ----------------
      -- Binding_OK --
      ----------------

      function Binding_OK (Name, Value : String) return Boolean is
         pragma Unreferenced (Value);
      begin
         return Name = "help" or else Name = "version" or else Name = "n";
      end Binding_OK;

   begin
      return Arguments.Check_Bindings (Binding_OK'Access);
   end Check;

   -----------
   -- Check --
   -----------

   overriding function Check
     (Command   : List_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class)
      return Boolean
   is
      pragma Unreferenced (Command);
      function Binding_OK (Name, Value : String) return Boolean;

      ----------------
      -- Binding_OK --
      ----------------

      function Binding_OK (Name, Value : String) return Boolean is
         pragma Unreferenced (Value);
      begin
         return Name = "help" or else Name = "version";
      end Binding_OK;

   begin
      return Arguments.Check_Bindings (Binding_OK'Access);
   end Check;

   ------------------
   -- Echo_Command --
   ------------------

   function Echo_Command
     return Nazar.Interfaces.Commands.Command_Interface'Class
   is
   begin
      return Echo_Command_Record'(Name => +"echo", Scope => null);
   end Echo_Command;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command   : Echo_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class;
      Environment : not null access
        Nazar.Interfaces.Text.Text_Environment_Interface'Class;
      Writer    : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class)
   is
      pragma Unreferenced (Command, Environment);
   begin
      for I in 1 .. Arguments.Argument_Count loop
         if I > 1 then
            Writer.Put (" ");
         end if;
         Writer.Put (Arguments.Argument (I));
      end loop;

      if not Arguments.Has_Binding ("n") then
         Writer.New_Line;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command   : Cat_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class;
      Environment : not null access
        Nazar.Interfaces.Text.Text_Environment_Interface'Class;
      Writer    : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class)
   is
      pragma Unreferenced (Environment);
   begin
      for I in 1 .. Arguments.Argument_Count loop
         declare
            File : constant Nazar.Interfaces.Hierarchy.Node_Reference_Class
              := Command.Scope.Get_Node (Arguments.Argument (I));
         begin
            if File.Is_Empty then
               Writer.Put_Line
                 (Nazar.Interfaces.Text_Writer.Error_Text,
                  Arguments.Argument (I) & ": no such file or directory");
            else
               declare
                  procedure Process_Line (Line : String);

                  ------------------
                  -- Process_Line --
                  ------------------

                  procedure Process_Line (Line : String) is
                  begin
                     Writer.Put_Line (Line);
                  end Process_Line;

               begin

                  Iterate_Lines (File.Get.Contents, Process_Line'Access);

               exception
                  when others =>
                     Writer.Put_Line
                       (Nazar.Interfaces.Text_Writer.Error_Text,
                        Arguments.Argument (I) & ": cannot read");
               end;
            end if;
         end;
      end loop;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command   : Change_Scope_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class;
      Environment : not null access
        Nazar.Interfaces.Text.Text_Environment_Interface'Class;
      Writer    : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class)
   is
      Path : constant String := Arguments.Argument (1);
   begin
      if Command.Scope.Change_Scope (Arguments.Argument (1)) then
         Environment.Set_Value
           ("CURRENT_SCOPE", Command.Scope.Current_Scope);
      else
         Writer.Put_Line
           (Path & ": not a directory");
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command   : List_Command_Record;
      Arguments : Nazar.Interfaces.Commands.Arguments_Interface'Class;
      Environment : not null access
        Nazar.Interfaces.Text.Text_Environment_Interface'Class;
      Writer    : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class)
   is
      pragma Unreferenced (Environment);

      procedure Process
        (Name : String;
         Node : Nazar.Interfaces.Hierarchy.Node_Reference_Class);

      procedure Process
        (Name : String;
         Node : Nazar.Interfaces.Hierarchy.Node_Reference_Class)
      is
         List : Nazar.Interfaces.Text_Writer.String_Lists.List;

         procedure Add
           (Name : String;
            Node : Nazar.Interfaces.Hierarchy.Node_Reference_Class);

         ---------
         -- Add --
         ---------

         procedure Add
           (Name : String;
            Node : Nazar.Interfaces.Hierarchy.Node_Reference_Class)
         is
            pragma Unreferenced (Node);
         begin
            List.Append (Name);
         end Add;

      begin
         if Node.Get.Is_Leaf then
            Add (Name, Node);
         else
            Node.Get.Iterate_Children
              (Add'Access);
         end if;

         Writer.Put_Names
           (Names           => List,
            Available_Width => 72,
            Sorted          => True,
            Down_First      => True);

      end Process;

   begin
      if Arguments.Argument_Count = 0 then
         Process ("", Command.Scope.Current_Node);
      elsif Arguments.Argument_Count = 1 then
         Process (Arguments.Argument (1),
                  Command.Scope.Get_Node (Arguments.Argument (1)));
      else
         for I in 1 .. Arguments.Argument_Count loop
            Writer.Put_Line
              (Arguments.Argument (I) & ":");
            Process (Arguments.Argument (I),
                     Command.Scope.Get_Node (Arguments.Argument (I)));
         end loop;
      end if;
   end Execute;

   ----------
   -- Help --
   ----------

   overriding function Help
     (Command : Cat_Command_Record)
      return String
   is
      pragma Unreferenced (Command);
   begin
      return Line ("Concatenate FILE(s) to standard output.")
        & Line ("")
        & Line ("      --help     display this help and exit")
        & Line ("      --version  output version information and exit");
   end Help;

   ----------
   -- Help --
   ----------

   overriding function Help
     (Command : Change_Scope_Command_Record)
      return String
   is
      pragma Unreferenced (Command);
   begin
      return Line ("Change the console's active scope")
        & Line ("")
        & Line ("Change the active scope to PATH.  The default PATH is the")
        & Line ("value of the HOME environment variable");
   end Help;

   ----------
   -- Help --
   ----------

   overriding function Help
     (Command : Echo_Command_Record)
      return String
   is
      pragma Unreferenced (Command);
   begin
      return Line ("Echo the STRING(s) to standard output.")
        & Line ("")
        & Line ("  -n             do not output the trailing newline")
        & Line ("      --help     display this help and exit")
        & Line ("      --version  output version information and exit");
   end Help;

   ----------
   -- Help --
   ----------

   overriding function Help
     (Command : List_Command_Record)
      return String
   is
      pragma Unreferenced (Command);
   begin
      return Line ("List information about the FILEs "
                   & "(the current scope by default).")
        & Line ("Sort entries alphabetically unless overridden");
   end Help;

   ------------------
   -- List_Command --
   ------------------

   function List_Command
     (Scope : Nazar.Models.Scope.Nazar_Scope_Model)
      return Nazar.Interfaces.Commands.Command_Interface'Class
   is
   begin
      return List_Command_Record'(Name => +"ls", Scope => Scope);
   end List_Command;

   -----------
   -- Usage --
   -----------

   overriding function Usage
     (Command : Cat_Command_Record)
      return String
   is
      pragma Unreferenced (Command);
   begin
      return Line ("usage: cat FILE");
   end Usage;

   -----------
   -- Usage --
   -----------

   overriding function Usage
     (Command : Change_Scope_Command_Record)
      return String
   is
      pragma Unreferenced (Command);
   begin
      return Line ("usage: cd [PATH]");
   end Usage;

   -----------
   -- Usage --
   -----------

   overriding function Usage
     (Command : Echo_Command_Record)
      return String
   is
      pragma Unreferenced (Command);
   begin
      return Line ("usage: echo [ -n ] [STRING]...");
   end Usage;

   -----------
   -- Usage --
   -----------

   overriding function Usage
     (Command : List_Command_Record)
      return String
   is
      pragma Unreferenced (Command);
   begin
      return Line ("usage: ls [OPTION]... [FILE]...");
   end Usage;

end Nazar.Models.Console.Commands;
