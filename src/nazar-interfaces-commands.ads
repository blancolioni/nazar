with Nazar.Interfaces.Text_Writer;
with Nazar.Interfaces.Text;

package Nazar.Interfaces.Commands is

   type Command_Interface is interface;

   function Name (Command : Command_Interface) return String is abstract;
   function Version (Command : Command_Interface) return String is abstract;
   function Usage (Command : Command_Interface) return String is abstract;
   function Help (Command : Command_Interface) return String is abstract;

   type Arguments_Interface is interface;

   function Check_Bindings
     (Arguments : Arguments_Interface;
      Binding_OK : not null access
        function (Name, Value : String) return Boolean)
      return Boolean
      is abstract;

   function Check
     (Command   : Command_Interface;
      Arguments : Arguments_Interface'Class)
      return Boolean
      is abstract;

   procedure Execute
     (Command   : Command_Interface;
      Arguments : Arguments_Interface'Class;
      Environment : not null access
        Nazar.Interfaces.Text.Text_Environment_Interface'Class;
      Writer    : in out Nazar.Interfaces.Text_Writer
      .Text_Writer_Interface'Class)
   is abstract;

   function Argument_Count
     (Arguments : Arguments_Interface)
      return Natural
      is abstract;

   function Argument
     (Arguments : Arguments_Interface;
      Index     : Positive)
      return String
   is abstract
     with Pre'Class => Index <= Argument_Count (Arguments);

   function Has_Binding
     (Argument : Arguments_Interface;
      Name     : String)
      return Boolean
   is abstract;

   function Binding
     (Arguments : Arguments_Interface;
      Name      : String)
      return String
   is abstract
     with Pre'Class => Arguments.Has_Binding (Name);

   function Binding
     (Arguments : Arguments_Interface'Class;
      Name      : String;
      Default   : String)
      return String;

private

   function Binding
     (Arguments : Arguments_Interface'Class;
      Name      : String;
      Default   : String)
      return String
   is (if Arguments.Has_Binding (Name)
       then Arguments.Binding (Name)
       else Default);

end Nazar.Interfaces.Commands;
