private package Nazar.Models.Console.Commands is

   function Cat_Command
     (Scope : Nazar.Models.Scope.Nazar_Scope_Model)
     return Nazar.Interfaces.Commands.Command_Interface'Class;

   function Echo_Command
     return Nazar.Interfaces.Commands.Command_Interface'Class;

   function Change_Scope_Command
     (Scope : Nazar.Models.Scope.Nazar_Scope_Model)
      return Nazar.Interfaces.Commands.Command_Interface'Class;

   function List_Command
     (Scope : Nazar.Models.Scope.Nazar_Scope_Model)
      return Nazar.Interfaces.Commands.Command_Interface'Class;

end Nazar.Models.Console.Commands;
