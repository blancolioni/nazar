with Nazar.Interfaces.Hierarchy;

package Nazar.Models.Directories is

   function Directory_Model
     (Root_Path : String)
      return Nazar.Interfaces.Hierarchy.Node_Reference_Class;

end Nazar.Models.Directories;
