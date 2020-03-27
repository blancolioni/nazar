with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

with WL.String_Maps;

package body Nazar.Models.Directories is

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Directory_Node;

   type Directory_Node_Access is
     access all Directory_Node'Class;

   package Directory_Node_Maps is
     new WL.String_Maps (Directory_Node_Access);

   type Directory_Node is
     new Nazar.Interfaces.Hierarchy.Node_Interface with
      record
         Path              : Ada.Strings.Unbounded.Unbounded_String;
         Child_Directories : Directory_Node_Maps.Map;
         Files             : String_Lists.List;
      end record;

   overriding function Is_Leaf
     (Node : Directory_Node)
      return Boolean
   is (False);

   overriding function Contents
     (Node : Directory_Node)
      return String
   is (Ada.Strings.Unbounded.To_String (Node.Path));

   overriding function Has_Child
     (Node : Directory_Node;
      Name : String)
      return Boolean
   is (Node.Child_Directories.Contains (Name)
       or else Node.Files.Contains (Name));

   overriding function Get_Child
     (Node  : Directory_Node;
      Child : String)
      return Nazar.Interfaces.Hierarchy.Node_Reference_Class;

   overriding procedure Bind_Child
     (Node  : in out Directory_Node;
      Name  : String;
      Child : Nazar.Interfaces.Hierarchy.Node_Reference_Class);

   overriding procedure Delete_Child
     (Node   : in out Directory_Node;
      Name   : String);

   overriding procedure Iterate_Children
     (Node    : Directory_Node;
      Process : not null access
        procedure (Name : String;
                   Child : Nazar.Interfaces.Hierarchy.Node_Reference_Class));

   type Directory_Reference is
     new Nazar.Interfaces.Hierarchy.Node_Reference_Interface with
      record
         Node : access Directory_Node'Class;
      end record;

   overriding function Is_Empty
     (Reference : Directory_Reference)
      return Boolean
   is (Reference.Node = null);

   overriding function Get
     (Id : Directory_Reference)
      return Nazar.Interfaces.Hierarchy.Node_Interface'Class
   is (Id.Node.all);

   overriding function Update
     (Id : Directory_Reference)
      return access Nazar.Interfaces.Hierarchy.Node_Interface'Class
   is (Id.Node);

   ----------------
   -- Bind_Child --
   ----------------

   overriding procedure Bind_Child
     (Node  : in out Directory_Node;
      Name  : String;
      Child : Nazar.Interfaces.Hierarchy.Node_Reference_Class)
   is
   begin
      raise Constraint_Error with
        "read-only file system access";
   end Bind_Child;

   ------------------
   -- Delete_Child --
   ------------------

   overriding procedure Delete_Child
     (Node   : in out Directory_Node;
      Name   : String)
   is
   begin
      raise Constraint_Error with
        "read-only filesystem access";
   end Delete_Child;

   ---------------------
   -- Directory_Model --
   ---------------------

   function Directory_Model
     (Root_Path : String)
      return Nazar.Interfaces.Hierarchy.Node_Reference_Class
   is
      Root_Node : constant Directory_Node_Access :=
        new Directory_Node'
          (Path   => Ada.Strings.Unbounded.To_Unbounded_String (Root_Path),
           others => <>);
      Root_Ref  : constant Directory_Reference :=
        Directory_Reference'
          (Node => Root_Node);
   begin
      return Root_Ref;
   end Directory_Model;

   ---------------
   -- Get_Child --
   ---------------

   overriding function Get_Child
     (Node  : Directory_Node;
      Child : String)
      return Nazar.Interfaces.Hierarchy.Node_Reference_Class
   is
   begin
      if Node.Child_Directories.Contains (Child) then
         return Directory_Reference'
           (Node => Node.Child_Directories.Element (Child));
      else
         return (raise Constraint_Error with
                   "files not implemented yet");
      end if;
   end Get_Child;

   ----------------------
   -- Iterate_Children --
   ----------------------

   overriding procedure Iterate_Children
     (Node    : Directory_Node;
      Process : not null access
        procedure (Name : String;
                   Child : Nazar.Interfaces.Hierarchy.Node_Reference_Class))
   is
   begin
      for Position in Node.Child_Directories.Iterate loop
         Process (Directory_Node_Maps.Key (Position),
                  Directory_Reference'
                    (Node => Directory_Node_Maps.Element (Position)));
      end loop;
   end Iterate_Children;

end Nazar.Models.Directories;
