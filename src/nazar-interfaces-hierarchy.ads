package Nazar.Interfaces.Hierarchy is

   type Node_Reference_Interface is interface;
   subtype Node_Reference_Class is Node_Reference_Interface'Class;

   function Is_Empty
     (Reference : Node_Reference_Interface)
      return Boolean
      is abstract;

   function No_Node return Node_Reference_Class;

   type Node_Interface is interface;

   function Is_Leaf
     (Node : Node_Interface)
      return Boolean
      is abstract;

   function Contents
     (Node : Node_Interface)
      return String
      is abstract;

   function Has_Child
     (Node : Node_Interface;
      Name : String)
      return Boolean
      is abstract
     with Pre'Class => not Is_Leaf (Node);

   function Get_Child
     (Node  : Node_Interface;
      Child : String)
      return Node_Reference_Class
      is abstract
     with Pre'Class => not Is_Leaf (Node);

   procedure Bind_Child
     (Node  : in out Node_Interface;
      Name  : String;
      Child : Node_Reference_Class)
   is abstract
     with Pre'Class => not Is_Leaf (Node);

   procedure Delete_Child
     (Node   : in out Node_Interface;
      Name   : String)
   is abstract
     with Pre'Class => not Is_Leaf (Node)
     and then Has_Child (Node_Interface'Class (Node), Name);

   procedure Iterate_Children
     (Node    : Node_Interface;
      Process : not null access
        procedure (Name : String;
                   Child : Node_Reference_Class))
   is abstract
     with Pre'Class => not Is_Leaf (Node);

   function Get
     (Id : Node_Reference_Interface)
      return Node_Interface'Class
      is abstract;

   function Update
     (Node : Node_Reference_Interface)
      return access Node_Interface'Class
      is abstract;

private

   type Empty_Reference is new Node_Reference_Interface with null record;

   overriding function Is_Empty
     (Reference : Empty_Reference)
      return Boolean
   is (True);

   overriding function Get
     (Id : Empty_Reference)
      return Node_Interface'Class
   is (raise Constraint_Error with "get called on empty reference");

   overriding function Update
     (Node : Empty_Reference)
      return access Node_Interface'Class
   is (raise Constraint_Error with "update called on empty reference");

   function No_Node return Node_Reference_Class
   is (Empty_Reference'(null record));

end Nazar.Interfaces.Hierarchy;
