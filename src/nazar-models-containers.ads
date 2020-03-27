private with Ada.Containers.Doubly_Linked_Lists;

package Nazar.Models.Containers is

   type Root_Container_Model is
     new Root_Model_Type with private;

   type Nazar_Container_Model is access all Root_Container_Model'Class;

   function Contains
     (Container : Root_Container_Model;
      Model     : not null access Root_Model_Type'Class)
      return Boolean;

   procedure Append
     (Container : in out Root_Container_Model;
      Model     : not null access Root_Model_Type'Class)
     with Pre => not Container.Contains (Model),
     Post => Container.Contains (Model);

   procedure Delete
     (Container : in out Root_Container_Model;
      Model     : not null access Root_Model_Type'Class)
     with Pre => Container.Contains (Model),
     Post => not Container.Contains (Model);

   procedure Iterate
     (Container : Root_Container_Model;
      Process   : not null access
        procedure (Model : not null access Root_Model_Type'Class));

private

   package Model_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Model_Type);

   type Root_Container_Model is
     new Root_Model_Type with
      record
         List : Model_Lists.List;
      end record;

end Nazar.Models.Containers;
