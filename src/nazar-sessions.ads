package Nazar.Sessions is

   type Root_Session_Type is abstract tagged private;

   type Nazar_Session is access all Root_Session_Type'Class;

private

   type Root_Session_Type is abstract tagged
      record
         null;
      end record;

end Nazar.Sessions;
