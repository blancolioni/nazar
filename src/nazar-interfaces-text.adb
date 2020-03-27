with Ada.Characters.Handling;

package body Nazar.Interfaces.Text is

   function Valid_Variable_Name
     (Ch : Character)
      return Boolean;

   ------------------------
   -- Expand_Environment --
   ------------------------

   function Expand_Environment
     (Environment : Text_Environment_Interface'Class;
      Source_Text : String)
      return String
   is
      Quoted         : Boolean := False;
      Variable       : Boolean := False;
      Text_End       : Natural := 0;
      Variable_Start : Positive := 1;
   begin
      for I in Source_Text'Range loop
         declare
            Ch : constant Character := Source_Text (I);
            Id : constant Boolean := Valid_Variable_Name (Ch);
            Last : constant Boolean := I = Source_Text'Last;
         begin
            if Variable and then (Last or else not Id) then
               declare
                  Variable_End : constant Positive :=
                                   (if Id then I else I - 1);
                  Variable_Name : constant String :=
                                    Source_Text
                                      (Variable_Start .. Variable_End);
               begin
                  if Variable_End >= Variable_Start then
                     return Source_Text (Source_Text'First .. Text_End)
                       & Environment.Get_Value (Variable_Name)
                       & Environment.Expand_Environment
                       (Source_Text (Variable_End + 1 .. Source_Text'Last));
                  else
                     Variable := False;
                  end if;
               end;
            elsif not Variable then
               if Ch = ''' then
                  Quoted := not Quoted;
               elsif Ch = '$' and then not Quoted then
                  Variable := True;
                  Text_End := I - 1;
                  Variable_Start := I + 1;
               end if;
            end if;
         end;
      end loop;

      return Source_Text;
   end Expand_Environment;

   -------------------------
   -- Valid_Variable_Name --
   -------------------------

   function Valid_Variable_Name
     (Ch : Character)
      return Boolean
   is
      use Ada.Characters.Handling;
   begin
      return Is_Alphanumeric (Ch) or else Ch = '_' or else Ch = '-';
   end Valid_Variable_Name;

end Nazar.Interfaces.Text;
