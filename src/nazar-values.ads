private with Ada.Strings.Fixed;
private with Ada.Strings.Unbounded;

package Nazar.Values is

   type Nazar_Value_Type is private;

   function Boolean_Value_Type return Nazar_Value_Type;
   function Integer_Value_Type return Nazar_Value_Type;
   function Real_Value_Type return Nazar_Value_Type;
   function Text_Value_Type return Nazar_Value_Type;

   type Nazar_Value is private;

   function Get_Type (Value : Nazar_Value) return Nazar_Value_Type;

   function Image
     (Value : Nazar_Value)
      return String;

   function Default_Value (Of_Type : Nazar_Value_Type) return Nazar_Value;

   function To_Value
     (X : Boolean)
      return Nazar_Value;

   function To_Value
     (X : Integer)
      return Nazar_Value;

   function To_Value
     (X : Nazar_Float)
      return Nazar_Value;

   function To_Value
     (S : String)
      return Nazar_Value;

   function To_Boolean
     (Value : Nazar_Value)
      return Boolean;

   function To_Integer
     (Value : Nazar_Value)
      return Integer;

   function To_Float
     (Value : Nazar_Value)
      return Nazar_Float;

   function To_String
     (Value : Nazar_Value)
      return String;

private

   type Nazar_Value_Type is (T_Boolean, T_Integer, T_Float, T_String);

   function Boolean_Value_Type return Nazar_Value_Type
     renames T_Boolean;

   function Integer_Value_Type return Nazar_Value_Type
     renames T_Integer;

   function Real_Value_Type return Nazar_Value_Type
     renames T_Float;

   function Text_Value_Type return Nazar_Value_Type
     renames T_String;

   type Nazar_Value (V_Type : Nazar_Value_Type := T_Boolean) is
      record
         case V_Type is
            when T_Boolean =>
               V_Boolean : Boolean := False;
            when T_Integer =>
               V_Integer : Integer := 0;
            when T_Float =>
               V_Float : Nazar_Float := 0.0;
            when T_String =>
               V_String  : Ada.Strings.Unbounded.Unbounded_String :=
                 Ada.Strings.Unbounded.Null_Unbounded_String;
         end case;
      end record;

   function Get_Type (Value : Nazar_Value) return Nazar_Value_Type
   is (Value.V_Type);

   function Image
     (Value : Nazar_Value)
      return String
   is (case Value.V_Type is
          when T_Boolean =>
             (if Value.V_Boolean then "true" else "false"),
          when T_Integer =>
             Ada.Strings.Fixed.Trim (Value.V_Integer'Image,
                                     Ada.Strings.Left),
          when T_Float   =>
             Ada.Strings.Fixed.Trim (Value.V_Float'Image,
                                     Ada.Strings.Left),
          when T_String  =>
             Ada.Strings.Unbounded.To_String (Value.V_String));

   function Default_Value (Of_Type : Nazar_Value_Type) return Nazar_Value
   is (case Of_Type is
          when T_Boolean => (T_Boolean, False),
          when T_Integer => (T_Integer, 0),
          when T_Float   => (T_Float, 0.0),
          when T_String  =>
         (T_String, Ada.Strings.Unbounded.Null_Unbounded_String));

   function To_Value
     (X : Boolean)
      return Nazar_Value
   is (T_Boolean, X);

   function To_Value
     (X : Integer)
      return Nazar_Value
   is (T_Integer, X);

   function To_Value
     (X : Nazar_Float)
      return Nazar_Value
   is (T_Float, X);

   function To_Value
     (S : String)
      return Nazar_Value
   is (T_String,
       Ada.Strings.Unbounded.To_Unbounded_String (S));

   function To_Boolean
     (Value : Nazar_Value)
      return Boolean
   is (case Value.V_Type is
          when T_Boolean => Value.V_Boolean,
          when T_Integer => Value.V_Integer /= 0,
          when T_Float   => Value.V_Float /= 0.0,
          when T_String  => To_String (Value) /= "");

   function To_Integer
     (Value : Nazar_Value)
      return Integer
   is (case Value.V_Type is
          when T_Boolean => Boolean'Pos (Value.V_Boolean),
          when T_Integer => Value.V_Integer,
          when T_Float   => Integer (Value.V_Float),
          when T_String  =>
             Integer'Value (To_String (Value)));

   function To_Float
     (Value : Nazar_Value)
      return Nazar_Float
   is (case Value.V_Type is
          when T_Boolean => Nazar_Float (Boolean'Pos (Value.V_Boolean)),
          when T_Integer => Nazar_Float (Value.V_Integer),
          when T_Float   => Value.V_Float,
          when T_String  =>
             Nazar_Float'Value (To_String (Value)));

   function To_String
     (Value : Nazar_Value)
      return String
   is (Image (Value));

end Nazar.Values;
