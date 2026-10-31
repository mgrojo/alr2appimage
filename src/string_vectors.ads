with Ada.Containers.Indefinite_Vectors;

package String_Vectors is

   package Container is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String);

   subtype Vector is Container.Vector;
   Empty_Vector : Vector renames Container.Empty_Vector;

   -- Split a string into a vector of strings, using the given delimiter.
   procedure Split
     (Source : String;
      Delimiter : String;
      Result : out Vector);

end String_Vectors;
