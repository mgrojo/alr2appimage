with Ada.Containers.Indefinite_Vectors;

package String_Vectors is

   package Container is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String);

   subtype Vector is Container.Vector;
   Empty_Vector : Vector renames Container.Empty_Vector;

end String_Vectors;
