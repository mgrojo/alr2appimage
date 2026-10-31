with Ada.Strings.Fixed;

package body String_Vectors is

   procedure Split
     (Source : String;
      Delimiter : String;
      Result : out Vector) is
   begin
      Result := Empty_Vector;
      if Source'Length = 0 then
         return;
      end if;

      declare
         Start : Natural := Source'First;
         End_Index : Natural;
      begin
         loop
            End_Index := Ada.Strings.Fixed.Index (Source, Delimiter, Start);
            if End_Index = 0 then
               Container.Append (Result, Source (Start .. Source'Last));
               exit;
            else
               Container.Append (Result, Source (Start .. End_Index - 1));
               Start := End_Index + Delimiter'Length;
            end if;
         end loop;
      end;
   end Split;

end String_Vectors;