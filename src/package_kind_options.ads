with Basic_Types;
with Parse_Args.Generic_Discrete_Options;

package Package_Kind_Options is new
  Parse_Args.Generic_Discrete_Options
    (Element => Basic_Types.Package_Kind,
     Fallback_Default => Basic_Types.AppImage);
