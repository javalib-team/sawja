type cfa_options = 
    {
      cfa_clinit_as_entry: bool;
      cfa_debug : int;
      cfa_html_dump : string option;
    }
val default_opt: cfa_options
