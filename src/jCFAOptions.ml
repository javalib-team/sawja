type cfa_options = {
  cfa_clinit_as_entry: bool;
  cfa_debug: int;
  cfa_html_dump : string option;
}
let default_opt = 
  {
    cfa_clinit_as_entry = true;
    cfa_debug = 0;
    cfa_html_dump = None;
  }
