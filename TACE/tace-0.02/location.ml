
open Format;;

type location =
{ 
  mutable file : string; 
  mutable start_line : int;
  mutable end_line : int;  
  mutable start_char : int;
  mutable end_char : int 
};;


let spawn startloc endloc =
  { file = startloc.file;
    start_line = startloc.start_line;
    start_char = startloc.start_char;
    end_line = endloc.end_line;
    end_char = endloc.end_char
  };;

let default_location =
  { 
    file = "";
    start_line = -1;
    start_char = -1;
    end_line = -1;
    end_char = -1
  };;


let print_location loc =
  begin
    open_box 0;
    print_string "file:";
    print_space ();
    print_string loc.file;
    print_space ();
    if ( loc.start_line = loc.end_line )
    then 
      begin
        print_string "line";
        print_space ();
        print_int loc.start_line;
        print_string ",";
        print_space ();
        if ( loc.start_char = loc.end_char )
        then
          begin
            print_string "col";
            print_space ();
            print_int loc.start_char;
          end
        else
          begin
            print_string "cols";
            print_space ();
            print_int loc.start_char;            
            print_string "-";
            print_int loc.end_char;    
          end
      end
    else
      begin
        print_string "line";
        print_space ();
        print_int loc.start_line;
        print_string ",";
        print_space (); 
        print_string "col";
        print_space ();
        print_int loc.start_char;
        print_space ();       
        print_string "to";
        print_space ();       
        print_string "line";
        print_space ();
        print_int loc.end_line;
        print_string ",";
        print_space (); 
        print_string "col";
        print_space ();
        print_int loc.end_char;
      end
  end
;;


let to_string loc =
  let sfile = 
    (if loc.file = ""
     then ""
     else sprintf "File \"%s\"," loc.file) in
    if loc.start_line = loc.end_line  
    then 
      sprintf "@[%s line %i, %s@]"
	sfile
	loc.start_line
	(if loc.start_char = loc.end_char
	 then sprintf "character %i" loc.start_char
	 else sprintf "characters %i-%i" loc.start_char loc.end_char)
    else 
      sprintf "@[File@ \"%s\"@],@ @[line %i, character %i@]@ -@ @[line %i, character %i@]"
	loc.file loc.start_line loc.start_char loc.end_line loc.end_char;;


