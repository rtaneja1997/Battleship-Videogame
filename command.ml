
type command = {instruction: string; ship_name: string; row_no: string;
                col_no: string; orientation: string; too_many_words:bool }

let parse str =
  let rec get lst i =
    match lst with
    | [] -> ""
    | h::t -> if i=0 then h else get t (i-1)

  in


  let string_list = String.split_on_char ' ' str in
  let blank_cmd =   {instruction = List.hd string_list;
                     ship_name = "" ;
                     row_no = "";
                     col_no = "";
                     orientation = "";
                     too_many_words = false;
                    }
  in

  let instr = String.lowercase_ascii (get string_list 0) in

  if List.length string_list = 1 then blank_cmd

  else if (instr="display" || instr="forfeit")
       then {blank_cmd with ship_name = List.nth string_list 1}


  else

    if instr = "attack" && List.length string_list <= 3 then
      {instruction = instr;
      ship_name = "";
      row_no = String.lowercase_ascii (get string_list 1);
      col_no = String.lowercase_ascii (get string_list 2);
      orientation = "";
      too_many_words = false;}

    else if instr = "attack" && List.length string_list > 3 then
      {instruction = instr;
      ship_name = "";
      row_no = String.lowercase_ascii (get string_list 1);
      col_no = String.lowercase_ascii (get string_list 2);
      orientation = "";
      too_many_words = true;}



    else if instr = "place" then

      if List.length string_list = 6 then
        {instruction = instr;
        ship_name = String.lowercase_ascii (get string_list 1);
        row_no = String.lowercase_ascii (get string_list 3);
        col_no = String.lowercase_ascii (get string_list 4);
        orientation = String.lowercase_ascii (get string_list 5);
        too_many_words = false;

        }
      else if List.length string_list > 6 then
      {instruction = instr;
        ship_name = String.lowercase_ascii (get string_list 1);
        row_no = String.lowercase_ascii (get string_list 3);
        col_no = String.lowercase_ascii (get string_list 4);
        orientation = String.lowercase_ascii (get string_list 5);
        too_many_words = true;

        }
      else

        {instruction = instr;
        ship_name = String.lowercase_ascii (get string_list 1);
        row_no = String.lowercase_ascii (get string_list 3);
        col_no = String.lowercase_ascii (get string_list 4);
        orientation = String.lowercase_ascii (get string_list 5);
        too_many_words = false;

        }


    else blank_cmd
