(*
==========
LIST-UTILS
==========
*)
type table = (Types.id * int) list;;

let update table id num =
  (id, num)::table;;

let rec look_up table id =
  match table with
  | [] -> 0
  | (identifier, value)::tail -> (
    if (identifier = id)
      then value
      else look_up tail id
  );;

(*
  ===========
  PRINT-UTILS
  ===========
*)

let print_bin_op operator =
  match operator with
      | Types.Plus -> print_string "+"
      | Types.Minus -> print_string "-"
      | Types.Times -> print_string "x"
      | Types.Division -> print_string "/"

let rec print_expression expression table =
  match expression with
    | Types.EseqExpression(_, exp) -> print_expression exp table
    | Types.IdentifierExpression(id) -> print_string (Int.to_string (look_up table id))
    | Types.NumberExpression(num) -> print_string (Int.to_string num)
    | Types.OperationExpression(left, operator, right) -> (
      print_string "Ope :";
      print_expression left table;
      print_bin_op operator;
      print_expression right table;
      print_endline "";
    );;

(*
  ===========
  INTERPRETER
  ===========
*)
let interpret head_statement = 
  let rec interpret_statement statement table =
    match statement with
    | Types.CompoundStatement(stm_A, stm_B) -> interpret_statement stm_B (interpret_statement stm_A table)
    | Types.AssignementStatement(id, exp) -> (
      let (value, new_table) = interpret_expression exp table in
      update new_table id value;
    )
    | Types.PrintStatement(exp_list) -> (
      match exp_list with
      | [] -> table
      | exp::rest -> (
        print_expression exp table;

        let (_, new_table) = interpret_expression exp table in
        interpret_statement (Types.PrintStatement rest) new_table;
      )
    )
  and interpret_expression expression table =
    match expression with
    | Types.EseqExpression(stm, exp) -> (
      let new_table = interpret_statement stm table in
      interpret_expression exp new_table;
    )
    | Types.IdentifierExpression(id) -> (
      let value = look_up table id in
      (value, table)
    )
    | Types.NumberExpression(num) -> (num, table)
    | Types.OperationExpression(left, operator, right) -> (
      let (left_value, t1) = interpret_expression left table in
      let (right_value, t2) = interpret_expression right t1 in
      match operator with
      | Types.Plus -> left_value + right_value, t2
      | Types.Minus -> left_value - right_value, t2
      | Types.Times -> left_value * right_value, t2
      | Types.Division -> left_value / right_value, t2
    )
  in
  (* run the program *)
  let empty_table = [] in
  interpret_statement head_statement empty_table;

(* TESTS *)