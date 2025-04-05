(* main.ml - Read input file, parse it, type check it, and execute it *)
open Ast
open Lexer
open Parser
open Ast_type_checker
open Interpreter  (* Import the interpreter module *)

(* Check that required modules are available *)
let () =
  try ignore (Hashtbl.hash 0) with Not_found -> 
    failwith "Required Str module not available. Install with 'opam install str'"


(* Add a reference to track line numbers for error reporting *)
let line_num = ref 1

(* New function to tokenize a file directly *)
let tokenize_file filename =
  let in_channel = open_in filename in
  let lexbuf = Lexing.from_channel in_channel in
  line_num := 1;  (* Reset line counter *)
  
  (* Set position info including filename *)
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with 
                              Lexing.pos_fname = filename };
  
  let rec get_all_tokens tokens =
    try
      (match Lexer.token lexbuf with
      | Parser.EOF ->
          close_in in_channel;
          List.rev (Parser.EOF :: tokens)
      | t -> 
          get_all_tokens (t :: tokens)) [@warning "-fragile-match"]
    with
    | Lexer.Lexical_error msg ->
        close_in in_channel;
        let pos = lexbuf.Lexing.lex_curr_p in
        failwith (Printf.sprintf "Lexical error at line %d, column %d: %s" 
                 pos.Lexing.pos_lnum 
                 (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
                 msg)
  in
  
  try
    let result = get_all_tokens [] in
    close_in in_channel;
    result
  with e ->
    close_in in_channel;
    raise e

(* Add tokenize_string function for string inputs *)
let tokenize_string str =
  let lexbuf = Lexing.from_string str in
  line_num := 1;  (* Reset line counter *)
  
  (* Set position info *)
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with 
                              Lexing.pos_lnum = 1;
                              Lexing.pos_bol = 0 };
  
  let rec get_all_tokens tokens =
    try
      (match Lexer.token lexbuf with
      | Parser.EOF -> List.rev (Parser.EOF :: tokens)
      | t -> get_all_tokens (t :: tokens))[@warning "-fragile-match"]
    with
    | Lexer.Lexical_error msg ->
        let pos = lexbuf.Lexing.lex_curr_p in
        failwith (Printf.sprintf "Lexical error at line %d, column %d: %s" 
                 pos.Lexing.pos_lnum 
                 (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
                 msg)
  in
  get_all_tokens []


(* Enhanced parse_tokens function with better error reporting *)
let parse_tokens tokens =
  (* Create a dummy lexer function that provides tokens from our list *)
  let token_idx = ref 0 in
  let token_list = Array.of_list tokens in
  
  (* Helper function to convert a token to string for error reporting *)
  let token_to_string token =
    (match token with
    | Parser.INT_LITERAL i -> Printf.sprintf "INT(%d)" i
    | Parser.FLOAT_LITERAL f -> Printf.sprintf "FLOAT(%f)" f
    | Parser.ID s -> Printf.sprintf "IDENTIFIER(%s)" s
    | Parser.STRING_LITERAL s -> Printf.sprintf "STRING(%s)" s
    | Parser.BOOL_LITERAL b -> Printf.sprintf "BOOL(%b)" b
    | Parser.LET -> "LET"
    | Parser.IF -> "IF"
    | Parser.THEN -> "THEN"
    | Parser.ELSE -> "ELSE"
    | Parser.END -> "END"
    | Parser.WHILE -> "WHILE"
    | Parser.DO -> "DO"
    | Parser.FOR -> "FOR"
    | Parser.TO -> "TO"
    | Parser.SEMICOLON -> ";"
    | Parser.COMMA -> ","
    | Parser.LBRACE -> "{"
    | Parser.RBRACE -> "}"
    | Parser.LPAREN -> "("
    | Parser.RPAREN -> ")"
    | Parser.LBRACKET -> "["
    | Parser.RBRACKET -> "]"
    | Parser.ASSIGN -> ":="
    | Parser.EOF -> "EOF"
    | _ -> "UNKNOWN_TOKEN") [@warning "-fragile-match"]
  in
  
  let next_token _ =
    let t = token_list.(!token_idx) in
    if !token_idx < Array.length token_list - 1 then
      incr token_idx;
    t
  in
  
  (* Create a dummy lexbuf *)
  let lexbuf = Lexing.from_string "" in
  
  (* Use our token provider with the parser *)
  try
    Parser.program next_token lexbuf
  with
  | Parsing.Parse_error ->
      let error_pos = !token_idx in
      let error_token = if error_pos < Array.length token_list then 
                          token_list.(error_pos) 
                        else 
                          token_list.(Array.length token_list - 1) in
      
      (* Get context around the error (tokens before and after) *)
      let context_start = max 0 (error_pos - 3) in
      let context_end = min (Array.length token_list - 1) (error_pos + 3) in
      
      let context = Buffer.create 100 in
      Buffer.add_string context "Token sequence: ";
      for i = context_start to context_end do
        let token_str = token_to_string token_list.(i) in
        if i = error_pos then
          Buffer.add_string context (Printf.sprintf "[ERROR->%s<-] " token_str)
        else
          Buffer.add_string context (Printf.sprintf "%s " token_str);
      done;
      
      failwith (Printf.sprintf "Parse error at token #%d: %s\n%s" 
                error_pos 
                (token_to_string error_token)
                (Buffer.contents context))
                
                
(* Update parse_string to use tokenize_string *)
let parse_string str =
  try
    let tokens = tokenize_string str in
    parse_tokens tokens
  with
  | Failure msg -> failwith msg
  | e -> failwith (Printexc.to_string e)

(* Update parse_file to use tokenize_file *)
let parse_file filename =
  try
    let tokens = tokenize_file filename in
    parse_tokens tokens
  with
  | Failure msg -> failwith msg
  | e -> 
      let msg = Printexc.to_string e in
      failwith msg

(* Helper function to determine if a message is an error *)
let is_error_message msg =
  String.length msg >= 10 && String.sub msg 0 10 = "Type error" ||
  String.length msg >= 6 && String.sub msg 0 6 = "Error:"

(* Parse, type check, and interpret a string, then print results *)
let parse_type_check_and_interpret str =
  try
    let ast = parse_string str in
    let ast_string = string_of_program ast in
    
    Printf.printf "Input:\n%s\n\n" str;
    Printf.printf "AST:\n%s\n\n" ast_string;
    
    (* Type check the AST *)
    let result = type_check ast in
    if is_error_message(result) then
      Printf.printf "Type Check: ❌ %s\n\n" result
    else begin
      Printf.printf "Type Check: ✅ %s\n\n" result;
      (* If type check passes, interpret the program *)
      Printf.printf "Executing program:\n";
      Interpreter.interpret ast;
      Printf.printf "\nExecution complete.\n\n"
    end
  with
  | Failure msg -> Printf.printf "Error: %s\n\n" msg

(* Parse, type check a string, then print results *)
let parse_type_check_and_print str =
  try
    let ast = parse_string str in
    let ast_string = string_of_program ast in
    
    Printf.printf "Input:\n%s\n\n" str;
    Printf.printf "AST:\n%s\n\n" ast_string;
    
    (* Type check the AST *)
    let result = type_check ast in
    if is_error_message(result) then
      Printf.printf "Type Check: ❌ %s\n\n" result
    else
      Printf.printf "Type Check: ✅ %s\n\n" result
  with
  | Failure msg -> Printf.printf "Error parsing: %s\n\n" msg

(* Parse a string, convert to AST representation, and print it *)
let parse_and_print str =
  try
    let ast = parse_string str in
    let ast_string = string_of_program ast in
    Printf.printf "Input:\n%s\n\nAST:\n%s\n\n" str ast_string
  with
  | Failure msg -> Printf.printf "Error parsing: %s\n\n" msg

(* Parse and type check a source file *)
let parse_and_check_file filename =
  let ast = parse_file filename in
  let result = type_check ast in
  if is_error_message(result) then begin
    Printf.printf "Type Check: ❌ %s\n" result;
    exit 1
  end else begin
    Printf.printf "Type Check: ✅ %s\n" result;
    ast
  end

(* Parse, type check, and interpret a source file *)
let parse_check_and_interpret_file filename =
  let ast = parse_file filename in
  let result = type_check ast in
  if is_error_message(result) then begin
    Printf.printf "Type Check: ❌ %s\n" result;
    exit 1
  end else begin
    Printf.printf "Type Check: ✅ %s\n" result;
    Printf.printf "Executing program:\n";
    Interpreter.interpret ast;
    Printf.printf "\nExecution complete.\n";
  end

(* Run test cases *)
let run_tests ~with_type_check ~with_interpret =
  print_endline "Running test cases...\n";
  
  let print_func = 
    if with_interpret then parse_type_check_and_interpret 
    else if with_type_check then parse_type_check_and_print 
    else parse_and_print 
  in
  
  (* Simple statements *)
  print_func "let x := 10;";
  
  (* Arithmetic operations *)
  print_func "let y := 5 + 10;";
  print_func "let z := 20 / 5;";
  
  (* Conditional statement *)
  print_func "let x := 15;\n if x < 10 then { Print(x); } else { Print(0); } end";
  
  (* For loop *)
  print_func "let sum := 0; for i := 1 to 10 do { sum := sum + i; } end";
  
  (* Vector operations *)
  print_func "let v := 3\n[6,7,8];\nlet v1 := v[0];\n Print(v1);";
  
  (* Function calls *)
  print_func "let v:=1,1\n[[1]];\nlet dim_v := dim(v);";
  
  (* Multiple statements *)
  print_func "let x := 5; let y := 10; let z := x + y;";

  (* Comment parse *)
  print_func "// This is a comment";
  print_func "/* This is a comment  Another comment  x:= 10;*/ \"Check this\";";
  
  (* Custom inputs *)
  print_func "let v:=3\n[1,2,3]\n;\nPrint(v);\nlet elem := v[1];\nPrint(elem);";
  print_func "let x:= 58;\nif x < 20 then \n { Print(x); } \n else { Print(); } \n end";
  print_func "let x:= 10;\nif x < 20 then \n if x < 5 then \n { Print(x); } \n else Print(0); \n end \n else { Print(0); } \n end";
  print_func "Print(0);";
  print_func "Print(\"WorkingPeople\");";
  print_func "let x := 10 + 7 / 2;";
  print_func "let x := 78;\nlet sum := 0;\nlet j :=0;\nfor i := x to j do \n { sum := sum + i; \n x := 10 + 19;\n Print(sum); } \n end";
  print_func "/-1;";
  print_func "let x := -10.9;";
  print_func "let m:=10;\nlet x := -1*m;";
  print_func "Print();";
  print_func "let V := 1,1\n[[1]];\n Print(row_access(V,0));";
  (*  Testcases Completed  *)
  print_endline "Test cases completed."

(* Test cases specifically for the type checker *)
let run_type_check_tests () =
  print_endline "Running type checker test cases...\n";
  
  (* Helper function for running type check tests *)
  let test_type_check code expected_result =
    try
      let ast = parse_string code in
      let result = type_check ast in
      let success = (result = expected_result) || 
                    (expected_result = "error" && is_error_message(result)) in
      Printf.printf "Test: %s\nResult: %s\nExpected: %s\nStatus: %s\n\n"
        (if String.length code > 50 then String.sub code 0 47 ^ "..." else code)
        result
        (if expected_result = "error" then "Error expected" else expected_result)
        (if success then "✅ PASS" else "❌ FAIL")
    with
    | Failure msg -> 
        let success = expected_result = "error" in
        Printf.printf "Test: %s\nResult: Error: %s\nExpected: %s\nStatus: %s\n\n"
          (if String.length code > 50 then String.sub code 0 47 ^ "..." else code)
          msg
          (if expected_result = "error" then "Error expected" else expected_result)
          (if success then "✅ PASS" else "❌ FAIL")
  in
  
  (* Basic declarations and type consistency *)
  test_type_check "let x := 5;" "Program type checks successfully";
  test_type_check "let x := 5; x := 10;" "Program type checks successfully";
    test_type_check "let x := 5; x := 3.14;" "error"; (* Type mismatch *)
  
  (* Variable redeclaration checks *)
  test_type_check "let x := 5; let x := 10;" "error"; (* Redeclaration not allowed *)
  test_type_check "let x := true; let y := 10;" "Program type checks successfully";
  
  (* Loop iterator variable tests *)
  test_type_check "for i := 1 to 5 do { let x := i; } end" "Program type checks successfully";
  test_type_check "let i := 10; for i := 1 to 5 do { Print(i); } end" "error"; (* Iterator shadows existing variable *)
  test_type_check "for i := 1 to 5 do {} end let i := 10;" "Program type checks successfully"; (* i no longer in scope *)
  
  (* Nested scopes *)
  test_type_check "if true then { let x := 5; } else { let y := 10; } end" "Program type checks successfully";
  test_type_check "let x := 5; if x > 0 then { let y := 10; } end Print(y);" "error"; (* y not in scope *)
  
  (* Type checking for operations *)
  test_type_check "let x := 5 + 10;" "Program type checks successfully";
  test_type_check "let x := 5 + 3.14;" "error"; (* Type mismatch in addition *)
  test_type_check "let v1 := 3\n[1,2,3]; let v2 := 3\n[4,5,6]; let v3 := v1 + v2;" "Program type checks successfully";
  test_type_check "let v1 := 3\n[1,2,3]; let v2 := 2\n[4,5]; let v3 := v1 + v2;" "error"; (* Vector dimension mismatch *)
  
  (* Matrix operations *)
  test_type_check "let m1 := 2,2\n[[1,2],[3,4]]; let m2 := 2,2\n[[5,6],[7,8]]; let m3 := m1 + m2;" "Program type checks successfully";
  test_type_check "let m1 := 2,2\n[[1,2],[3,4]]; let m2 := 3,2\n[[5,6],[7,8],[9,10]]; let m3 := m1 + m2;" "error"; (* Matrix dimension mismatch *)
  
  (* Mixed operations *)
  test_type_check "let x := 5; let y := true; let z := x + y;" "error"; (* Cannot add int and bool *)
  test_type_check "let x := 5; let y := true; if y then { let z := x + 10; } end" "Program type checks successfully";
  
  (* Let declaration and assignment semantics *)
  test_type_check "let x := 10; let y := x + 5;" "Program type checks successfully";
  test_type_check "x := 10;" "error"; (* Assignment to undeclared variable *)
  test_type_check "let x := 10; y := x + 5;" "error"; (* Assignment to undeclared variable *)
  
  (* Function application and type checking *)
  test_type_check "let v := 3\n[1,2,3]; let m := mag(v);" "Program type checks successfully";
  
  (* Vector/Matrix access *)
  test_type_check "let v := 3\n[1,2,3]; let x := v[0];" "Program type checks successfully";
  test_type_check "let v := 3\n[1,2,3]; let x := v[true];" "error"; (* Index must be int *)
  test_type_check "let m := 2,2\n[[1,2],[3,4]]; let x := m[0,1];" "Program type checks successfully";
  
  (* Additional type error test cases from test_type_errors.txt *)
  test_type_check "let int_var := 5; int_var := true;" "error"; (* Mismatched types in assignment *)
  test_type_check "let mix_types := 3 + true;" "error"; (* Mismatched types in binary operation *)
  test_type_check "let vec1 := 2\n[1,2]; let vec2 := 3\n[3,4,5]; let bad_add := vec1 + vec2;" "error"; (* Vector dimension mismatch *)
  test_type_check "let mat1 := 2,3\n[[1,2,3],[4,5,6]]; let mat2 := 2,2\n[[1,2],[3,4]]; let bad_mul := mat1 * mat2;" "error"; (* Matrix multiplication dimension mismatch *)
  test_type_check "let v := 3\n[1,2,3]; let bad_idx := v[true];" "error"; (* Indexing with non-integer *)
  test_type_check "let non_square := 2,3\n[[1,2,3],[4,5,6]]; let bad_det := det(non_square);" "error"; (* Determinant of non-square matrix *)
  test_type_check "if 5 then { Print(\"Error\"); } else { Print(\"OK\"); } end" "error"; (* If condition not a boolean *)
  test_type_check "let s := \"hello\"; let n := 10; if s = n then { Print(\"Impossible\"); } end" "error"; (* Incompatible types in comparison *)
  
  print_endline "Type checker test cases completed."

(* Helper function to check if a substring exists in a string *)
let string_contains s1 s2 =
  try
    let _ = Str.search_forward (Str.regexp_string s2) s1 0 in
    true
  with Not_found ->
    false

(* Test cases specifically for runtime errors *)
let run_runtime_tests () =
  print_endline "Running runtime error test cases...\n";
  
  (* Helper function for running runtime tests *)
  let test_runtime code =
    try
      let ast = parse_string code in
      (* First type check the AST *)
      let type_check_result = type_check ast in
      if is_error_message(type_check_result) then
        Printf.printf "Test: %s\nResult: Type error: %s\nStatus: ❌ SKIPPED (Type error)\n\n"
          (if String.length code > 50 then String.sub code 0 47 ^ "..." else code)
          type_check_result
      else begin
        (* If type check passes, try to interpret *)
        try
          (* Run the program *)
          let _ = Interpreter.interpret ast in
          
          (* Program executed without error *)
          Printf.printf "Test: %s\nResult: No runtime error occurred\nStatus: ❌ FAIL\n\n"
            (if String.length code > 50 then String.sub code 0 47 ^ "..." else code)
        with
        | Interpreter.Runtime_error msg ->
            Printf.printf "Test: %s\nRuntime Error: %s\nStatus: ✅ PASS\n\n"
              (if String.length code > 50 then String.sub code 0 47 ^ "..." else code)
              msg
        | e ->
            Printf.printf "Test: %s\nUnexpected error: %s\nStatus: ⚠️ UNEXPECTED\n\n"
              (if String.length code > 50 then String.sub code 0 47 ^ "..." else code)
              (Printexc.to_string e)
      end
    with
    | Failure msg -> 
        Printf.printf "Test: %s\nParse/lex error: %s\nStatus: ❌ SKIPPED (Parse error)\n\n"
          (if String.length code > 50 then String.sub code 0 47 ^ "..." else code)
          msg
  in
  
  (* Division by zero *)
  test_runtime "let division_by_zero := 10 / 0;";
  
  (* Vector index out of bounds *)
  test_runtime "let v := 2\n[10, 20];\nlet out_of_bounds := v[5];";
  
  (* Matrix access out of bounds *)
  test_runtime "let m := 2,2\n[[1,2],[3,4]];\nlet mat_out_of_bounds := m[3, 1];";
  
  (* Modulo by zero *)
  test_runtime "let mod_zero := 10 mod 0;";
  
  (* Vector dimension mismatch in dot product *)
  test_runtime "let v1 := 2\n[1, 2];\nlet v2 := 3\n[3, 4, 5];\nlet bad_dot := v1 * v2;";
  
  (* Nested if with bad condition *)
  test_runtime "if true then {\n  if 10 then {\n    Print(\"Bad nested condition\");\n  } end\n} end";
  
  (* Float division by zero *)
  test_runtime "let float_div_zero := 10.5 /. 0.0;";
  
  (* Matrix with incorrect dimensions *)
  test_runtime "let bad_matrix := 3,2\n[[1,2],[3,4]];";
  
  (* Access an undefined variable *)
  test_runtime "Print(undefined_variable);";
  
  (* Row access out of bounds *)
  test_runtime "let m := 2,2\n[[1,2],[3,4]];\nlet row := row_access(m, 5);";
  
  print_endline "Runtime error test cases completed."

(* Enhanced function to tokenize and parse a file, with better error reporting *)
let tokenize_parse_and_print_file filename =
  Printf.printf "Processing file: %s\n" filename;
  
  (* First read the file contents for error reporting *)
  let read_file_lines file =
    let ic = open_in file in
    let rec read_lines acc line_num =
      try
        let line = input_line ic in
        read_lines ((line_num, line) :: acc) (line_num + 1)
      with End_of_file ->
        close_in ic;
        List.rev acc
    in
    read_lines [] 1
  in
  
  
  try
    (* Tokenize the file *)
    let tokens = tokenize_file filename in
    Printf.printf "Lexing completed successfully.\n";
    
    (* Parse tokens into AST *)
    let ast = parse_tokens tokens in
    Printf.printf "Parsing completed successfully.\n";
    
    (* Display the AST *)
    Printf.printf "AST representation:\n%s\n" (string_of_program ast);
    Printf.printf "Processing complete.\n";
    ast
  with
  | Lexer.Lexical_error msg ->
      let file_lines = read_file_lines filename in
      let pos_info = !line_num in
      let line_content = try List.assoc pos_info file_lines with Not_found -> "<line not available>" in
      Printf.printf "Lexical error at line %d:\n%s\nError: %s\n" 
        pos_info line_content msg;
      exit 1
  | Failure msg when String.length msg >= 11 && String.sub msg 0 11 = "Parse error" ->
      Printf.printf "Parser error: %s\n" msg;
      exit 1
  | e -> 
      Printf.printf "Error during processing: %s\n" (Printexc.to_string e);
      exit 1

(* Main function *)
let () =
  let process_file = ref true in
  let run_test_cases = ref false in
  let with_type_check = ref false in
  let type_check_only = ref false in
  let interpret = ref false in
  let run_type_tests = ref false in  (* New flag for running type checker tests *)
  let list_tests = ref false in
  let run_test_file = ref None in
  let tokenize_only = ref false in  (* New flag for tokenization only *)
  let run_runtime_tests_flag = ref false in  (* Add runtime error tests option *)
  
  Arg.parse [
    ("-test", Arg.Set run_test_cases, "Run built-in test cases");
    ("-nofile", Arg.Clear process_file, "Don't process input.txt");
    ("-typecheck", Arg.Set with_type_check, "Enable type checking for tests");
    ("-check-only", Arg.Set type_check_only, "Only perform type checking on input file");
    ("-interpret", Arg.Set interpret, "Interpret the program after type checking");
    ("-test-type", Arg.Set run_type_tests, "Run type checker specific tests");  (* New option *)
    ("-test-runtime", Arg.Set run_runtime_tests_flag, "Run runtime error tests");
    ("-list-tests", Arg.Set list_tests, "List all .txt test files in the directory");
    ("-run-test", Arg.String (fun s -> run_test_file := Some s), "Run specific test file (provide filename)");
    ("-tokenize", Arg.Set tokenize_only, "Only tokenize the input file");  (* New option *)
  ] (fun _ -> ()) "Matrix/Vector Language Parser and Interpreter";
  
  (* New functionality: If -list-tests is provided, list all .txt files and exit *)
  if !list_tests then (
    Printf.printf "Available .txt test files in this directory:\n";
    Array.iter (fun file -> if Filename.check_suffix file ".txt" then Printf.printf "%s\n" file) (Sys.readdir ".");
    exit 0
  );
  
    
  (* Run type checker tests if requested *)
  if !run_type_tests then run_type_check_tests();
  
  (* Run runtime error tests if requested *)
  if !run_runtime_tests_flag then run_runtime_tests();
  
  (* Update the tokenize-only mode to use the new function *)
  if !tokenize_only then
    (match !run_test_file with
    | Some f -> 
        Printf.printf "Running tokenizer file: %s\n" f;
        let _ = tokenize_parse_and_print_file f in
        exit 0
    | None ->
        let input_file = "input.txt" in
        let _ = tokenize_parse_and_print_file input_file in
        exit 0);

  (* New functionality: If -run-test <filename> is provided then process that file *)
  (match !run_test_file with
  | Some f ->
      Printf.printf "Running test file: %s\n" f;
      (try
        let tokens = tokenize_file f in
        Printf.printf "Lexing completed successfully.\n";
        
        let ast = parse_tokens tokens in
        Printf.printf "Parsing completed successfully.\n";
        
        if !interpret then begin
          let result = type_check ast in
          if is_error_message(result) then begin
            Printf.printf "Type Check: ❌ %s\n" result;
            exit 1
          end else begin
            Printf.printf "Type Check: ✅ %s\n" result;
            Printf.printf "Executing program:\n";
            Interpreter.interpret ast;
            Printf.printf "\nExecution complete.\n";
          end
        end else if !type_check_only || !with_type_check then begin
          let result = type_check ast in
          if is_error_message(result) then begin
            Printf.printf "Type Check: ❌ %s\n" result;
            exit 1
          end else begin
            Printf.printf "Type Check: ✅ %s\n" result;
            let ast_string = string_of_program ast in
            Printf.printf "AST representation:\n%s\n" ast_string;
          end
        end else begin
          let ast_string = string_of_program ast in
          Printf.printf "AST representation:\n%s\n" ast_string;
        end;
        exit 0
      with
      | Failure msg ->
          Printf.printf "Error: %s\n" msg;
          exit 1
      | e ->
          Printf.printf "Error: %s\n" (Printexc.to_string e);
          exit 1)
  | None -> ());

  if !run_test_cases then run_tests ~with_type_check:!with_type_check ~with_interpret:!interpret;
  
  if !process_file then
    try
      (* Parse input file *)
      let input_file = "input.txt" in
      let output_file = "output.txt" in
      
      Printf.printf "Parsing file: %s\n" input_file;
      
      let tokens = tokenize_file input_file in
      Printf.printf "Lexing completed successfully.\n";
      
      let ast = parse_tokens tokens in
      Printf.printf "Parsing completed successfully.\n";
      
      if !interpret then begin
        let result = type_check ast in
        if is_error_message(result) then begin
          Printf.printf "Type Check: ❌ %s\n" result;
          exit 1
        end else begin
          Printf.printf "Type Check: ✅ %s\n" result;
          Printf.printf "Executing program:\n";
          Interpreter.interpret ast;
          Printf.printf "\nExecution complete.\n";
        end
      end else if !type_check_only || !with_type_check then begin
        let result = type_check ast in
        if is_error_message(result) then begin
          Printf.printf "Type Check: ❌ %s\n" result;
          exit 1
        end else begin
          Printf.printf "Type Check: ✅ %s\n" result;
          if not !type_check_only then begin
            (* Convert AST to string representation *)
            Printf.printf "Converting AST to string representation\n";
            let ast_string = string_of_program ast in
            
            (* Write AST to output file *)
            Printf.printf "Writing AST to: %s\n" output_file;
            let oc = open_out output_file in
            output_string oc ast_string;
            close_out oc;
            
            print_endline "Processing complete. AST written to output.txt"
          end else
            print_endline "Type checking complete."
        end
      end else begin
        (* Convert AST to string representation *)
        Printf.printf "Converting AST to string representation\n";
        let ast_string = string_of_program ast in
        
        (* Write AST to output file *)
        Printf.printf "Writing AST to: %s\n" output_file;
        let oc = open_out output_file in
        output_string oc ast_string;
        close_out oc;
        
        print_endline "Processing complete. AST written to output.txt"
      end
    with
    | Sys_error msg ->
        prerr_endline ("System error: " ^ msg);
        exit 1
    | Failure msg ->
        prerr_endline ("Error: " ^ msg);
        exit 1
    | e ->
        prerr_endline ("Unexpected error: " ^ Printexc.to_string e);
        exit 1