(* ast.ml - Abstract Syntax Tree definitions for the language *)

(** Binary operation types *)
type binary_op =
  | IAdd      (** Integer addition (+) *)
  | FAdd      (** Float addition (+.) *)
  | ISub      (** Integer subtraction (/-) *)
  | FSub      (** Float subtraction (-.) *)
  | IMul      (* Integer multiplication * *)
  | FMul      (** Float multiplication *. *)
  | IDiv      (** Integer division (/) *)
  | FDiv      (** Float division (/.) *)
  | IMod      (** Integer modulo (mod) *)
  | FMod      (** Float modulo (mod_float) *)
  | Equal     (** Equality (=) *)
  | NotEqual  (** Inequality (~=) *)
  | Less      (** Less than (<) *)
  | Greater   (** Greater than (>) *)
  | LessEq    (** Less than or equal (<=) *)
  | GreaterEq (** Greater than or equal (>=) *)
  | And       (** Logical and (&&) *)
  | Or        (** Logical or (||) *)
  | Xor       (** Logical xor (^) *)
  | Angle     (** Angle between vectors *)
  | Power     (** Power operator (pow) *)

(** Unary operation types *)
type unary_op =
  | INeg       (** Integer negation (-) *)
  | FNeg       (** Float negation (-.) *)
  | Not        (** Logical not (~) *)
  | Abs        (** Absolute value (abs) *)
  | Transpose  (** Matrix transpose (') *)
  | Det        (** Matrix determinant (det) *)
  | Dimension  (** Vector/Matrix dimension (dim) *)
  | Magnitude  (** Vector magnitude (mag) *)
  | I2F       (** Integer to float conversion *)

(** Expression types *)
type expr =
  | Empty                                   (** Empty expression *)
  | Var of string                           (** Variable reference *)
  | IntLit of int                           (** Integer literal *)
  | FloatLit of float                       (** Float literal *)
  | BoolLit of bool                         (** Boolean literal *)
  | StringLit of string                     (** String literal *)
  | IVectorLit of int * int list            (** Integer vector (size, elements) *)
  | FVectorLit of int * float list          (** Float vector (size, elements) *)
  | IMatrixLit of int * int * int list list (** Integer matrix (rows, cols, elements) *)
  | FMatrixLit of int * int * float list list (** Float matrix (rows, cols, elements) *)
  | BinOp of expr * binary_op * expr        (** Binary operation *)
  | UnOp of unary_op * expr                 (** Unary operation *)
  | VectorIndex of expr * expr              (** Vector indexing (vector[index]) *)
  | MatrixIndex of expr * expr * expr       (** Matrix indexing (matrix[row,col]) *)
  | RowAccess of expr * expr                (** Row access (matrix,row]) *)
  
(** Statement types *)
type stmt =
  | ExprStmt of expr                        (** Expression statement *)
  | DeclareStmt of string * expr            (** Variable declaration with let (x := expr) *)
  | AssignStmt of string * expr             (** Assignment (x := expr) *)
  | IfStmt of expr * stmt list * stmt list  (** If statement (condition, then-branch, else-branch) *)
  | ForStmt of string * expr * expr * stmt list (** For loop (counter, start, end, body) *)
  | WhileStmt of expr * stmt list           (** While loop (condition, body) *)
  | InputStmt of expr                       (** Input into variable *)
  | PrintStmt of expr                       (** Print expression *)

(** Program type - the top level structure *)
type program = Program of stmt list         (** List of statements *)

(* Raw AST printer *)
let rec string_of_expr expr =
  match expr with
  | Empty -> ""
  | Var name -> "Var \"" ^ name ^ "\""
  | IntLit i -> "IntLit " ^ string_of_int i
  | FloatLit f -> "FloatLit " ^ string_of_float f
  | BoolLit b -> "BoolLit " ^ string_of_bool b
  | StringLit s -> "StringLit \"" ^ s ^ "\""
  | IVectorLit (size, elems) ->
      "IVectorLit (" ^ string_of_int size ^ ", " ^ 
      "[" ^ String.concat "; " (List.map string_of_int elems) ^ "])"
  | FVectorLit (size, elems) ->
      "FVectorLit (" ^ string_of_int size ^ ", " ^ 
      "[" ^ String.concat "; " (List.map string_of_float elems) ^ "])"
  | IMatrixLit (rows, cols, data) ->
      "IMatrixLit (" ^ string_of_int rows ^ ", " ^ 
      string_of_int cols ^ ", " ^ 
      "[" ^ String.concat "; " 
        (List.map (fun row -> "[" ^ String.concat "; " 
          (List.map string_of_int row) ^ "]") data) ^ "])"
  | FMatrixLit (rows, cols, data) ->
      "FMatrixLit (" ^ string_of_int rows ^ ", " ^ 
      string_of_int cols ^ ", " ^ 
      "[" ^ String.concat "; " 
        (List.map (fun row -> "[" ^ String.concat "; " 
          (List.map string_of_float row) ^ "]") data) ^ "])"
  | BinOp (left, op, right) ->
      "BinOp (" ^ string_of_expr left ^ ", " ^ 
      (match op with
        | IAdd -> "IAdd" | FAdd -> "FAdd" | ISub -> "ISub" | FSub -> "FSub"
        | IMul -> "IMul" | FMul -> "FMul" | IDiv -> "IDiv" | FDiv -> "FDiv"
        | IMod -> "IMod" | FMod -> "FMod" | Equal -> "Equal" | NotEqual -> "NotEqual"
        | Less -> "Less" | Greater -> "Greater" | LessEq -> "LessEq" | GreaterEq -> "GreaterEq"
        | And -> "And" | Or -> "Or" | Xor -> "Xor" | Angle -> "Angle" | Power -> "Power")  ^ 
      ", " ^ string_of_expr right ^ ")"
  | UnOp (op, expr) ->
      "UnOp (" ^ 
      (match op with
        | INeg -> "INeg" | FNeg -> "FNeg" | Not -> "Not" | Abs -> "Abs"
        | Transpose -> "Transpose" | Det -> "Det" | Dimension -> "Dimension" | Magnitude -> "Magnitude" | I2F -> "I2F") ^ 
      ", " ^ string_of_expr expr ^ ")"
  | VectorIndex (vec, idx) ->
      "VectorIndex (" ^ string_of_expr vec ^ ", " ^ string_of_expr idx ^ ")"
  | MatrixIndex (mat, row, col) ->
      "MatrixIndex (" ^ string_of_expr mat ^ ", " ^ 
      string_of_expr row ^ ", " ^ string_of_expr col ^ ")"
  | RowAccess (mat, row) ->
      "RowAccess (" ^ string_of_expr mat ^ ", " ^ string_of_expr row ^ ")"

let rec string_of_stmt stmt =
  match stmt with
  | ExprStmt expr -> "ExprStmt (" ^ string_of_expr expr ^ ")"
  | DeclareStmt (var, expr) ->
      "DeclareStmt (\"" ^ var ^ "\", " ^ string_of_expr expr ^ ")"
  | AssignStmt (var, expr) ->
      "AssignStmt (\"" ^ var ^ "\", " ^ string_of_expr expr ^ ")"
  | IfStmt (cond, then_stmts, []) ->
      "IfStmt {" ^ string_of_expr cond ^ ", " ^ 
      "[" ^ String.concat "; " (List.map string_of_stmt then_stmts) ^ "], []}"
  | IfStmt (cond, then_stmts, else_stmts) ->
      "IfStmt {" ^ string_of_expr cond ^ ", " ^ 
      "[" ^ String.concat "; " (List.map string_of_stmt then_stmts) ^ "], " ^ 
      "[" ^ String.concat "; " (List.map string_of_stmt else_stmts) ^ "]}"
  | ForStmt (var, start, finish, body) ->
      "ForStmt (\"" ^ var ^ "\", " ^ string_of_expr start ^ ", " ^ 
      string_of_expr finish ^ ", " ^ 
      "[" ^ String.concat "; " (List.map string_of_stmt body) ^ "])"
  | WhileStmt (cond, body) ->
      "WhileStmt (" ^ string_of_expr cond ^ ", " ^ 
      "[" ^ String.concat "; " (List.map string_of_stmt body) ^ "])"
  | InputStmt expr -> "InputStmt (" ^ string_of_expr expr ^ ")"
  | PrintStmt expr -> "PrintStmt (" ^ string_of_expr expr ^ ")"


let string_of_program (Program stmts) =
  "Program [" ^ String.concat "; " (List.map string_of_stmt stmts) ^ "]"