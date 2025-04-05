open Ast
open Ast_type_checker

(* Runtime value types *)
type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VString of string
  | VIVector of int * int list      (* Changed from array to list *)
  | VFVector of int * float list    (* Changed from array to list *)
  | VIMatrix of int * int * int list list    (* Changed from array array to list list *)
  | VFMatrix of int * int * float list list  (* Changed from array array to list list *)
  | VUnit

(* Exception for runtime errors *)
exception Runtime_error of string

(* Environment for variable values *)
module StringMap = Map.Make(String)
type runtime_env = {
  values: value StringMap.t;
  declared: bool StringMap.t;  (* Track whether a variable has been declared *)
}

(* Empty environment *)
let empty_runtime_env = {
  values = StringMap.empty;
  declared = StringMap.empty;
}

(* Environment operations *)
let lookup_var (env : runtime_env) name =
  match StringMap.find_opt name env.values with
  | Some v -> v
  | None -> raise (Runtime_error ("Undefined variable: " ^ name))

let declare_var (env : runtime_env) name value =
  if StringMap.mem name ((env : runtime_env).declared) then
    raise (Runtime_error ("Variable '" ^ name ^ "' is already declared. Re-declaration is not allowed."))
  else
    { values = StringMap.add name value env.values;
      declared = StringMap.add name true env.declared }

let update_var (env : runtime_env) name value =
  if StringMap.mem name ((env : runtime_env).declared) then
    { env with values = StringMap.add name value env.values }
  else
    raise (Runtime_error ("Cannot assign to undeclared variable: " ^ name))

(* New scope management functions *)

(* Create a new environment that inherits from a parent environment *)
let enter_scope env = {
  values = env.values;
  declared = env.declared;
}

(* Merge a child environment into its parent environment *)
(* Only variables that already exist in the parent are updated *)
let exit_scope (parent_env : runtime_env) child_env =
  (* Update values in parent that were modified in child scope *)
  let update_var var_name value (parent_env : runtime_env) =
    if StringMap.mem var_name ((parent_env : runtime_env).declared) then
      (* Variable exists in parent, update it *)
      { parent_env with values = StringMap.add var_name value parent_env.values }
    else
      (* Variable was declared in child scope, ignore it *)
      parent_env
  in
  
  (* Update all variables in parent that were modified in child *)
  StringMap.fold update_var child_env.values parent_env

(* Common helper functions for matrix and vector operations *)
(* Helper functions for common list operations *)
let rec map_with_acc f lst acc =
  match lst with
  | [] -> List.rev acc
  | x::xs -> map_with_acc f xs ((f x) :: acc)

let map f lst = map_with_acc f lst []

(* Helper functions for list operations with tail recursion *)
let rec fold_left f acc lst =
  match lst with
  | [] -> acc
  | x::xs -> fold_left f (f acc x) xs

let rec get_nth lst i =
  match lst with
  | [] -> raise (Runtime_error "Index out of bounds")
  | x::xs -> if i = 0 then x else get_nth xs (i-1)

let dot_product v1 v2 init_val op =
  let rec aux v1 v2 acc =
    match v1, v2 with
    | [], [] -> acc
    | x::xs, y::ys -> aux xs ys (op acc (op x y))
    | _, _ -> raise (Runtime_error "Vector dimension mismatch")
  in
  aux v1 v2 init_val

(* Module for vector operations *)
module VectorOps = struct
  let dot_product_int v1 v2 =
    let rec compute v1 v2 acc =
      match v1, v2 with
      | [], [] -> acc
      | x::xs, y::ys -> compute xs ys (acc + (x * y))
      | _, _ -> raise (Runtime_error "Vector dimension mismatch in dot product")
    in
    compute v1 v2 0

  let dot_product_float v1 v2 =
    let rec compute v1 v2 acc =
      match v1, v2 with
      | [], [] -> acc
      | x::xs, y::ys -> compute xs ys (acc +. (x *. y))
      | _, _ -> raise (Runtime_error "Vector dimension mismatch in dot product")
    in
    compute v1 v2 0.0

  let vector_zipWith f v1 v2 =
    let rec aux v1 v2 acc =
      match v1, v2 with
      | [], [] -> List.rev acc
      | x::xs, y::ys -> aux xs ys ((f x y)::acc)
      | _, _ -> raise (Runtime_error "Vector dimension mismatch")
    in
    aux v1 v2 []

  let scalar_mul lst scalar op = List.map (fun x -> op scalar x) lst

  let calculate_magnitude vec add_op mul_op sqrt_op zero =
    let sum_of_squares = fold_left (fun acc x -> add_op acc (mul_op x x)) zero vec in
    sqrt_op sum_of_squares

  let calculate_angle vec1 vec2 dot_product_fn magnitude_fn =
    let dot = dot_product_fn vec1 vec2 in
    let mag1 = magnitude_fn vec1 in
    let mag2 = magnitude_fn vec2 in
    let denominator = mag1 *. mag2 in
    let cos_theta = max (-1.0) (min 1.0 (dot /. denominator)) in
    acos cos_theta

  let magnitude_int_vector vec =
    calculate_magnitude vec (+) ( * ) (fun x -> sqrt(float_of_int x)) 0

  let magnitude_float_vector vec =
    calculate_magnitude vec (+.) ( *. ) sqrt 0.0

  let angle_int_vectors vec1 vec2 =
    calculate_angle vec1 vec2 (fun v1 v2 -> float_of_int (dot_product_int v1 v2)) magnitude_int_vector

  let angle_float_vectors vec1 vec2 =
    calculate_angle vec1 vec2 dot_product_float magnitude_float_vector

end

(* Module for matrix operations *)
module MatrixOps = struct
  let get_column mat j =
    List.map (fun row ->
      let rec get_element lst index =
        match lst with
        | [] -> raise (Runtime_error "Column index out of bounds")
        | x::xs -> if index = 0 then x else get_element xs (index-1)
      in
      get_element row j
    ) mat

  let get_row mat i =
    let rec find_row mat index =
      match mat with
      | [] -> raise (Runtime_error "Row index out of bounds")
      | x::xs -> if index = 0 then x else find_row xs (index-1)
    in
    find_row mat i

  let build_transpose mat c =
    let rec aux j acc =
      if j >= c then List.rev acc
      else aux (j+1) ((get_column mat j)::acc)
    in
    aux 0 []

  let multiply_int_matrices r1 c1 mat1 r2 c2 mat2 =
    if c1 <> r2 then
      raise (Runtime_error "Matrix multiplication dimension mismatch")
    else
      let rec compute_result_matrix i acc =
        if i = r1 then List.rev acc
        else
          let row_i = get_row mat1 i in
          let rec compute_row j row_acc =
            if j = c2 then List.rev row_acc
            else
              let col_j = get_column mat2 j in
              compute_row (j+1) ((VectorOps.dot_product_int row_i col_j)::row_acc)
          in
          let result_row = compute_row 0 [] in
          compute_result_matrix (i+1) (result_row::acc)
      in
      VIMatrix(r1, c2, compute_result_matrix 0 [])

  let multiply_float_matrices r1 c1 mat1 r2 c2 mat2 =
    if c1 <> r2 then
      raise (Runtime_error "Matrix multiplication dimension mismatch")
    else
      let rec compute_result_matrix i acc =
        if i = r1 then List.rev acc
        else
          let row_i = get_row mat1 i in
          let rec compute_row j row_acc =
            if j = c2 then List.rev row_acc
            else
              let col_j = get_column mat2 j in
              compute_row (j+1) ((VectorOps.dot_product_float row_i col_j)::row_acc)
          in
          let result_row = compute_row 0 [] in
          compute_result_matrix (i+1) (result_row::acc)
      in
      VFMatrix(r1, c2, compute_result_matrix 0 [])

  let multiply_matrix_vector_int r c mat vec n =
    if c <> n then
      raise (Runtime_error "Matrix-vector dimension mismatch")
    else
      let result = List.map (fun row -> [VectorOps.dot_product_int row vec]) mat in
      VIMatrix(r, 1, result)

  let multiply_matrix_vector_float r c mat vec n =
    if c <> n then
      raise (Runtime_error "Matrix-vector dimension mismatch")
    else
      let result = List.map (fun row -> [VectorOps.dot_product_float row vec]) mat in
      VFMatrix(r, 1, result)

  let multiply_vector_matrix_int n vec r c mat =
    if r <> 1 then
      raise (Runtime_error "Vector-matrix multiplication requires matrix with 1 row")
    else
      let mat_row = List.hd mat in
      let result = List.map (fun v -> List.map (fun m -> v * m) mat_row) vec in
      VIMatrix(n, c, result)

  let multiply_vector_matrix_float n vec r c mat =
    if r <> 1 then
      raise (Runtime_error "Vector-matrix multiplication requires matrix with 1 row")
    else
      let mat_row = List.hd mat in
      let result = List.map (fun v -> List.map (fun m -> v *. m) mat_row) vec in
      VFMatrix(n, c, result)

  let scalar_mul_int_matrix r c mat scalar =
    let mul_row row = List.map (fun x -> x * scalar) row in
    VIMatrix(r, c, List.map mul_row mat)

  let scalar_mul_float_matrix r c mat scalar =
    let mul_row row = List.map (fun x -> x *. scalar) row in
    VFMatrix(r, c, List.map mul_row mat)

  let matrix_zipWith f m1 m2 =
    let rec aux_rows m1 m2 acc =
      match m1, m2 with
      | [], [] -> List.rev acc
      | row1::rest1, row2::rest2 ->
          aux_rows rest1 rest2 ((VectorOps.vector_zipWith f row1 row2)::acc)
      | _, _ -> raise (Runtime_error "Matrix dimension mismatch")
    in
    aux_rows m1 m2 []

  let get_submatrix mat i j =
    List.mapi (fun row_idx row ->
      if row_idx <> i then
        List.mapi (fun col_idx elem -> (col_idx, elem)) row
        |> List.filter (fun (col_idx, _) -> col_idx <> j)
        |> List.map snd
      else []
    ) mat |> List.filter (fun row -> row <> [])

  let rec calculate_int_determinant mat =
    let n = List.length mat in
    if n = 1 then List.hd (List.hd mat)
    else if n = 2 then
      let a = List.hd (List.hd mat) in
      let b = List.nth (List.hd mat) 1 in
      let c = List.hd (List.nth mat 1) in
      let d = List.nth (List.nth mat 1) 1 in
      a * d - b * c
    else
      let rec sum_cofactors j acc =
        if j >= n then acc
        else
          let element = List.nth (List.hd mat) j in
          let sign = if j mod 2 = 0 then 1 else -1 in
          let submat = get_submatrix mat 0 j in
          let cofactor = sign * element * (calculate_int_determinant submat) in
          sum_cofactors (j+1) (acc + cofactor)
      in
      sum_cofactors 0 0

  let rec calculate_float_determinant mat =
    let n = List.length mat in
    if n = 1 then List.hd (List.hd mat)
    else if n = 2 then
      let a = List.hd (List.hd mat) in
      let b = List.nth (List.hd mat) 1 in
      let c = List.hd (List.nth mat 1) in
      let d = List.nth (List.nth mat 1) 1 in
      a *. d -. b *. c
    else
      let rec sum_cofactors j acc =
        if j >= n then acc
        else
          let element = List.nth (List.hd mat) j in
          let sign = if j mod 2 = 0 then 1.0 else -1.0 in
          let submat = get_submatrix mat 0 j in
          let cofactor = sign *. element *. (calculate_float_determinant submat) in
          sum_cofactors (j+1) (acc +. cofactor)
      in
      sum_cofactors 0 0.0

  let determinant_int_matrix r c mat =
    if r <> c then raise (Runtime_error "Determinant requires a square matrix")
    else VInt (calculate_int_determinant mat)

  let determinant_float_matrix r c mat =
    if r <> c then raise (Runtime_error "Determinant requires a square matrix")
    else VFloat (calculate_float_determinant mat)
end

(* Pretty-print values *)
let string_of_value = function
  | VInt i -> string_of_int i
  | VFloat f -> string_of_float f
  | VBool b -> string_of_bool b
  | VString s -> s  (* Remove the quotes around the string *)
  | VIVector(n, lst) -> 
      let elements = List.map string_of_int lst |> String.concat ", " in
      Printf.sprintf "%d\n[%s]" n elements
  | VFVector(n, lst) -> 
      let elements = List.map string_of_float lst |> String.concat ", " in
      Printf.sprintf "%d\n[%s]" n elements
  | VIMatrix(r, c, mat) ->
      let rows = List.map (fun row ->
        let elements = List.map string_of_int row |> String.concat ", " in
        "[" ^ elements ^ "]"
      ) mat |> String.concat ", " in
      Printf.sprintf "%d,%d\n[%s]" r c rows
  | VFMatrix(r, c, mat) ->
      let rows = List.map (fun row ->
        let elements = List.map string_of_float row |> String.concat ", " in
        "[" ^ elements ^ "]"
      ) mat |> String.concat ", " in
      Printf.sprintf "%d,%d\n[%s]" r c rows
  | VUnit -> "()"  (* Fixed: Return the string "()" instead of unit value *)

open VectorOps
open MatrixOps

(* Helper function to determine if a message is an error *)
let is_error_message msg =
  String.length msg >= 10 && String.sub msg 0 10 = "Type error" ||
  String.length msg >= 6 && String.sub msg 0 6 = "Error:"

(* Evaluate an expression *)
let rec eval_expr env = function
  | Empty -> VUnit
  | Var name -> lookup_var env name
  
  (* Literals *)
  | IntLit i -> VInt i
  | FloatLit f -> VFloat f
  | BoolLit b -> VBool b
  | StringLit s -> VString s
  | IVectorLit(size, elems) -> VIVector(size, elems)
  | FVectorLit(size, elems) -> VFVector(size, elems)
  | IMatrixLit(rows, cols, data) -> VIMatrix(rows, cols, data)
  | FMatrixLit(rows, cols, data) -> VFMatrix(rows, cols, data)
  
  (* Binary operations *)
  | BinOp(e1, op, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match op, v1, v2 with
        (* Integer arithmetic *)
        | IAdd, VInt a, VInt b -> VInt (a + b)
        | ISub, VInt a, VInt b -> VInt (a - b)
        | IMul, VInt a, VInt b -> VInt (a * b)
        | IDiv, VInt a, VInt b -> 
            if b = 0 then raise (Runtime_error "Division by zero")
            else VInt (a / b)
        | IMod, VInt a, VInt b -> 
            if b = 0 then raise (Runtime_error "Modulo by zero")
            else VInt (a mod b)
        | Power, VInt a, VInt b ->
            VInt (int_of_float (float_of_int a ** float_of_int b))

        (* Float arithmetic *)
        | FAdd, VFloat a, VFloat b -> VFloat (a +. b)
        | FSub, VFloat a, VFloat b -> VFloat (a -. b)
        | FMul, VFloat a, VFloat b -> VFloat (a *. b)
        | FDiv, VFloat a, VFloat b -> 
            if b = 0.0 then raise (Runtime_error "Division by zero")
            else VFloat (a /. b)
        | FMod, VFloat a, VFloat b -> 
            if b = 0.0 then raise (Runtime_error "Modulo by zero")
            else VFloat (mod_float a b)
        | Power, VFloat a, VFloat b -> VFloat (a ** b)
   
        (* Integer vector operations *)
        | IAdd, VIVector(n1, lst1), VIVector(n2, lst2) ->
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch")
            else VIVector(n1, vector_zipWith (+) lst1 lst2)
              
        | ISub, VIVector(n1, lst1), VIVector(n2, lst2) ->
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch")
            else VIVector(n1, vector_zipWith (-) lst1 lst2)
              
        | IMul, VIVector(n1, lst1), VIVector(n2, lst2) -> (* Dot product *)
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch")
            else VInt (dot_product lst1 lst2 0 ( * ))
              
        | IMul, VIVector(n, lst), VInt scalar ->
            VIVector(n, scalar_mul lst scalar ( * ))
            
        | IMul, VInt scalar, VIVector(n, lst) ->
            VIVector(n, scalar_mul lst scalar ( * ))
            
        (* Float vector operations *)
        | FAdd, VFVector(n1, lst1), VFVector(n2, lst2) ->
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch")
            else VFVector(n1, vector_zipWith (+.) lst1 lst2)
              
        | FSub, VFVector(n1, lst1), VFVector(n2, lst2) ->
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch")
            else VFVector(n1, vector_zipWith (-.) lst1 lst2)
              
        | FMul, VFVector(n1, lst1), VFVector(n2, lst2) -> (* Dot product *)
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch")
            else VFloat (dot_product lst1 lst2 0.0 ( *. ))
              
        | FMul, VFVector(n, lst), VFloat scalar ->
            VFVector(n, scalar_mul lst scalar ( *. ))
            
        | FMul, VFloat scalar, VFVector(n, lst) ->
            VFVector(n, scalar_mul lst scalar ( *. ))
            
        (* Integer Matrix operations *)
        | IAdd, VIMatrix(r1, c1, mat1), VIMatrix(r2, c2, mat2) ->
            if r1 <> r2 || c1 <> c2 then raise (Runtime_error "Matrix dimension mismatch")
            else VIMatrix(r1, c1, matrix_zipWith (+) mat1 mat2)
              
        | ISub, VIMatrix(r1, c1, mat1), VIMatrix(r2, c2, mat2) ->
            if r1 <> r2 || c1 <> c2 then raise (Runtime_error "Matrix dimension mismatch")
            else VIMatrix(r1, c1, matrix_zipWith (-) mat1 mat2)
              
        | IMul, VIMatrix(r1, c1, mat1), VIMatrix(r2, c2, mat2) ->
            if c1 <> r2 then raise (Runtime_error "Matrix multiplication dimension mismatch")
            else multiply_int_matrices r1 c1 mat1 r2 c2 mat2
              
        | IMul, VIMatrix(r, c, mat), VInt scalar -> scalar_mul_int_matrix r c mat scalar
            
        | IMul, VInt scalar, VIMatrix(r, c, mat) -> scalar_mul_int_matrix r c mat scalar
            
        (* Float Matrix operations *)
        | FAdd, VFMatrix(r1, c1, mat1), VFMatrix(r2, c2, mat2) ->
            if r1 <> r2 || c1 <> c2 then raise (Runtime_error "Matrix dimension mismatch")
            else VFMatrix(r1, c1, matrix_zipWith (+.) mat1 mat2)
              
        | FSub, VFMatrix(r1, c1, mat1), VFMatrix(r2, c2, mat2) ->
            if r1 <> r2 || c1 <> c2 then raise (Runtime_error "Matrix dimension mismatch")
            else VFMatrix(r1, c1, matrix_zipWith (-.) mat1 mat2)
              
        | FMul, VFMatrix(r1, c1, mat1), VFMatrix(r2, c2, mat2) ->
            if c1 <> r2 then raise (Runtime_error "Matrix multiplication dimension mismatch")
            else  multiply_float_matrices r1 c1 mat1 r2 c2 mat2
              
        | FMul, VFMatrix(r, c, mat), VFloat scalar -> scalar_mul_float_matrix r c mat scalar
            
        | FMul, VFloat scalar, VFMatrix(r, c, mat) -> scalar_mul_float_matrix r c mat scalar

        (* Matrix-vector operations *)
        | IMul, VIMatrix(r, c, mat), VIVector(n, vec) -> multiply_matrix_vector_int r c mat vec n
          
        | IMul, VIVector(n, vec), VIMatrix(r, c, mat) -> multiply_vector_matrix_int n vec r c mat
          
        | FMul, VFMatrix(r, c, mat), VFVector(n, vec) -> multiply_matrix_vector_float r c mat vec n
          
        | FMul, VFVector(n, vec), VFMatrix(r, c, mat) -> multiply_vector_matrix_float n vec r c mat
            
        (* Comparison operators *)
        | Equal, v1, v2 -> VBool (v1 = v2)
        | NotEqual, v1, v2 -> VBool (v1 <> v2)
        | Less, VInt a, VInt b -> VBool (a < b)
        | Greater, VInt a, VInt b -> VBool (a > b)
        | LessEq, VInt a, VInt b -> VBool (a <= b)
        | GreaterEq, VInt a, VInt b -> VBool (a >= b)
        | Less, VFloat a, VFloat b -> VBool (a < b)
        | Greater, VFloat a, VFloat b -> VBool (a > b)
        | LessEq, VFloat a, VFloat b -> VBool (a <= b)
        | GreaterEq, VFloat a, VFloat b -> VBool (a >= b)
        
        (* Logical operators *)
        | And, VBool a, VBool b -> VBool (a && b)
        | Or, VBool a, VBool b -> VBool (a || b)
        | Xor, VBool a, VBool b -> VBool ((a || b) && not (a && b))
        
        (* Special vector operations *)
        | Angle, VIVector(n1, vec1), VIVector(n2, vec2) ->
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch for angle")
            else VFloat (angle_int_vectors vec1 vec2)
              
        | Angle, VFVector(n1, vec1), VFVector(n2, vec2) ->
            if n1 <> n2 then raise (Runtime_error "Vector dimension mismatch for angle")
            else VFloat (angle_float_vectors vec1 vec2)
              
        | _ -> raise (Runtime_error "Invalid binary operation")
      )[@warning "-fragile-match"]
      
  (* Unary operations *)
  | UnOp(op, e) ->
      let v = eval_expr env e in
      (match op, v with
      | INeg, VInt i -> VInt (-i)
      | FNeg, VFloat f -> VFloat (-.f)
      | Not, VBool b -> VBool (not b)
      
      (* Type casting *)
      | I2F, VInt i -> VFloat (float_of_int i)
      
      | Abs, VInt i -> VInt (abs i)
      | Abs, VFloat f -> VFloat (abs_float f)
      
      | Transpose, VIMatrix(r, c, mat) ->  VIMatrix(c, r, build_transpose mat c)
          
      | Transpose, VFMatrix(r, c, mat) -> VFMatrix(c, r, build_transpose mat c)
          
      | Det, VIMatrix(r, c, mat) -> determinant_int_matrix r c mat
      | Det, VFMatrix(r, c, mat) -> determinant_float_matrix r c mat
            
      | Dimension, VIVector(n, _) -> VInt n
      | Dimension, VFVector(n, _) -> VInt n
      | Dimension, VIMatrix(r, c, _) -> VIVector(2, [r; c])
          
      | Dimension, VFMatrix(r, c, _) -> VIVector(2, [r; c])
          
      | Magnitude, VIVector(_, vec) ->
          VFloat (magnitude_int_vector vec)
          
      | Magnitude, VFVector(_, vec) ->
          VFloat (magnitude_float_vector vec)
          
      | _ -> raise (Runtime_error "Invalid unary operation")
      )[@warning "-fragile-match"]
      
  (* Indexing *)
  | VectorIndex(vec_expr, idx_expr) ->
      let vec = eval_expr env vec_expr in
      let idx = eval_expr env idx_expr in
      (match vec, idx with
      | VIVector(n, lst), VInt i ->
          if i < 0 || i >= n then raise (Runtime_error "Vector index out of bounds")
          else VInt (get_nth lst i)
          
      | VFVector(n, lst), VInt i ->
          if i < 0 || i >= n then raise (Runtime_error "Vector index out of bounds")
          else VFloat (get_nth lst i)
          
      | _ -> raise (Runtime_error "Invalid vector indexing")
      )[@warning "-fragile-match"]
      
  | MatrixIndex(mat_expr, row_expr, col_expr) ->
      let mat = eval_expr env mat_expr in
      let row = eval_expr env row_expr in
      let col = eval_expr env col_expr in
      (match mat, row, col with
      | VIMatrix(rows, cols, mat_lst), VInt r, VInt c ->
          if r < 0 || r >= rows || c < 0 || c >= cols then 
            raise (Runtime_error "Matrix index out of bounds")
          else 
            let row_lst = get_row mat_lst r in
            VInt (get_nth row_lst c)
          
      | VFMatrix(rows, cols, mat_lst), VInt r, VInt c ->
          if r < 0 || r >= rows || c < 0 || c >= cols then 
            raise (Runtime_error "Matrix index out of bounds")
          else
            let row_lst = get_row mat_lst r in
            VFloat (get_nth row_lst c)
          
      | _ -> raise (Runtime_error "Invalid matrix indexing")
      )[@warning "-fragile-match"]
      
  | RowAccess(mat_expr, row_expr) ->
      let mat = eval_expr env mat_expr in
      let row = eval_expr env row_expr in
      (match mat, row with
      | VIMatrix(rows, cols, mat_lst), VInt r ->
          if r < 0 || r >= rows then raise (Runtime_error "Row index out of bounds")
          else
            let row_lst = get_row mat_lst r in
            VIVector(cols, row_lst)
            
      | VFMatrix(rows, cols, mat_lst), VInt r ->
          if r < 0 || r >= rows then raise (Runtime_error "Row index out of bounds")
          else
            let row_lst = get_row mat_lst r in
            VFVector(cols, row_lst)
            
      | _ -> raise (Runtime_error "Invalid row access")
      )[@warning "-fragile-match"]

(* Execute a statement *)
let rec exec_stmt env = function
  | ExprStmt e ->
      let _ = eval_expr env e in
      env
  
  | DeclareStmt(var, e) ->
      (* For let declaration, variable must not exist *)
      let v = eval_expr env e in
      if StringMap.mem var env.declared then
        raise (Runtime_error ("Variable '" ^ var ^ "' is already declared. Use regular assignment to update."))
      else
        declare_var env var v

  | AssignStmt(var, e) ->
      (* For assignment, variable must exist *)
      let v = eval_expr env e in
      if not (StringMap.mem var env.declared) then
        raise (Runtime_error ("Cannot assign to undeclared variable: '" ^ var ^ "'. Use 'let' to declare first."))
      else
        update_var env var v
  
  | IfStmt(cond, then_stmts, else_stmts) ->
      let condition = eval_expr env cond in(
      match condition with
      | VBool true -> 
          (* Create new scope for then branch *)
          let then_scope = enter_scope env in
          (* Execute then branch in new scope *)
          let then_result = exec_stmts then_scope then_stmts in
          (* Merge changes back to parent scope *)
          exit_scope env then_result
          
      | VBool false -> 
          (* Create new scope for else branch *)
          let else_scope = enter_scope env in
          (* Execute else branch in new scope *)
          let else_result = exec_stmts else_scope else_stmts in
          (* Merge changes back to parent scope *)
          exit_scope env else_result
          
      | _ -> raise (Runtime_error "If condition must evaluate to a boolean")
  )[@warning "-fragile-match"]
  | ForStmt(var, start_expr, end_expr, body) ->
      let start_val = eval_expr env start_expr in
      let end_val = eval_expr env end_expr in
      
      (match start_val, end_val with
      | VInt start, VInt end_val ->
          (* Create a single child scope for the entire loop *)
          let iter_scope = enter_scope env in
          let iter_scope = declare_var iter_scope var (VInt start) in
          (* Iterative loop implementation *)
          let rec iterative_loop i current_scope =
            if i >= end_val then
              (* Loop complete, return the final scope state *)
              current_scope
            else
              (* For each iteration: 
                 1. Add/update the iterator variable 
                 2. Execute the body 
                 3. Move to next iteration with same scope object *)
              let updated_scope = update_var current_scope var (VInt i) in
              (* Execute the body in the updated scope *)
              let after_body = exec_stmts updated_scope body in
              let i = match lookup_var after_body var with
              | VInt new_i -> new_i
              | _ -> raise (Runtime_error "Iterator variable must be an integer") in

              (* Continue to next iteration with this scope *)
              iterative_loop (i + 1) after_body
          in
          
          (* Run the iterative loop and get the final child scope *)
          let final_child_scope = iterative_loop start iter_scope in
          
          (* Update the parent environment with any changes to parent variables *)
          exit_scope env final_child_scope
          
      | _ -> raise (Runtime_error "For loop bounds must be integers")
  )[@warning "-fragile-match"]
  | WhileStmt(cond, body) ->
      (* Create a single child scope for the entire loop *)
      let loop_scope = enter_scope env in
      
      (* Iterative implementation *)
      let rec iterative_loop current_scope =
        (match eval_expr current_scope cond with
        | VBool true ->
            (* Execute the body in the current scope *)
            let after_body = exec_stmts current_scope body in
            (* Continue to next iteration with updated scope *)
            iterative_loop after_body
            
        | VBool false -> 
            (* Loop complete, return final scope *)
            current_scope
            
        | _ -> raise (Runtime_error "While condition must evaluate to a boolean")
      )[@warning "-fragile-match"]
      in
      
      (* Run the iterative loop and get the final child scope *)
      let final_child_scope = iterative_loop loop_scope in
      
      (* Update the parent environment with any changes to parent variables *)
      exit_scope env final_child_scope
  
  | InputStmt e ->
      (match eval_expr env e with
      | VUnit -> (* Simple input with no arguments *)
          print_string "Input: ";
          flush stdout;
          let _ = read_line () in
          (* Just return the environment unchanged, input will be handled elsewhere *)
          env
      | VString filename -> (* Read from file *)
          (* In a real implementation, this would read from a file *)
          Printf.printf "Reading from file: %s\n" filename;
          env
      | _ -> raise (Runtime_error "Input argument must be a string (filename) or empty")
    )[@warning "-fragile-match"]
  | PrintStmt e ->(
      match eval_expr env e with
      | VUnit -> print_newline (); env
      | v -> 
          print_endline (string_of_value v);
          env
      )[@warning "-fragile-match"]

(* Execute a list of statements *)
and exec_stmts env = function
  | [] -> env
  | s :: rest ->
      let env' = exec_stmt env s in
      exec_stmts env' rest      

(* Type check and execute a program *)
let interpret program =
  (* First type check the program *)
  let type_result = type_check program in
  if is_error_message type_result then
    Printf.printf "Type error: %s\n" type_result
  else
    (* If type checking passed, execute the program *)
    try
      match program with
      | Program stmts ->
          let _ = exec_stmts empty_runtime_env stmts in
          Printf.printf "Program executed successfully.\n"
    with
    | Runtime_error msg -> 
        Printf.printf "Runtime error: %s\n" msg;
        exit 1  (* Exit with error code *)
    | e -> 
        Printf.printf "Unexpected error: %s\n" (Printexc.to_string e);
        exit 1  (* Exit with error code *)
