
exception VariableLookupError of string
exception EvaluationFailure of string
type identifier = string
type exp =
  | Identifier of identifier
  | Integer of int          
  | Boolean of bool         
  | Lambda of identifier * exp
  | App of exp * exp
  | If of exp * exp * exp  
  | Addition of exp * exp  
  | Subtraction of exp * exp  
  | Multiplication of exp * exp  
  | Not of exp              
  | And of exp * exp        
  | Or of exp * exp         


type gamma = (identifier * closure) list
and closure = Closure of exp * gamma


let rec lookup (x : identifier) (environment : gamma) : closure option =
  match environment with
  | [] -> None  (* Variable not found - it's free *)
  | (name, closure) :: rest -> 
      if name = x then Some closure 
      else lookup x rest

let rec remove (x : identifier) (environment : gamma) : gamma =
  match environment with
  | [] -> []  (* Empty environment - nothing to remove *)
  | (name, closure) :: rest -> 
      if name = x then remove x rest  (* Skip this binding and continue *)
      else (name, closure) :: remove x rest

let rec execute_krivine (state : closure * closure list) : closure =
  match evaluate_step state with
  | None -> fst state  (* Terminal state - return the closure *)
  | Some next_state -> execute_krivine next_state

(* Single step of the Krivine machine evaluation *)
and evaluate_step (machine_state : closure * closure list) : (closure * closure list) option =
  let (Closure(expr, env), ctx_stack) = machine_state in
  match expr with
  | Identifier var_name ->
    (* Rule (Var): Look up the variable in environment *)
    (match lookup var_name env with
     | Some cl -> Some (cl, ctx_stack)
     | None -> 
         None)
    
  | Lambda(param, body) ->
      (* Rule (App): Apply function if arguments available on stack *)
      (match ctx_stack with
        | [] -> None  (* lambda value *)
        | arg_closure :: rest_stack ->
            let extended_env = (param, arg_closure) :: env in
            Some (Closure(body, extended_env), rest_stack))
            
  | App(fun_expr, arg_expr) ->
      (* Rule (Op): Push argument as closure, continue with function evaluation *)
      Some (Closure(fun_expr, env), Closure(arg_expr, env) :: ctx_stack)
      
  | Integer _ | Boolean _ ->
      (* Primitive values are terminal unless on stack with more work *)
      if ctx_stack = [] then None
      else raise (EvaluationFailure "Cannot apply a primitive value")
      
  | If(cond, then_branch, else_branch) ->
      (* Handle conditional by evaluating condition first *)
      let condition_result = execute_krivine (Closure(cond, env), []) in
      (match condition_result with
       | Closure(Boolean true, _) -> Some (Closure(then_branch, env), ctx_stack)
       | Closure(Boolean false, _) -> Some (Closure(else_branch, env), ctx_stack)
       | _ -> raise (EvaluationFailure "Condition doesn't evaluate to a boolean"))
      
  | Addition(left, right) ->
      (* Evaluate both operands for addition *)
      let left_val = execute_krivine (Closure(left, env), []) in
      let right_val = execute_krivine (Closure(right, env), []) in
      (match (left_val, right_val) with
       | (Closure(Integer n1, _), Closure(Integer n2, _)) ->
           Some (Closure(Integer (n1 + n2), env), ctx_stack)
       | _ -> raise (EvaluationFailure "Addition requires integer operands"))
  | Subtraction(left, right) ->
      (* Evaluate both operands for subtraction *)
      let left_val = execute_krivine (Closure(left, env), []) in
      let right_val = execute_krivine (Closure(right, env), []) in
      (match (left_val, right_val) with
        | (Closure(Integer n1, _), Closure(Integer n2, _)) ->
            Some (Closure(Integer (n1 - n2), env), ctx_stack)
        | _ -> raise (EvaluationFailure "Subtraction requires integer operands"))
         
  | Multiplication(left, right) ->
      (* Evaluate both operands for multiplication *)
      let left_val = execute_krivine (Closure(left, env), []) in
      let right_val = execute_krivine (Closure(right, env), []) in
      (match (left_val, right_val) with
        | (Closure(Integer n1, _), Closure(Integer n2, _)) ->
            Some (Closure(Integer (n1 * n2), env), ctx_stack)
        | _ -> raise (EvaluationFailure "Multiplication requires integer operands"))
  | Not(expr) ->
      (* Evaluate the expression and negate its boolean value *)
      let expr_val = execute_krivine (Closure(expr, env), []) in
      (match expr_val with
       | Closure(Boolean b, _) ->
           Some (Closure(Boolean (not b), env), ctx_stack)
       | _ -> raise (EvaluationFailure "NOT operation requires a boolean operand"))
           
  | And(left, right) ->
      (* Evaluate left operand first (short-circuit evaluation) *)
      let left_val = execute_krivine (Closure(left, env), []) in
      (match left_val with
       | Closure(Boolean false, _) ->
           (* Short-circuit: if left is false, result is false *)
           Some (Closure(Boolean false, env), ctx_stack)
       | Closure(Boolean true, _) ->
           (* If left is true, result depends on right *)
           let right_val = execute_krivine (Closure(right, env), []) in
           (match right_val with
            | Closure(Boolean b, _) ->
                Some (Closure(Boolean b, env), ctx_stack)
            | _ -> raise (EvaluationFailure "AND operation requires boolean operands"))
       | _ -> raise (EvaluationFailure "AND operation requires boolean operands"))
           
  | Or(left, right) ->
      (* Evaluate left operand first (short-circuit evaluation) *)
      let left_val = execute_krivine (Closure(left, env), []) in
      (match left_val with
       | Closure(Boolean true, _) ->
           (* Short-circuit: if left is true, result is true *)
           Some (Closure(Boolean true, env), ctx_stack)
       | Closure(Boolean false, _) ->
           (* If left is false, result depends on right *)
           let right_val = execute_krivine (Closure(right, env), []) in
           (match right_val with
            | Closure(Boolean b, _) ->
                Some (Closure(Boolean b, env), ctx_stack)
            | _ -> raise (EvaluationFailure "OR operation requires boolean operands"))
       | _ -> raise (EvaluationFailure "OR operation requires boolean operands"))

(* Convert closure back to pure lambda exp with Church numeral normalization *)
let rec unload (Closure(t, env)) : exp =
  let exp = match t with
  | Identifier x ->
      (match lookup x env with
       | Some bound_closure -> unload bound_closure
       | None -> Identifier x)  (* Free variable - return as is *)
      
  | Lambda(param, body) ->
      Lambda(param, unload (Closure(body, (remove param env))))
      
  | App(t1, t2) ->
      App(unload (Closure(t1, env)), 
          unload (Closure(t2, env)))
            
  | Integer n -> Integer n
  | Boolean b -> Boolean b
  | If(c, t, e) ->
      If(unload (Closure(c, env)),
         unload (Closure(t, env)),
         unload (Closure(e, env)))
  | Addition(e1, e2) ->
      Addition(unload (Closure(e1, env)),
               unload (Closure(e2, env)))
  | Subtraction(e1, e2) ->
      Subtraction(unload (Closure(e1, env)),
                  unload (Closure(e2, env)))
  | Multiplication(e1, e2) ->
      Multiplication(unload (Closure(e1, env)),
                    unload (Closure(e2, env)))
  | Not(e) ->
      Not(unload (Closure(e, env)))
  | And(e1, e2) ->
      And(unload (Closure(e1, env)),
          unload (Closure(e2, env)))
  | Or(e1, e2) ->
      Or(unload (Closure(e1, env)),
         unload (Closure(e2, env)))
  in
  let normalized = normalize_expression exp in
  normalize_church_numeral normalized

(* Helper function to recognize and normalize Church numerals *)
and normalize_church_numeral exp =
  (* Check if this has the form λf.λx.BODY where BODY could be a Church numeral application *)
  match exp with
  | Lambda(f, Lambda(x, body)) ->
      (* Try to count nested applications of f to x *)
      let rec count_applications term n =
        match term with
        | Identifier var when var = x -> 
            Some n 
        | App(Identifier var, inner) when var = f ->
            count_applications inner (n + 1)  
        | App(outer, inner) ->
            (match normalize_expression outer with
             | Identifier var when var = f -> count_applications inner (n + 1)
             | _ -> None)  
        | _ -> None  (* Not a Church numeral pattern *)
      in
      (match count_applications body 0 with
       | Some n -> church_numeral_of_int n f x  (* canonical Church numeral *)
       | None -> exp)  (* Not recognized as a Church numeral, return as is *)
  | _ -> exp  (* Not a Church numeral pattern *)

  and normalize_expression exp =
  let rec normalize_until_fixed_point e =
    let e' = normalize_step e in
    if e = e' then e else normalize_until_fixed_point e'
  in
  normalize_until_fixed_point exp

and normalize_step exp =
  match exp with
  | App(Lambda(x, body), arg) ->
      (* Beta reduction: substitute arg for x in body *)
      substitute body x arg
  | App(e1, e2) ->
      let e1' = normalize_step e1 in
      let e2' = normalize_step e2 in
      if e1 = e1' && e2 = e2' then 
        match e1' with
        | Lambda _ -> App(e1', e2')  
        | _ -> App(e1', e2')
      else
        App(e1', e2')
  | Lambda(x, body) ->
      Lambda(x, normalize_step body)
  | _ -> exp


and substitute e1 x e2 =
  match e1 with
  | Identifier y -> if y = x then e2 else Identifier y
  | Lambda(y, body) ->
      if y = x then Lambda(y, body)  
      else Lambda(y, substitute body x e2)
  | App(e1', e2') ->
      App(substitute e1' x e2, substitute e2' x e2)
  | _ -> e1  


and church_numeral_of_int n f x =
  let rec apply_n_times f_id x_id n =
    if n = 0 then Identifier x_id
    else App(Identifier f_id, apply_n_times f_id x_id (n-1))
  in
  Lambda(f, Lambda(x, apply_n_times f x n))


let rec string_of_term (t : exp) : string =
  match t with
  | Identifier x -> x
  | Lambda(x, body) -> "λ" ^ x ^ ".(" ^ string_of_term body ^ ")"
  | App(t1, t2) -> "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | Integer n -> string_of_int n
  | Boolean b -> string_of_bool b
  | If(c, t, e) ->
      "if " ^ string_of_term c ^ " then " ^ string_of_term t ^ " else " ^ string_of_term e
  | Addition(e1, e2) -> "(" ^ string_of_term e1 ^ " + " ^ string_of_term e2 ^ ")"
  | Subtraction(e1, e2) -> "(" ^ string_of_term e1 ^ " - " ^ string_of_term e2 ^ ")"
  | Multiplication(e1, e2) -> "(" ^ string_of_term e1 ^ " * " ^ string_of_term e2 ^ ")"
  | Not(e) -> "(not " ^ string_of_term e ^ ")"
  | And(e1, e2) -> "(" ^ string_of_term e1 ^ " && " ^ string_of_term e2 ^ ")"
  | Or(e1, e2) -> "(" ^ string_of_term e1 ^ " || " ^ string_of_term e2 ^ ")"

(* Main evaluation function *)
let evaluate (t : exp) : exp =
  let result_closure = execute_krivine (Closure(t, []), []) in
  unload result_closure
