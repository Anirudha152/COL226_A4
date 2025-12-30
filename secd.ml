
exception VariableLookupError of string
exception MachineError of string
type identifier = string
type exp =
  | Identifier of identifier
  | Lambda of identifier * exp
  | App of exp * exp
  | Integer of int          (* Extension for primitives *)
  | Boolean of bool         (* Extension for primitives *)
  | If of exp * exp * exp  
  | Addition of exp * exp  
  | Subtraction of exp * exp
  | Multiplication of exp * exp 
  | Not of exp              
  | And of exp * exp        
  | Or of exp * exp         

(* SECD Machine Instructions *)
type opcode =
  | LOOKUP of identifier     
  | APP
  | RET                   
  | MkCLOS of identifier * opcode list
  | PushInt of int           
  | PushBool of bool         
  | IF of opcode list * opcode list  
  | PLUS                    
  | MINUS                   
  | MULT                    
  | NOT                     
  | AND                     
  | OR                      

(* String representation of opcodes *)
let rec string_of_opcode_list (ops : opcode list) : string =
  "[" ^ String.concat "; " (List.map string_of_opcode ops) ^ "]"
and string_of_opcode (op : opcode) : string =
  match op with
  | LOOKUP id -> "LOOKUP " ^ id
  | APP -> "APP"
  | RET -> "RET"
  | MkCLOS(param, body) -> "MkCLOS(" ^ param ^ ", " ^ string_of_opcode_list body ^ ")"
  | PushInt n -> "PushInt " ^ string_of_int n
  | PushBool b -> "PushBool " ^ string_of_bool b
  | IF(then_code, else_code) ->
      "IF(" ^ string_of_opcode_list then_code ^ ", " ^ string_of_opcode_list else_code ^ ")"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | MULT -> "MULT"
  | NOT -> "NOT"
  | AND -> "AND"
  | OR -> "OR"


type value =
  | IntegerVal of int
  | BooleanVal of bool
  | ClosureVal of identifier * opcode list * gamma

and gamma = (identifier * value) list


type dump_stack = (value list * gamma * opcode list) list


(* Lookup a variable in the gamma *)
let rec lookup (id : identifier) (env : gamma) : value =
  match env with
  | [] -> raise (VariableLookupError ("Unbound variable: " ^ id))
  | (name, value) :: rest -> if name = id then value else lookup id rest


let rec compile (t : exp) : opcode list =
  match t with
  | Identifier id -> [LOOKUP id]
  | Lambda(param, body) -> [MkCLOS(param, (compile body) @ [RET])]
  | App(fun_term, arg_term) -> (compile fun_term) @ (compile arg_term) @ [APP]
  | Integer n -> [PushInt n]
  | Boolean b -> [PushBool b]
  | If(cond, then_branch, else_branch) ->
      (compile cond) @ [IF(compile then_branch, compile else_branch)]
  | Addition(left, right) -> (compile left) @ (compile right) @ [PLUS]
  | Subtraction(left, right) -> (compile left) @ (compile right) @ [MINUS]
  | Multiplication(left, right) -> (compile left) @ (compile right) @ [MULT]
  | Not(expr) -> (compile expr) @ [NOT]
  | And(left, right) -> (compile left) @ (compile right) @ [AND]
  | Or(left, right) -> (compile left) @ (compile right) @ [OR]

(* SECD Machine execution *)
let rec execute_secd (stack : value list) 
                     (env : gamma) 
                     (code : opcode list) 
                     (dump : dump_stack) : value =
  match code with
  | [] ->
      (* Machine halts with top value on stack *)
      (match stack with
       | value :: _ -> value
       | [] -> raise (MachineError "Machine halted with empty stack"))
       
  | opcode :: rest_code ->
      match opcode with
      | LOOKUP id ->
        (* Look up variable value and push to stack *)
        let value = lookup id env in
        execute_secd (value :: stack) env rest_code dump
                          
      | PushInt n ->
        execute_secd (IntegerVal n :: stack) env rest_code dump
    
      | PushBool b ->
          execute_secd (BooleanVal b :: stack) env rest_code dump
          
      | APP ->
        (match stack with
          | arg_val :: (ClosureVal(param, body_code, closure_env)) :: rest_stack ->
              (* Apply function: set up new gamma with bound parameter *)
              let new_env = (param, arg_val) :: closure_env in
              (* Save current context to dump *)
              let saved_context = (rest_stack, env, rest_code) in
              execute_secd [] new_env body_code (saved_context :: dump)
          | _ -> raise (MachineError "Function application requires closure and argument"))
          
      | RET ->
          (match dump with
            | (saved_stack, saved_env, saved_code) :: rest_dump ->
                (* RET from function call with result value *)
                (match stack with
                | result :: _ ->
                    execute_secd (result :: saved_stack) saved_env saved_code rest_dump
                | [] -> raise (MachineError "RET with empty result stack"))
            | [] ->
                (* If dump is empty, we're at the top level *)
                (match stack with
                | result :: _ -> result
                | [] -> raise (MachineError "RET at top level with empty stack")))
      | MkCLOS(param, body_code) ->
          (* Create a function closure *)
          let closure = ClosureVal(param, body_code, env) in
          execute_secd (closure :: stack) env rest_code dump

      | IF(then_code, else_code) ->
          (match stack with
           | BooleanVal b :: rest_stack ->
               let branch_code = if b then then_code else else_code in
               execute_secd rest_stack env (branch_code @ rest_code) dump
           | _ -> raise (MachineError "Conditional requires boolean value"))
           
      | PLUS ->
          (match stack with
           | IntegerVal n2 :: IntegerVal n1 :: rest_stack ->
               execute_secd (IntegerVal (n1 + n2) :: rest_stack) env rest_code dump
           | _ -> raise (MachineError "Addition requires two integers"))
           
      | MINUS ->
          (match stack with
          | IntegerVal n2 :: IntegerVal n1 :: rest_stack ->
              execute_secd (IntegerVal (n1 - n2) :: rest_stack) env rest_code dump
          | _ -> raise (MachineError "Subtraction requires two integers"))
          
      | MULT ->
          (match stack with
          | IntegerVal n2 :: IntegerVal n1 :: rest_stack ->
              execute_secd (IntegerVal (n1 * n2) :: rest_stack) env rest_code dump
          | _ -> raise (MachineError "Multiplication requires two integers"))
          
      | NOT ->
          (match stack with
           | BooleanVal b :: rest_stack ->
               execute_secd (BooleanVal (not b) :: rest_stack) env rest_code dump
           | _ -> raise (MachineError "NOT operation requires a boolean"))
           
      | AND ->
          (match stack with
           | BooleanVal b2 :: BooleanVal b1 :: rest_stack ->
               execute_secd (BooleanVal (b1 && b2) :: rest_stack) env rest_code dump
           | _ -> raise (MachineError "AND operation requires two booleans"))
           
      | OR ->
          (match stack with
           | BooleanVal b2 :: BooleanVal b1 :: rest_stack ->
               execute_secd (BooleanVal (b1 || b2) :: rest_stack) env rest_code dump
           | _ -> raise (MachineError "OR operation requires two booleans"))


let string_of_value (v : value) : string =
  match v with
  | IntegerVal n -> string_of_int n
  | BooleanVal b -> string_of_bool b
  | ClosureVal(_, l, _) -> string_of_opcode_list l


let value_to_term (v : value) : exp =
  match v with
  | IntegerVal n -> Integer n
  | BooleanVal b -> Boolean b
  | ClosureVal(param, _, _) -> Lambda(param, Identifier "...")


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


let evaluate (t : exp) : value =
  let instructions = compile t in
    execute_secd [] [] instructions []
;;