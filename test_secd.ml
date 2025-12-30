(* Tests for SECD machine *)
open Claude_secd

(* ================= Common Test Framework ================= *)

(* Church encodings for testing *)
module Church = struct
  (* Church booleans *)
  let true_c = Lambda("t", Lambda("f", Identifier "t"))  (* λt.λf.t *)
  let false_c = Lambda("t", Lambda("f", Identifier "f")) (* λt.λf.f *)
  
  (* Church numerals: n = λf.λx.f^n(x) *)
  let zero = Lambda("f", Lambda("x", Identifier "x"))    (* λf.λx.x *)
  let one = Lambda("f", Lambda("x", App(Identifier "f", Identifier "x"))) (* λf.λx.f x *)
  let two = Lambda("f", Lambda("x", App(Identifier "f", App(Identifier "f", Identifier "x")))) (* λf.λx.f(f x) *)
  let three = Lambda("f", Lambda("x", App(Identifier "f", App(Identifier "f", App(Identifier "f", Identifier "x"))))) (* λf.λx.f(f(f x)) *)
  
  (* Church successor: λn.λf.λx.f(n f x) *)
  let succ = Lambda("n", Lambda("f", Lambda("x", 
              App(Identifier "f", App(App(Identifier "n", Identifier "f"), Identifier "x")))))
  
  (* Church addition: λm.λn.λf.λx.m f (n f x) *)
  let add = Lambda("m", Lambda("n", Lambda("f", Lambda("x",
             App(App(Identifier "m", Identifier "f"), 
                 App(App(Identifier "n", Identifier "f"), Identifier "x"))))))
             
  (* Church multiplication: λm.λn.λf.m (n f) *)
  let mult = Lambda("m", Lambda("n", Lambda("f", 
              App(Identifier "m", App(Identifier "n", Identifier "f")))))
end

(* Combinators *)
module Combinators = struct
  (* Identity: I = λx.x *)
  let id = Lambda("x", Identifier "x")
  
  (* Constant function: K = λx.λy.x *)
  let k = Lambda("x", Lambda("y", Identifier "x"))
end

(* Pairs for testing higher-order functions *)
module Pairs = struct
  (* Church pair constructor: pair = λx.λy.λf.f x y *)
  let pair = Lambda("x", Lambda("y", Lambda("f", 
              App(App(Identifier "f", Identifier "x"), Identifier "y"))))
              
  (* First element of pair: fst = λp.p (λx.λy.x) *)
  let fst = Lambda("p", App(Identifier "p", Lambda("x", Lambda("y", Identifier "x"))))
  
  (* Second element of pair: snd = λp.p (λx.λy.y) *)
  let snd = Lambda("p", App(Identifier "p", Lambda("x", Lambda("y", Identifier "y"))))
end

module Extension = struct
  (* Basic arithmetic operations *)
  let add_test = Addition(Integer 15, Integer 27)       (* 15 + 27 = 42 *)
  let sub_test = Subtraction(Integer 50, Integer 60)    (* 50 - 60 = -10 *)
  let mult_test = Multiplication(Integer 6, Integer 0)  (* 6 * 0 = 0 *)
  
  (* Nested arithmetic operations *)
  let nested_add = Addition(Addition(Integer 10, Integer 15), Integer 17)  (* (10 + 15) + 17 = 42 *)
  let nested_sub = Subtraction(Integer 50, Subtraction(Integer 5, Integer 3))  (* 50 - (5 - 3) = 48 *)
  let nested_mult = Multiplication(Integer 3, Multiplication(Integer 4, Integer 2))  (* 3 * (4 * 2) = 24 *)
  
  (* Mixed operations *)
  let mixed_add_sub = Addition(Integer 30, Subtraction(Integer 20, Integer 8))  (* 30 + (20 - 8) = 42 *)
  let mixed_sub_mult = Subtraction(Integer 50, Multiplication(Integer 4, Integer 2))  (* 50 - (4 * 2) = 42 *)
  let mixed_add_mult = Addition(Integer 10, Multiplication(Integer 8, Integer 4))  (* 10 + (8 * 4) = 42 *)
  let complex_expr = Multiplication(
                       Addition(Integer 3, Integer 7), 
                       Subtraction(Integer 9, Integer 4))  (* (3 + 7) * (9 - 4) = 50 *)
  
  (* Operations with lambda expressions *)
  let apply_arithmetic = App(
                           Lambda("x", Addition(Identifier "x", Integer 10)),
                           Integer 32)  (* (λx.x + 10) 32 = 42 *)
                           
  let nested_lambda_arith = App(
                              Lambda("x", App(
                                Lambda("y", Multiplication(Identifier "x", Identifier "y")),
                                Integer 6)),
                              Integer 7)  (* (λx.(λy.x * y) 6) 7 = 42 *)

  (* Boolean operations *)
  let not_true = Not(Boolean true)                        (* not true = false *)
  let not_false = Not(Boolean false)                      (* not false = true *)
  let not_complex = Not(Not(Boolean false))               (* not (not false) = false *)
  
  (* AND operations *)
  let and_true_true = And(Boolean true, Boolean true)     (* true && true = true *)
  let and_true_false = And(Boolean true, Boolean false)   (* true && false = false *)
  let and_false_true = And(Boolean false, Boolean true)   (* false && true = false *)
  let and_false_false = And(Boolean false, Boolean false) (* false && false = false *)
  
  (* OR operations *)
  let or_true_true = Or(Boolean true, Boolean true)       (* true || true = true *)
  let or_true_false = Or(Boolean true, Boolean false)     (* true || false = true *)
  let or_false_true = Or(Boolean false, Boolean true)     (* false || true = true *)
  let or_false_false = Or(Boolean false, Boolean false)   (* false || false = false *)
  
  (* Complex boolean expressions *)
  let complex_bool_expr1 = And(Boolean true, 
                              Or(Boolean false, Boolean true))
                              (* true && (false || true) = true *)
                              
  let complex_bool_expr2 = Or(And(Boolean true, Boolean false),
                             And(Boolean true, Boolean true))
                             (* (true && false) || (true && true) = false || true = true *)
                             
  (* Nested boolean operations with NOT *)
  let nested_not = Not(And(Boolean true, Or(Boolean false, Boolean true)))
                    (* not (true && (false || true)) = not true = false *)
  
  (* Boolean operations with lambda expressions *)
  let lambda_bool_expr = App(
                           Lambda("x", And(Identifier "x", Boolean true)),
                           Boolean false)
                           (* (λx.x && true) false = false && true = false *)
end

(* ================= Test Utilities ================= *)

(* Function to complete the string_of_term implementation *)
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

(* Execute test and print result *)
let test_secd name expr expected =
  try
    let code = compile expr in
    let result_value = execute_secd [] [] code [] in
    let result = value_to_term result_value in
    let result_str = string_of_term result in
    let expected_str = string_of_term expected in
    Printf.printf "[SECD] Test %s: %s\n" name 
      (if result_str = expected_str then "PASSED" else 
        Printf.sprintf "FAILED\n  Expected: %s\n  Got: %s" expected_str result_str)
  with
  | MachineError msg -> Printf.printf "[SECD] Test %s: ERROR - %s\n" name msg
  | VariableLookupError msg -> Printf.printf "[SECD] Test %s: ERROR - %s\n" name msg
  | _ -> Printf.printf "[SECD] Test %s: UNKNOWN ERROR\n" name
  
(* Execute test and print result with opcode verification *)
let test_secd_with_opcodes name expr expected expected_opcodes =
  try
    let code = compile expr in
    let code_str = string_of_opcode_list code in
    let expected_opcodes_str = string_of_opcode_list expected_opcodes in
    
    (* First check if compilation produced expected opcodes *)
    let opcodes_match = code_str = expected_opcodes_str in
    
    (* Then check execution result *)
    let result_value = execute_secd [] [] code [] in
    let result = value_to_term result_value in
    let result_str = string_of_term result in
    let expected_str = string_of_term expected in
    let result_match = result_str = expected_str in
    
    Printf.printf "[SECD] Test %s:\n" name;
    Printf.printf "  Opcodes: %s\n" (if opcodes_match then "PASSED" else 
      Printf.sprintf "FAILED\n    Expected: %s\n    Got: %s" expected_opcodes_str code_str);
    Printf.printf "  Result: %s\n" (if result_match then "PASSED" else 
      Printf.sprintf "FAILED\n    Expected: %s\n    Got: %s" expected_str result_str);
  with
  | MachineError msg -> Printf.printf "[SECD] Test %s: ERROR - %s\n" name msg
  | VariableLookupError msg -> Printf.printf "[SECD] Test %s: ERROR - %s\n" name msg
  | _ -> Printf.printf "[SECD] Test %s: UNKNOWN ERROR\n" name

(* ================= SECD Machine Tests ================= *)

let run_secd_tests () = 
  Printf.printf "\n========== SECD MACHINE TESTS ==========\n";
  
  (* 1. Basic lambda calculus properties *)
  test_secd "Identity function" 
    (App(Combinators.id, Integer 5)) (Integer 5);
    
  test_secd "K combinator test" 
    (App(App(Combinators.k, Integer 3), Integer 7)) (Integer 3);
    
  (* 2. Built-in primitives *)
  test_secd "Built-in addition" 
    (Addition(Integer 5, Integer 7)) (Integer 12);
    
  test_secd "Built-in conditional (true)" 
    (If(Boolean true, Integer 5, Integer 10)) (Integer 5);
    
  test_secd "Built-in conditional (false)" 
    (If(Boolean false, Integer 5, Integer 10)) (Integer 10);
    
  (* 3. Pairs *)
  let test_pair = App(App(Pairs.pair, Integer 3), Integer 4) in
  
  test_secd "Church pair first element" 
    (App(Pairs.fst, test_pair)) (Integer 3);
    
  test_secd "Church pair second element" 
    (App(Pairs.snd, test_pair)) (Integer 4);
    
  (* 4. Extended arithmetic operations *)
  Printf.printf "\n----- Arithmetic Extensions SECD Tests -----\n";
  
  test_secd "Basic addition" 
    Extension.add_test (Integer 42);
    
  test_secd "Basic subtraction" 
    Extension.sub_test (Integer (-10));
    
  test_secd "Basic multiplication" 
    Extension.mult_test (Integer 0);
    
  test_secd "Nested addition" 
    Extension.nested_add (Integer 42);
    
  test_secd "Nested subtraction" 
    Extension.nested_sub (Integer 48);
    
  test_secd "Nested multiplication" 
    Extension.nested_mult (Integer 24);
    
  test_secd "Mixed addition and subtraction" 
    Extension.mixed_add_sub (Integer 42);
    
  test_secd "Mixed subtraction and multiplication" 
    Extension.mixed_sub_mult (Integer 42);
    
  test_secd "Mixed addition and multiplication" 
    Extension.mixed_add_mult (Integer 42);
    
  test_secd "Complex expression" 
    Extension.complex_expr (Integer 50);
    
  test_secd "Arithmetic in lambda application" 
    Extension.apply_arithmetic (Integer 42);
    
  test_secd "Nested lambda with arithmetic" 
    Extension.nested_lambda_arith (Integer 42);;

  (* Boolean operations tests *)
  Printf.printf "\n----- Boolean Logic SECD Tests -----\n";
  
  test_secd "Basic NOT true" 
    Extension.not_true (Boolean false);
    
  test_secd "Basic NOT false" 
    Extension.not_false (Boolean true);
    
  test_secd "Nested NOT operation" 
    Extension.not_complex (Boolean false);
    
  test_secd "AND true,true" 
    Extension.and_true_true (Boolean true);
    
  test_secd "AND true,false" 
    Extension.and_true_false (Boolean false);
    
  test_secd "AND false,true" 
    Extension.and_false_true (Boolean false);
    
  test_secd "AND false,false" 
    Extension.and_false_false (Boolean false);
    
  test_secd "OR true,true" 
    Extension.or_true_true (Boolean true);
    
  test_secd "OR true,false" 
    Extension.or_true_false (Boolean true);
    
  test_secd "OR false,true" 
    Extension.or_false_true (Boolean true);
    
  test_secd "OR false,false" 
    Extension.or_false_false (Boolean false);
    
  test_secd "Complex boolean expression 1" 
    Extension.complex_bool_expr1 (Boolean true);
    
  test_secd "Complex boolean expression 2" 
    Extension.complex_bool_expr2 (Boolean true);
    
  test_secd "Nested NOT with AND/OR" 
    Extension.nested_not (Boolean false);
    
  test_secd "Lambda with boolean operation" 
    Extension.lambda_bool_expr (Boolean false);

  (* ================= SECD Opcode Tests ================= *)

  Printf.printf "\n===== Extensions Opcode Tests =====\n";
  (* Basic arithmetic operations with opcode verification *)
  test_secd_with_opcodes "Basic addition" 
    Extension.add_test
    (Integer 42)
    [PushInt 15; PushInt 27; PLUS];
    
  test_secd_with_opcodes "Basic subtraction" 
    Extension.sub_test
    (Integer (-10))
    [PushInt 50; PushInt 60; MINUS];
    
  test_secd_with_opcodes "Basic multiplication" 
    Extension.mult_test
    (Integer 0)
    [PushInt 6; PushInt 0; MULT];
    
  (* Nested operations with opcode verification *)
  test_secd_with_opcodes "Nested addition" 
    Extension.nested_add
    (Integer 42)
    [PushInt 10; PushInt 15; PLUS; PushInt 17; PLUS];
    
  test_secd_with_opcodes "Nested subtraction" 
    Extension.nested_sub
    (Integer 48)
    [PushInt 50; PushInt 5; PushInt 3; MINUS; MINUS];
    
  test_secd_with_opcodes "Nested multiplication" 
    Extension.nested_mult
    (Integer 24)
    [PushInt 3; PushInt 4; PushInt 2; MULT; MULT];
    
  (* Mixed operations with opcode verification *)
  test_secd_with_opcodes "Mixed addition and subtraction" 
    Extension.mixed_add_sub
    (Integer 42)
    [PushInt 30; PushInt 20; PushInt 8; MINUS; PLUS];
    
  test_secd_with_opcodes "Mixed subtraction and multiplication" 
    Extension.mixed_sub_mult
    (Integer 42)
    [PushInt 50; PushInt 4; PushInt 2; MULT; MINUS];
    
  test_secd_with_opcodes "Lambda with arithmetic" 
    Extension.apply_arithmetic
    (Integer 42)
    [MkCLOS("x", [LOOKUP "x"; PushInt 10; PLUS; RET]); PushInt 32; APP];;

  (* λx.(λy.x * y) 6 *)
(* Compiled opcodes should reflect closure inside closure *)

  test_secd_with_opcodes "Nested lambda with arithmetic" 
    Extension.nested_lambda_arith
    (Integer 42)
    [
      MkCLOS("x", [
        MkCLOS("y", [LOOKUP "x"; LOOKUP "y"; MULT; RET]);
        PushInt 6; APP;
        RET
      ]);
      PushInt 7;
      APP
    ];

  Printf.printf "\n===== Boolean Logic Opcode Tests =====\n";
  
  test_secd_with_opcodes "NOT operation" 
    Extension.not_true
    (Boolean false)
    [PushBool true; NOT];
    
  test_secd_with_opcodes "AND operation" 
    Extension.and_true_false
    (Boolean false)
    [PushBool true; PushBool false; AND];
    
  test_secd_with_opcodes "OR operation" 
    Extension.or_false_true
    (Boolean true)
    [PushBool false; PushBool true; OR];
    
  test_secd_with_opcodes "Complex boolean expression" 
    Extension.complex_bool_expr1
    (Boolean true)
    [PushBool true; PushBool false; PushBool true; OR; AND];
    
  test_secd_with_opcodes "Lambda with boolean" 
    Extension.lambda_bool_expr
    (Boolean false)
    [MkCLOS("x", [LOOKUP "x"; PushBool true; AND; RET]); PushBool false; APP]

let () = run_secd_tests ()