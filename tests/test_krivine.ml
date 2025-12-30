(* Tests for Krivine machine *)
open Claude_krivine

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
              
  (* Church predecessor (complex!): λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u) *)
  let pred = Lambda("n", Lambda("f", Lambda("x",
              App(App(App(Identifier "n", 
                          Lambda("g", Lambda("h", App(Identifier "h", App(Identifier "g", Identifier "f"))))),
                      Lambda("u", Identifier "x")),
                  Lambda("u", Identifier "u")))))
                  
  (* Church equality test for zero *)
  let is_zero = Lambda("n", App(App(Identifier "n", Lambda("x", Identifier "false_c")), Identifier "true_c"))
  
  (* Church conditional: If = λp.λa.λb.p a b *)
  let cond = Lambda("p", Lambda("a", Lambda("b", 
              App(App(Identifier "p", Identifier "a"), Identifier "b"))))
end

(* Combinators *)
module Combinators = struct
  (* The Y combinator for recursion: Y = λf.(λx.f(x x))(λx.f(x x)) *)
  let y_comb = Lambda("f", 
                 App(Lambda("x", App(Identifier "f", App(Identifier "x", Identifier "x"))),
                     Lambda("x", App(Identifier "f", App(Identifier "x", Identifier "x")))))
                     
  (* The fixed-point combinator Z (call-by-value variant of Y)
     Z = λf.(λx.f(λv.x x v))(λx.f(λv.x x v)) *)
  let z_comb = Lambda("f",
                 App(Lambda("x", App(Identifier "f", Lambda("v", App(App(Identifier "x", Identifier "x"), Identifier "v")))),
                     Lambda("x", App(Identifier "f", Lambda("v", App(App(Identifier "x", Identifier "x"), Identifier "v"))))))
                     
  (* Identity: I = λx.x *)
  let id = Lambda("x", Identifier "x")
  
  (* Constant function: K = λx.λy.x *)
  let k = Lambda("x", Lambda("y", Identifier "x"))
  
  (* Function composition: B = λf.λg.λx.f(g x) *)
  let b = Lambda("f", Lambda("g", Lambda("x", 
            App(Identifier "f", App(Identifier "g", Identifier "x")))))
            
  (* Self-application: ω = λx.x x *)
  let omega = Lambda("x", App(Identifier "x", Identifier "x"))
  
  (* The famous Ω (omega-omega) that causes infinite recursion: Ω = ω ω *)
  let big_omega = App(omega, omega)
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
  let sub_test = Subtraction(Integer 50, Integer 60)    (* 50 -60 = -10 *)
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
  
  (* AND operations with short-circuit behavior *)
  let and_true_true = And(Boolean true, Boolean true)     (* true && true = true *)
  let and_true_false = And(Boolean true, Boolean false)   (* true && false = false *)
  let and_false_true = And(Boolean false, Boolean true)   (* false && true = false, short-circuits *)
  let and_false_false = And(Boolean false, Boolean false) (* false && false = false, short-circuits *)
  
  (* Short-circuit with expressions *)
  let and_short_circuit = And(Boolean false, 
                             App(Lambda("x", Boolean false), Combinators.big_omega))
                             (* false && (infinite loop) = false, shouldn't hang *)
  
  (* OR operations with short-circuit behavior *)
  let or_true_true = Or(Boolean true, Boolean true)       (* true || true = true, short-circuits *)
  let or_true_false = Or(Boolean true, Boolean false)     (* true || false = true, short-circuits *)
  let or_false_true = Or(Boolean false, Boolean true)     (* false || true = true *)
  let or_false_false = Or(Boolean false, Boolean false)   (* false || false = false *)
  
  (* Short-circuit with expressions *)
  let or_short_circuit = Or(Boolean true, 
                           App(Lambda("x", Boolean true), Combinators.big_omega))
                           (* true || (infinite loop) = true, shouldn't hang *)
  
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

(* Execute test and print result *)
let test_krivine name expr expected =
  let result = evaluate expr in
  let result_str = string_of_term result in
  let expected_str = string_of_term expected in
  Printf.printf "[Krivine] Test %s: %s\n" name 
    (if result_str = expected_str then "PASSED" else 
      Printf.sprintf "FAILED\n  Expected: %s\n  Got: %s" expected_str result_str)

(* ================= Krivine Machine Tests ================= *)

let run_krivine_tests () = 
  Printf.printf "\n========== KRIVINE MACHINE TESTS ==========\n";
  
  (* 1. Basic lambda calculus properties *)
  test_krivine "Identity function" 
    (App(Combinators.id, Integer 5)) (Integer 5);
    
  test_krivine "K combinator test" 
    (App(App(Combinators.k, Integer 3), Integer 7)) (Integer 3);
    
  (* 2. Church numerals *)
  test_krivine "Church numeral definition" 
    Church.two Church.two;
    
  test_krivine "Church successor (0->1)" 
    (App(Church.succ, Church.zero)) Church.one;
    
  test_krivine "Church addition (1+2=3)" 
    (App(App(Church.add, Church.one), Church.two)) Church.three;
    
  (* 3. Church booleans *)
  test_krivine "Church true selector" 
    (App(App(Church.true_c, Integer 1), Integer 2)) (Integer 1);
    
  test_krivine "Church false selector" 
    (App(App(Church.false_c, Integer 1), Integer 2)) (Integer 2);
    
  test_krivine "Church conditional true case" 
    (App(App(App(Church.cond, Church.true_c), Integer 10), Integer 20)) (Integer 10);
    
  test_krivine "Church conditional false case" 
    (App(App(App(Church.cond, Church.false_c), Integer 10), Integer 20)) (Integer 20);
    
  (* 4. Built-in primitives *)
  test_krivine "Built-in addition" 
    (Addition(Integer 5, Integer 7)) (Integer 12);
    
  test_krivine "Built-in conditional (true)" 
    (If(Boolean true, Integer 5, Integer 10)) (Integer 5);
    
  test_krivine "Built-in conditional (false)" 
    (If(Boolean false, Integer 5, Integer 10)) (Integer 10);
    
  (* 5. Pairs *)
  let test_pair = App(App(Pairs.pair, Integer 3), Integer 4) in
  
  test_krivine "Church pair first element" 
    (App(Pairs.fst, test_pair)) (Integer 3);
    
  test_krivine "Church pair second element" 
    (App(Pairs.snd, test_pair)) (Integer 4);
    
  (* 6. Lazy evaluation specific tests - should work well in Krivine *)
  let omega_with_escape = App(Lambda("x", Integer 42), Combinators.big_omega) in
  test_krivine "Non-strict evaluation with omega" 
    omega_with_escape (Integer 42);
    
  (* 8. Extended arithmetic operations *)
  Printf.printf "\n----- Arithmetic Extensions KRIVINE Tests -----\n";
  
  test_krivine "Basic addition" 
    Extension.add_test (Integer 42);
    
  test_krivine "Basic subtraction" 
    Extension.sub_test (Integer (-10));
    
  test_krivine "Basic multiplication" 
    Extension.mult_test (Integer 0);
    
  test_krivine "Nested addition" 
    Extension.nested_add (Integer 42);
    
  test_krivine "Nested subtraction" 
    Extension.nested_sub (Integer 48);
    
  test_krivine "Nested multiplication" 
    Extension.nested_mult (Integer 24);
    
  test_krivine "Mixed addition and subtraction" 
    Extension.mixed_add_sub (Integer 42);
    
  test_krivine "Mixed subtraction and multiplication" 
    Extension.mixed_sub_mult (Integer 42);
    
  test_krivine "Mixed addition and multiplication" 
    Extension.mixed_add_mult (Integer 42);
    
  test_krivine "Complex expression" 
    Extension.complex_expr (Integer 50);
    
  test_krivine "Arithmetic in lambda application" 
    Extension.apply_arithmetic (Integer 42);
    
  test_krivine "Nested lambda with arithmetic" 
    Extension.nested_lambda_arith (Integer 42);
    
  (* Boolean operations tests *)
  Printf.printf "\n----- Boolean Logic KRIVINE Tests -----\n";
  
  test_krivine "Basic NOT true" 
    Extension.not_true (Boolean false);
    
  test_krivine "Basic NOT false" 
    Extension.not_false (Boolean true);
    
  test_krivine "Nested NOT operation" 
    Extension.not_complex (Boolean false);
    
  test_krivine "AND true,true" 
    Extension.and_true_true (Boolean true);
    
  test_krivine "AND true,false" 
    Extension.and_true_false (Boolean false);
    
  test_krivine "AND false,true (short-circuit)" 
    Extension.and_false_true (Boolean false);
    
  test_krivine "AND false,false (short-circuit)" 
    Extension.and_false_false (Boolean false);
    
  test_krivine "AND short-circuit with omega" 
    Extension.and_short_circuit (Boolean false);
    
  test_krivine "OR true,true (short-circuit)" 
    Extension.or_true_true (Boolean true);
    
  test_krivine "OR true,false (short-circuit)" 
    Extension.or_true_false (Boolean true);
    
  test_krivine "OR false,true" 
    Extension.or_false_true (Boolean true);
    
  test_krivine "OR false,false" 
    Extension.or_false_false (Boolean false);
    
  test_krivine "OR short-circuit with omega" 
    Extension.or_short_circuit (Boolean true);
    
  test_krivine "Complex boolean expression 1" 
    Extension.complex_bool_expr1 (Boolean true);
    
  test_krivine "Complex boolean expression 2" 
    Extension.complex_bool_expr2 (Boolean true);
    
  test_krivine "Nested NOT with AND/OR" 
    Extension.nested_not (Boolean false);
    
  test_krivine "Lambda with boolean operation" 
    Extension.lambda_bool_expr (Boolean false);;

let () = run_krivine_tests ()