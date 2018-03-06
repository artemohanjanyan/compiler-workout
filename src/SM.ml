open GT       
open Language

open List
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let eval config prog =
  let evalStep (stack, ((state, input, output) as sconfig)) instr =
    match instr with
      | BINOP op ->
        (match stack with
          | y :: x :: stack1 ->
            let res = (Language.Expr.eval state (Language.Expr.Binop (op, Language.Expr.Const x, Language.Expr.Const y)))
            in (res :: stack1, sconfig)
          | _ -> failwith "stack is too small")
      | CONST c -> (c :: stack, sconfig)
      | READ ->
        (match input with
          | i :: input1 -> (i :: stack, (state, input1, output))
          | _ -> failwith "not enough input")
      | WRITE ->
        (match stack with
          | s :: stack1 -> (stack1, (state, input, output @ [s]))
          | _ -> failwith "nothing to write")
      | LD var -> (state var :: stack, sconfig)
      | ST var ->
        (match stack with
          | s :: stack1 -> (stack1, (Language.Expr.update var s state, input, output))
          | _ -> failwith "nothing to store")
  in fold_left evalStep config prog

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compile stmt =
  let rec compileExpr expr =
    match expr with
      | Language.Expr.Const n -> [CONST n]
      | Language.Expr.Var varName -> [LD varName]
      | Language.Expr.Binop (op, a, b) -> compileExpr a @ compileExpr b @ [BINOP op]
  in match stmt with
    | Language.Stmt.Read varName -> [READ; ST varName]
    | Language.Stmt.Write expr -> compileExpr expr @ [WRITE]
    | Language.Stmt.Assign (varName, expr) -> compileExpr expr @ [ST varName]
    | Language.Stmt.Seq (prog1, prog2) -> compile prog1 @ compile prog2
