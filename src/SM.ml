open GT       

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
type config = int list * Syntax.Stmt.config

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
            let res = (Syntax.Expr.eval state (Syntax.Expr.Binop (op, Syntax.Expr.Const x, Syntax.Expr.Const y)))
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
          | s :: stack1 -> (stack1, (Syntax.Expr.update var s state, input, output))
          | _ -> failwith "nothing to store")
  in fold_left evalStep config prog

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compile stmt =
  let rec compileExpr expr =
    match expr with
      | Syntax.Expr.Const n -> [CONST n]
      | Syntax.Expr.Var varName -> [LD varName]
      | Syntax.Expr.Binop (op, a, b) -> compileExpr a @ compileExpr b @ [BINOP op]
  in match stmt with
    | Syntax.Stmt.Read varName -> [READ; ST varName]
    | Syntax.Stmt.Write expr -> compileExpr expr @ [WRITE]
    | Syntax.Stmt.Assign (varName, expr) -> compileExpr expr @ [ST varName]
    | Syntax.Stmt.Seq (prog1, prog2) -> compile prog1 @ compile prog2
