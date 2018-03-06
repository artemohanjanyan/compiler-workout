(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Ostap.Combinators

open List
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
    let rec eval s e =
      let fromInt = function
        | 0 -> false
        | _ -> true
      and toInt = function
        | false -> 0
        | true -> 1 in
      match e with
        | Const (x) -> x
        | Var (varName) -> s varName
        | Binop (op, l, r) ->
          let left  = eval s l
          and right = eval s r
          in match op with
            | "!!" -> toInt (fromInt left || fromInt right)
            | "&&" -> toInt (fromInt left && fromInt right)
            | "==" -> toInt (left == right)
            | "!=" -> toInt (left != right)
            | "<=" -> toInt (left <= right)
            | "<"  -> toInt (left <  right)
            | ">=" -> toInt (left >= right)
            | ">"  -> toInt (left >  right)
            | "+"  -> left  +  right
            | "-"  -> left  -  right
            | "*"  -> left  *  right
            | "/"  -> left  /  right
            | "%"  -> left mod right
            | _    -> failwith "unknown binary operator"

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
   
    *)
    ostap (
      parse:
        !(let binops = map (fun op -> ostap ($(op)), fun l r -> Binop (op, l, r))
          in Util.expr
            (fun x -> x)
            [|
               `Lefta, binops ["!!"];
               `Lefta, binops ["&&"];
               `Nona , binops [">="; ">"; "<="; "<"; "=="; "!="];
               `Lefta, binops ["+"; "-"];
               `Lefta, binops ["*"; "/"; "%"]
            |]
            primary
        );
      primary: x:IDENT {Var x} | x:DECIMAL {Const x} | -"(" parse -")"
    )

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval ((state, input, output) as config) stmt =
      match stmt with
        | Read (varName) -> (Expr.update varName (hd input) state, tl input, output)
        | Write (expr) -> (state, input, output @ [Expr.eval state expr])
        | Assign (varName, expr) -> (Expr.update varName (Expr.eval state expr) state, input, output)
        | Seq (prog1, prog2) ->
          let config1 = eval config prog1
          in eval config1 prog2

    (* Statement parser *)
    ostap (
      stmt  : read | write | assign;
      read  : "read"  "(" varName:IDENT      ")" {Read  varName};
      write : "write" "(" expr:!(Expr.parse) ")" {Write expr   };
      assign: varName:IDENT ":=" expr:!(Expr.parse) {Assign (varName, expr)};
      parse : <s::ss> :!(Util.listBy)[ostap (";")][stmt] {
        fold_left (fun a b -> Seq (a, b)) s ss (* use fold_right *)
      }
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
