(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT
open Ostap

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
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
    
    let run op = 
        match op with
        | "+"  -> (+)
        | "-"  -> (-)
        | "*"  -> ( * )
        | "/"  -> (/)
        | "%"  -> (mod)
        | "==" -> fun x y -> if x = y  then 1 else 0
        | "!=" -> fun x y -> if x <> y then 1 else 0
        | "<=" -> fun x y -> if x <= y then 1 else 0
        | "<"  -> fun x y -> if x < y  then 1 else 0
        | ">=" -> fun x y -> if x >= y then 1 else 0
        | ">"  -> fun x y -> if x > y  then 1 else 0
        | "!!" -> fun x y -> if x <> 0 || y <> 0 then 1 else 0
        | "&&" -> fun x y -> if x <> 0 && y <> 0 then 1 else 0
        |  _   -> failwith (Printf.sprintf "Undefined operator %s" op)

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
     *)                                                       
    let rec eval s e = 
        match e with
        | Const c -> c
        | Var v -> s v
        | Binop (op, x, y) -> run op (eval s x) (eval s y)
    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    ostap (                                      
      expr:
        !(Util.expr
            (fun x -> x)
            [|
                `Lefta, [ 
                    ostap ("!!"), (fun x y -> Binop("!!", x, y)) 
                ];
                `Lefta, [ 
                    ostap ("&&"), (fun x y -> Binop("&&", x, y))
                ];
                `Nona, [
                    ostap ("!="), (fun x y -> Binop("!=", x, y)); 
                    ostap ("=="), (fun x y -> Binop("==", x, y)); 
                    ostap ("<="), (fun x y -> Binop("<=", x, y)); 
                    ostap ("<"),  (fun x y -> Binop("<", x, y)); 
                    ostap (">="), (fun x y -> Binop(">=", x, y)); 
                    ostap (">"),  (fun x y -> Binop(">", x, y))
                ];
                `Lefta, [ 
                    ostap ("+"),  (fun x y -> Binop("+", x, y)); 
                    ostap ("-"),  (fun x y -> Binop("-", x, y))
                ];
                `Lefta , [ 
                    ostap ("*"),  (fun x y -> Binop("*", x, y)); 
                    ostap ("/"),  (fun x y -> Binop("/", x, y)); 
                    ostap ("%"),  (fun x y -> Binop("%", x, y))
                 ]
            |]
            primary
        );
      
      primary: x:IDENT {Var x} | n:DECIMAL {Const n} | -"(" expr -")"
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
    let rec eval ((s, i, o) as cfg) smt = 
        match smt with 
        | Read v -> let hd :: tl = i in (Expr.update v hd s, tl, o)
        | Write e -> let res = Expr.eval s e in (s, i, o @ [res])
        | Assign (v, e) -> let res = Expr.eval s e in (Expr.update v res s, i, o)
        | Seq (a, b) -> eval (eval cfg a) b 
        
    (* Statement parser *)
    ostap (
        simple_stmt:
            x :IDENT ":=" e:!(Expr.expr) {Assign (x, e)}
            | "read" "(" x:IDENT ")" {Read x}
            | "write" "(" e:!(Expr.expr) ")" {Write e};
        parse: <s::ss> : !(Util.listBy)[ostap (";")][simple_stmt] {List.fold_left (fun s ss -> Seq (s, ss)) s ss} 
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

(* Top-level parser *)
let parse = Stmt.parse                                                     
