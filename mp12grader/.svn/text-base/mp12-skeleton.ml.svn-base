open List;;
open Miniocamlast;;
open Mp12common;;

(* union set1 set2 returns set1@set2 without repetitions, assuming
   set1 and set2 do not have repetitions *)
let union set1 set2 = 
   fold_right (fun x s -> if mem x set2 then s else x::s) set1 set2;;

(* string_of_typeterm returns a "pretty-printed" version of an exp *)
let rec string_of_typeterm (t:typeterm) : string = match t with
     IntType -> "int"
   | BoolType -> "bool"
   | Typevar s -> "var " ^ s
   | FunType (t1, t2) -> "(" ^ (string_of_typeterm t1) ^ "->"
                             ^ (string_of_typeterm t2) ^ ")"

(* Return all the free type variables occurring in a typeterm,
   without repetitions *)
let rec getvars (t:typeterm) : string list = match t with
     IntType | BoolType -> []
   | Typevar s -> [s]
   | FunType(t1,t2) -> union (getvars t1) (getvars t2)

(* typeenv is a map from program variables to type schemes *)
type typeenv = (string * typescheme) list
and typescheme = typeterm * (string list)

(* Printable version of typescheme - for debugging *)
let string_of_typescheme ((t,lis):typescheme) : string =
   (string_of_typeterm t) ^ "[" ^
        fold_right (fun i s -> i ^ " " ^ s) lis "]"

(* Basic operations on type environments.  lookup throws a
   Not_found exception if the variable is absent. *)
let empty_te = []

let lookup (x:string) (gamma:typeenv) : typescheme =
   snd (find (fun (x',_) -> x=x') gamma)

let extend (s:string) (ts:typescheme) (gamma:typeenv) : typeenv
     = (s,ts)::gamma

(* Return the list of type variables free in a type scheme -
   for type scheme (tau,bndvars), this is just the list of
   free variables return by getvars, minus variables in bndvars. *)
let rec freevars ((tau,bndvars):typescheme) : string list =
    let tauvars = getvars tau
    in filter (fun s -> not (mem s bndvars)) tauvars

(* instanceOf tau' ts - determine if tau' is an instance of ts,
   i.e. tau' is the same as ts except that bound variables in ts
   are replaced by types.  In addition, every occurrence of a
   single type variable must be replaced by the same type. *)
let instanceOf (tau':typeterm) ((tau,bndvars):typescheme) : bool =
  raise (TypeError "not implemented")

(* generalize tau in gamma = i.e. find variables in tau that
   are not free in gamma and return a type scheme that binds them *)
let generalize (tau:typeterm) (gamma:typeenv) : typescheme =
  raise (TypeError "not implemented")

let opTypes (bop:binary_operation) (tau1:typeterm) (tau2:typeterm) : typeterm =
  raise (TypeError "not implemented")

let rec tcheck (e:exp) (gamma:typeenv) : typeterm =
  raise (TypeError "not implemented")
