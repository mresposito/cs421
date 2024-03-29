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
  let help (tau':typeterm) ((tau,bndvars):typescheme) (subs:typeenv) :
    bool =
  match tau', tau with
    ( Typevar(a), Typevar(b)  ) -> a = b
  | ( IntType, Typevar(_) ) -> true
  | ( BoolType, Typevar(_)) -> true
  | ( BoolType, BoolType ) -> true
  | ( IntType, IntType   ) -> true
  | ( FunType( f1,f2 ), FunType(f3,f4) ) -> true
  | _ -> false

  in help tau' (tau, bndvars) []

(* generalize tau in gamma = i.e. find variables in tau that
   are not free in gamma and return a type scheme that binds them *)
let rec generalize (tau:typeterm) (gamma:typeenv) : typescheme =
  match tau with
  (* make better!  *)
  Typevar ( x ) -> ( Typevar (x), [x] )
  | FunType ( f1, f2  ) -> let ts1 =  generalize f1 gamma 
    and ts2 =  generalize f2 gamma 
    in ( FunType ( fst ts1, fst ts2 ), (snd ts1) @ ( snd ts2 ) )
  | t -> (t, [] )

let opTypes (bop:binary_operation) (tau1:typeterm) (tau2:typeterm) : typeterm =
  match bop, tau1, tau1 with
    (LessThan, IntType, IntType    ) -> BoolType
  | (GreaterThan, IntType, IntType ) -> BoolType
  | (And, BoolType, BoolType       ) -> BoolType
  | (Or, BoolType, BoolType        ) -> BoolType
  | (Plus , IntType, IntType       ) -> IntType
  | (Minus , IntType, IntType      ) -> IntType
  | (Div , IntType, IntType        ) -> IntType
  | (Mult, IntType, IntType        ) -> IntType
  | _ -> raise (TypeError("misapplied binary operation"))

let rec tcheck (e:exp) (gamma:typeenv) : typeterm =
  match e with
    Operation (e1, bop, e2    ) -> opTypes bop (tcheck e1 gamma) (tcheck e2 gamma)
  | True | False                -> BoolType
  | IntConst  (i              ) -> IntType
  | Var       (id             ) -> fst ( lookup id gamma )
  | PolyVar   (x, t         ) -> 
      if instanceOf t (lookup x gamma)
      then t
      else raise ( TypeError("Poly variable does not match") )
  | Let       (x, t, e , e' ) -> if t = tcheck e gamma
    then tcheck e' (extend x (t,[]) gamma)
    else raise(TypeError("Types dont match in let"))
  | Fun       (x, t, e      ) -> 
      FunType( t , tcheck e ( extend x (generalize t gamma) gamma ) )
  | App       (e1, e2         ) -> let t2 = tcheck e2 gamma
    and FunType ( t1', t1'' ) = tcheck e1 gamma
    in if t1' = t2 then t1''
    else raise(TypeError("Types dont match"))
  | _ -> raise (TypeError "not implemented")
