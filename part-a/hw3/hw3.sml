(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* #1 *)
fun only_capitals(xs) = 
  List.filter(fn s => Char.isUpper(String.sub(s, 0))) xs

(* #2 *)
fun longest_string1(xs) =
  List.foldl(fn (a, b) => if String.size(a) > String.size(b) then a else b) "" xs

(* #3 *)
fun longest_string2(xs) =
  List.foldl(fn (a, b) => if String.size(a) >= String.size(b) then a else b) "" xs

(* #4 *)
fun longest_string_helper f xs = 
  List.foldl(fn (a, b) => if f(String.size(a), String.size(b)) then a else b) "" xs

fun longest_string3(xs) =
  longest_string_helper (fn (a, b) => a > b) xs

fun longest_string4(xs) =
  longest_string_helper (fn (a, b) => a >= b) xs

(* #5 *)
val longest_capitalized =
  longest_string3 o only_capitals

(* #6 *)
val rev_string = 
  String.implode o List.rev o String.explode

(* #7 *)
fun first_answer f xs =
  case xs of
       [] => raise NoAnswer
     | x::xs' =>
         case f(x) of
              NONE => first_answer f xs'
            | SOME y => y

(* #8 *)
fun all_answers f xs =
  let
    fun helper(partial, acc) = 
      case partial of
        [] => SOME acc
      | x::xs' => 
          case f(x) of
               NONE => NONE
             | SOME y => helper(xs', acc @ y)
  in
    helper(xs, [])
  end

(* #9 *)

(* a *)
val count_wildcards =
  g (fn x => 1) (fn x => 0)

(* b *)
val count_wild_and_variable_lengths =
  g (fn x => 1) (fn x => String.size(x))

(* c *)
fun count_some_var(s, p) =
  g (fn x => 0) (fn x => if x = s then 1 else 0) p

(* #10 *)
val check_pat =
  let
    fun list_all_strings(p) =
      case p of
           Variable x => [x]
         | TupleP xs => List.foldl(fn(x, acc) => list_all_strings(x) @ acc) [] xs
         | ConstructorP(_, p') => list_all_strings(p')
         | _ => []
    fun list_has_repeats(xs) =
      case xs of
           [] => false
         | x::xs' => List.exists(fn y => x = y) xs' orelse list_has_repeats(xs')
  in
    not o list_has_repeats o list_all_strings
  end

(* #11 *)
fun match(v, p) =
  case (v, p) of
       (_, Wildcard) => SOME []
     | (v', Variable s) => SOME [(s, v')]
     | (Unit, UnitP) => SOME []
     | (Const x, ConstP y) => if x = y then SOME [] else NONE
     | (Tuple vs, TupleP ps) =>

         if length(vs) = length(ps) then
           let
             val pairs = ListPair.zip(vs, ps)
           in
             all_answers match pairs
           end
         else
           NONE
     | (Constructor(s2, v'), ConstructorP(s1, p')) =>
         if s1 = s2 then match(v', p') else NONE
     | _ => NONE

(* #12 *)
fun first_match v ps =
  SOME (first_answer (fn p => match(v, p)) ps) handle NoAnswer => NONE
