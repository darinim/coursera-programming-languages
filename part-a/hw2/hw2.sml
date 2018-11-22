(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* ------------- QUESTION 1 ---------------- *)
(* A *)
fun all_except_option(s : string, xs : string list) =
  let
    fun helper(partial: string list, acc : string list) =
      case partial of
           [] => NONE
         | head::tail  =>
            case same_string(head, s) of
                true => SOME(acc @ tail)
              | false => helper(tail, acc @ [head])
  in
    helper(xs, [])
  end

(* B *)
fun get_substitutions1(xs : string list list, s : string) = 
  case xs of
       [] => []
     | head::tail =>
         case all_except_option(s, head) of
              NONE => get_substitutions1(tail, s)
            | SOME x => get_substitutions1(tail, s) @ x

(* C *)
fun get_substitutions2(xs : string list list, s : string) = 
  let
    fun helper(partial : string list list, acc : string list) =
      case partial of
           [] => acc
         | head::tail =>
             case all_except_option(s, head) of
                  NONE => helper(tail, acc)
                | SOME x => helper(tail, acc @ x)
  in
    helper(xs, [])
  end

(* D *)
fun similar_names(xs : string list list, {first=f, middle=m, last=l}) =
  let
    val names = get_substitutions2(xs, f)
    fun helper(partial : string list, acc : {first:string, middle:string, last:string} list) =
      case partial of
           [] => acc
         | head::tail => helper(tail, acc @ [{first=head, middle=m, last=l}])
  in
    {first=f, middle=m, last=l}::helper(names, [])
  end



(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove


(* ------------- QUESTION 2 ---------------- *)
(* A *)
fun card_color(c : card) =
  case c of
    (Clubs, _) => Black
   | (Spades, _) => Black
   | (Diamonds, _) => Red
   | (Hearts, _) => Red

(* B *)
fun card_value(c : card) = 
  case c of
    (_, Jack) => 10
   | (_, Queen) => 10
   | (_, King) => 10
   | (_, Ace) => 11
   | (_, Num(i)) => i

(* C *)
fun remove_card(cs : card list, c : card, e : exn) =
  let
    fun helper(partial : card list, acc : card list) =
      case partial of
           [] => raise e
         | head::tail =>
             case head = c of
                  true => acc @ tail
                | false => helper(tail, acc @ [head])
  in
    helper(cs, [])
  end

(* D *)
fun all_same_color(cs : card list) = 
  case cs of
       [] => true
     | head::[] => true
     | head::middle::tail =>
         case card_color(head) = card_color(middle) of
           true => all_same_color(middle::tail)
         | false => false

(* E *)
fun sum_cards(cs : card list) =
  case cs of
       [] => 0
     | head::tail => card_value(head) + sum_cards(tail)

(* F *)
fun score(cs : card list, goal : int) =
  let
    val sum = sum_cards(cs)
    val is_same_color = all_same_color(cs)
  in
    case (sum > goal, is_same_color) of
         (true, true)   => 3 * (sum - goal) div 2
       | (true, false)  => 3 * (sum - goal)
       | (false, true)  => (goal - sum) div 2
       | (false, false) => (goal - sum)
  end

(* G *)
fun officiate(cs : card list, ms : move list, goal : int) =
  let
    fun play(cards, moves, held) = 
      case (cards, moves) of
         (_, []) => score(held, goal)
       | (_, Discard(c)::ms') => play(cards, ms', remove_card(held, c, IllegalMove))
       | ([], Draw::ms') => score(held, goal)
       | (c::cs', Draw::ms') =>
           let
             val new_held = c::held
           in
             case sum_cards(new_held) > goal of
                  true => score(new_held, goal)
                | false => play(cs', ms', new_held)
           end
  in
    play(cs, ms, [])
  end
