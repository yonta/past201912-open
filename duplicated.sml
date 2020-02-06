signature RED_BLACK_SET =
sig
  type set
  val empty : set
  val insert : int * set -> set
  exception Duplicate of int
end
structure RedBlackSet : RED_BLACK_SET =
struct
  datatype color = R | B
  datatype tree = E | T of color * tree * int * tree
  type set = tree
  exception Duplicate of int
  val empty = E
  fun lbalance (B,T (R,T (R,a,x,b),y,c),z,d) = T (R,T (B,a,x,b),y,T (B,c,z,d))
    | lbalance (B,T (R,a,x,T (R,b,y,c)),z,d) = T (R,T (B,a,x,b),y,T (B,c,z,d))
    | lbalance body = T body
  fun rbalance (B,a,x,T (R,T (R,b,y,c),z,d)) = T (R,T (B,a,x,b),y,T (B,c,z,d))
    | rbalance (B,a,x,T (R,b,y,T (R,c,z,d))) = T (R,T (B,a,x,b),y,T (B,c,z,d))
    | rbalance body = T body
  fun insert (x, s) =
    let
      fun ins E = T (R, E, x, E)
        | ins (s as T (color, a, y, b)) =
          if x < y then lbalance (color, ins a, y ,b)
          else if y < x then rbalance (color, a, y, ins b)
          else raise Duplicate x
    in
      case ins s of
          T (_, a, y, b) => T (B, a, y, b)
        | E => raise Fail "bug: not inserted"
    end
end

(*
 * array-sort
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 * See https://www.smlnj.org/license.html
 *)
signature ARRAY_SORT =
sig
    type 'a array
    val sort   : ('a * 'a -> order) -> 'a array -> unit
end
structure Unsafe = struct
  structure Array = struct
    val sub = SMLSharp_Builtin.Array.sub_unsafe
    val update = SMLSharp_Builtin.Array.update_unsafe
  end
end
structure ArrayQSort : ARRAY_SORT =
struct
  structure A = Array
  type 'a array = 'a A.array
  val sub = Unsafe.Array.sub
  val update = Unsafe.Array.update
  fun sortRange (array, start, n, cmp) = let
    fun item i = sub(array,i)
    fun swap (i,j) = let
      val tmp = sub(array,i)
    in update(array,i,sub(array,j)); update(array,j,tmp) end
    fun vecswap (i,j,0) = ()
      | vecswap (i,j,n) = (swap(i,j);vecswap(i+1,j+1,n-1))
    fun insertSort (start, n) = let
      val limit = start+n
      fun outer i =
          if i >= limit then ()
          else let
            fun inner j =
                if j = start then outer(i+1)
                else let
                  val j' = j - 1
                in
                  if cmp(item j',item j) = GREATER
                  then (swap(j,j'); inner j')
                  else outer(i+1)
                end
          in inner i end
    in
      outer (start+1)
    end
    fun med3(a,b,c) = let
      val a' = item a and b' = item b and c' = item c
    in
      case (cmp(a', b'),cmp(b', c'))
       of (LESS, LESS) => b
        | (LESS, _) => (
          case cmp(a', c') of LESS => c | _ => a)
        | (_, GREATER) => b
        | _ => (case cmp(a', c') of LESS => a | _ => c)
                 (* end case *)
    end
    fun getPivot (a,n) =
        if n <= 7 then a + n div 2
        else let
          val p1 = a
          val pm = a + n div 2
          val pn = a + n - 1
        in
          if n <= 40 then med3(p1,pm,pn)
          else let
            val d = n div 8
            val p1 = med3(p1,p1+d,p1+2*d)
            val pm = med3(pm-d,pm,pm+d)
            val pn = med3(pn-2*d,pn-d,pn)
          in
            med3(p1,pm,pn)
          end
        end
    fun quickSort (arg as (a, n)) = let
      fun bottom limit = let
        fun loop (arg as (pa,pb)) =
            if pb > limit then arg
            else case cmp(item pb,item a) of
                     GREATER => arg
                   | LESS => loop (pa,pb+1)
                   | _ => (swap arg; loop (pa+1,pb+1))
      in loop end
      fun top limit = let
        fun loop (arg as (pc,pd)) =
            if limit > pc then arg
            else case cmp(item pc,item a) of
                     LESS => arg
                   | GREATER => loop (pc-1,pd)
                   | _ => (swap arg; loop (pc-1,pd-1))
      in loop end
      fun split (pa,pb,pc,pd) = let
        val (pa,pb) = bottom pc (pa,pb)
        val (pc,pd) = top pb (pc,pd)
      in
        if pb > pc then (pa,pb,pc,pd)
        else (swap(pb,pc); split(pa,pb+1,pc-1,pd))
      end
      val pm = getPivot arg
      val _ = swap(a,pm)
      val pa = a + 1
      val pc = a + (n-1)
      val (pa,pb,pc,pd) = split(pa,pa,pc,pc)
      val pn = a + n
      val r = Int.min(pa - a, pb - pa)
      val _ = vecswap(a, pb-r, r)
      val r = Int.min(pd - pc, pn - pd - 1)
      val _ = vecswap(pb, pn-r, r)
      val n' = pb - pa
      val _ = if n' > 1 then sort(a,n') else ()
      val n' = pd - pc
      val _ = if n' > 1 then sort(pn-n',n') else ()
    in () end
    and sort (arg as (_, n)) = if n < 7 then insertSort arg
                               else quickSort arg
  in sort (start,n) end
  fun sort cmp array = sortRange(array, 0, A.length array, cmp)
end (* ArraySort *)
fun arrayToList array = Array.foldr (op ::) nil array

infix &&>
fun op &&> (reader, f) input =
    case reader input of
        NONE => NONE
      | SOME (result, rest) => SOME (result, f rest)
fun ** reader input =
    case reader input of
        NONE => NONE
      | SOME (result, rest) =>
        case ** reader rest of
            NONE => SOME ([result], rest)
          | SOME (results, rest2) => SOME (result :: results, rest2)
fun parse reader instream =
    let
      val results = reader instream
    in
      case results of
          NONE => raise Fail "bug: invalid input format"
        | SOME (p as (inputs, rest)) => p
    end

fun getDuplicated _ (SOME dupli, SOME none) _ = SOME (dupli, none)
  | getDuplicated _ _ nil = NONE
  | getDuplicated expected (NONE, NONE) [input] =
    (* special pattern, like [1, 2, 3, 3] *)
    if input = expected - 1 then SOME (input, expected)
    else if input = expected then NONE
    else raise Fail "bug: invalid input"
  | getDuplicated expected (SOME d, NONE) [input] =
    if input = expected then SOME (d, expected + 1)
    else if input = expected + 1 then SOME (d, expected)
    else raise Fail "bug: invalid input, only duplicated"
  | getDuplicated expected (dupli, none) (input :: inputs) =
    if input = expected
    then getDuplicated (expected + 1) (dupli, none) inputs
    else if input = expected + 1
    then getDuplicated (expected + 2) (dupli, SOME expected) inputs
    else if input = expected - 1
    then getDuplicated expected (SOME input, none) inputs
    else raise Fail "bug: not sorted input"
fun checkDuplicated N inputs =
    let
      val array = Array.fromList inputs
      val () = ArrayQSort.sort Int.compare array
      val sorted = arrayToList array
    in
      case getDuplicated 1 (NONE, NONE) sorted of
          NONE => print "Correct"
        | SOME (dupli, none) =>
          print ((Int.toString dupli) ^ " " ^ (Int.toString none))
    end

fun main () =
    let
      val io = TextIO.stdIn
      val instream = TextIO.getInstream io
      val getc = TextIO.StreamIO.input1
      val readInt = Int.scan StringCvt.DEC getc
      val skipWS = StringCvt.skipWS getc
      val intSpace = readInt &&> skipWS
      val (N, instream) = parse intSpace instream
      val (inputs, instream) = parse ( ** intSpace) instream
      val () = TextIO.setInstream (io, instream)
    in
      checkDuplicated N inputs
    end
val () = main ()
