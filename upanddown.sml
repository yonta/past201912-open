fun inputNumber () =
    let
      val str = valOf (TextIO.inputLine TextIO.stdIn)
    in
      valOf (Int.fromString str)
    end handle Option.Option => raise Fail "ilegal input"
fun printDiff prev now =
    (if prev = now then print "stay"
     else if prev < now then print ("up " ^ Int.toString (now - prev))
     else print ("down " ^ Int.toString (prev - now));
     print "\n")
fun existNextInput () = Option.isSome (TextIO.lookahead TextIO.stdIn)
fun loop 0 _ = ()
  | loop day prev =
    let
      val input = inputNumber ()
      val () = printDiff prev input
    in
      if existNextInput () then loop (day - 1) input else ()
    end
fun main () =
    let
      val day = inputNumber ()
      val () = if day < 2 then raise Fail "not enough day" else ()
      val first = inputNumber ()
      val () = loop (day - 1) first
    in
      ()
    end
val () = main ()
