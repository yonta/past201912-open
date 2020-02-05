fun main () =
    let
      val s = TextIO.input TextIO.stdIn
      fun twice x = 2 * x
      val () = if List.exists Char.isAlpha (String.explode s)
               then print "error"
               else (print o Int.toString o twice o valOf o Int.fromString) s
    in
      ()
    end
val () = main ()
