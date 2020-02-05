structure Input =
struct
  fun inputLine () =
      case TextIO.inputLine TextIO.stdIn of
          NONE => ""
       | SOME s => s
end

fun test f filename =
    let
      (* val () = print "========== start ==========\n" *)
      val originalIns = TextIO.getInstream TextIO.stdIn
      val fileIns = TextIO.getInstream (TextIO.openIn (filename))
      val () = TextIO.setInstream (TextIO.stdIn, fileIns)
      val () = f ()
      val () = TextIO.setInstream (TextIO.stdIn, originalIns)
    in
      (* print "\n========== end ==========\n" *)
      print "\n"
    end

(*
 * scan : (char, Substring.substring) StringCvt.reader ->
 *          ('a, Substring.substring) StringCvt.reader)
 *)
fun makeStringReader scan str =
    let
      val substr = Substring.full str
      val reader = scan Substring.getc substr
    in
      Option.map (fn (a, b) => (a, Substring.string b)) reader
    end
infix && || &&> ||>
fun op && (reader1, reader2) input =
    case reader1 input of
        NONE => NONE
      | SOME (result1, rest) =>
        (case reader2 rest of
             NONE => NONE
           | SOME (result2, rest) => SOME ((result1, result2), rest))
fun op || (reader1, reader2) input =
    case reader1 input of
        NONE => (case reader2 input of
                     NONE => NONE
                   | r2 as SOME _ => r2)
      | r1 as SOME _ => r1
fun op &&> (reader, f) input =
    case reader input of
        NONE => NONE
      | SOME (result, rest) => SOME (result, f rest)
fun op ||> (reader, f) base input =
    case reader input of
        NONE => SOME (base, f input)
      | r as SOME _ => r
fun ** reader input =
    case reader input of
        NONE => SOME (nil, input)
      | SOME (result1, rest1) =>
        (case reader rest1 of
             SOME (result2, rest2) => SOME (result1 @ result2, rest2)
           | NONE => raise Fail "bug: reader returns NONE")
fun listReader nil input = SOME (nil, input)
  | listReader (reader :: readers) input =
    case reader input of
        NONE => NONE
      | SOME (result, rest) =>
        case listReader readers rest of
            NONE => NONE
          | SOME (results, rest2) => SOME (result :: results, rest2)
fun showResult NONE = NONE
  | showResult (SOME (result, substr)) = SOME (result, Substring.string substr)
(* example *)
val _ =
    let
      val getc = Substring.getc
      val intReader = Int.scan StringCvt.DEC getc
      val skipWS = StringCvt.skipWS getc
      val oneInt = intReader &&> skipWS
      val readers = [oneInt, oneInt, oneInt, oneInt, oneInt, intReader]
    in
      listReader readers
    end

datatype 'a readers =
         RInt of 'a -> (int * 'a) option
       | RSpace of 'a -> (char * 'a) option
fun scanHalfSpace baseReader input =
    case baseReader input of
        NONE => NONE
      | r as SOME (#" ", _) => r
      | SOME _ => NONE
val readers : substring readers list =
    [
      RInt (Int.scan StringCvt.DEC Substring.getc),
      RSpace (scanHalfSpace Substring.getc),
      RInt (Int.scan StringCvt.DEC Substring.getc),
      RSpace (scanHalfSpace Substring.getc),
      RInt (Int.scan StringCvt.DEC Substring.getc),
      RSpace (scanHalfSpace Substring.getc),
      RInt (Int.scan StringCvt.DEC Substring.getc),
      RSpace (scanHalfSpace Substring.getc),
      RInt (Int.scan StringCvt.DEC Substring.getc),
      RSpace (scanHalfSpace Substring.getc),
      RInt (Int.scan StringCvt.DEC Substring.getc)
    ]
