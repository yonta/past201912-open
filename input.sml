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
      val _ = f () handle Fail str => print ("FAIL:" ^ str)
      val () = TextIO.closeIn TextIO.stdIn
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
infix && || &&> >&& ||>
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
fun op >&& (f, reader) input = reader (f input)
fun op ||> (reader, f) base input =
    case reader input of
        NONE => SOME (base, f input)
      | r as SOME _ => r
fun ** reader input =
    case reader input of
        NONE => NONE
      | SOME (result, rest) =>
        case ** reader rest of
            NONE => SOME ([result], rest)
          | SOME (results, rest2) => SOME (result :: results, rest2)
fun listReader nil input = SOME (nil, input)
  | listReader (reader :: readers) input =
    case reader input of
        NONE => NONE
      | SOME (result, rest) =>
        case listReader readers rest of
            NONE => NONE
          | SOME (results, rest2) => SOME (result :: results, rest2)
fun skipOneChar char reader input =
    case reader input of
        NONE => input
      | SOME(c, rest) => if c = char then rest else input
fun skipChar char reader input =
    case reader input of
        NONE => input
      | SOME(c, rest) =>
        if c = char then skipChar char reader rest else input
fun showResult NONE = NONE
  | showResult (SOME (result, substr)) = SOME (result, Substring.string substr)

(* example *)
val _ =
    let
      val getc = Substring.getc
      val intReader = Int.scan StringCvt.DEC getc
      (* automaticly skip white space *)
      val readers = [intReader, intReader, intReader,
                     intReader, intReader, intReader]
    in
      listReader readers
    end
