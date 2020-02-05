infix &&>
fun op &&> (reader, f) input =
    case reader input of
        NONE => NONE
      | SOME (result, rest) => SOME (result, f rest)
fun listReader nil input = SOME (nil, input)
  | listReader (reader :: readers) input =
    case reader input of
        NONE => NONE
      | SOME (result, rest) =>
        case listReader readers rest of
            NONE => NONE
          | SOME (results, rest2) => SOME (result :: results, rest2)
fun parse () =
    let
      val getc = TextIO.StreamIO.input1
      val readInt = Int.scan StringCvt.DEC getc
      val skipWS = StringCvt.skipWS getc
      val intSpace = readInt &&> skipWS
      val readers =
          [intSpace, intSpace, intSpace, intSpace, intSpace, readInt]
      val instream = TextIO.getInstream TextIO.stdIn
      val results = listReader readers instream
    in
      case results of
          NONE => (TextIO.inputAll TextIO.stdIn; (* flush *)
                   raise Fail "bug: invalid input format")
        | SOME (inputs, rest) =>
          (TextIO.setInstream (TextIO.stdIn, rest); inputs)
    end
fun getThird _ _ third nil = third
  | getThird first second third (h::t) =
    if first < h then getThird h first second t
    else if second < h then getThird first h second t
    else if third < h then getThird first second h t
    else getThird first second third t
fun main () =
    let
      val inputs = parse ()
      val third = getThird 0 0 0 inputs
    in
      print (Int.toString third)
    end
val () = main ()
