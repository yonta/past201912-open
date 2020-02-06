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

fun count _ nil = ()
  | count array (input :: inputs) =
    let
      (* one origin to zero origin *)
      val index = input - 1
      val n = Array.sub (array, index)
      val () = Array.update (array, index, n + 1)
    in
      count array inputs
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
      val array = Array.array (N, 0)
      val () = count array inputs
      val wrong = Array.findi (fn (_, x) => x = 2) array
      val original = Array.findi (fn (_, x) => x = 0) array
      val () = TextIO.setInstream (io, instream)
    in
      case (wrong, original) of
          (NONE, NONE) => print "Correct"
        | (SOME (index1, _), SOME (index2, _)) =>
          (* zero origin to one origin *)
          print (Int.toString (index1 + 1) ^ " " ^ Int.toString (index2 + 1))
        | _ => raise Fail "bug: invalid input"
    end
val () = main ()
