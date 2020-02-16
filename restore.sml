infix &&
fun op && (reader1, reader2) input =
    case reader1 input of
        NONE => NONE
      | SOME (result1, rest) =>
        (case reader2 rest of
             NONE => NONE
           | SOME (result2, rest) => SOME ((result1, result2), rest))

structure SNS
          : sig
            type user
            type querie
            type follows
            type state
            val initState : int -> state
            val follow : user -> user -> state -> unit
            (* val unfollow : user -> user -> state -> unit *)
            val unfollowSelf : state -> unit
            val getFollows : user -> state -> follows
            val getFollowers : user -> state -> follows
            val updateState : querie -> state -> unit
            val runQueries : (querie, 'a) StringCvt.reader ->
                             'a -> state -> unit
            val stateToString : state -> string
            val queryReader : (int, 'a) StringCvt.reader ->
                              (querie, 'a) StringCvt.reader
          end
=
struct
  type user = int
  datatype querie = Follow of user * user
                  | ReturnFollow of user
                  | FollowFollow of user
  type follows = bool vector
  type state = bool array array

  fun initState n =
      Array.tabulate (n, fn _ => Array.tabulate (n, fn _ => false))
  fun follow userA userB state =
      Array.update (Array.sub (state, userA), userB, true)
  fun unfollowSelf state =
      Array.appi
        (fn (user, _) => Array.update (Array.sub (state, user), user, false))
        state
  fun getFollows user state = Array.vector (Array.sub (state, user))
  fun getFollowers user state =
      let
        val followersList =
            Array.foldr (fn (follows, acc) => (Array.sub (follows, user)) :: acc)
                        nil
                        state
      in
        Vector.fromList followersList
      end
  fun updateState (Follow (userA, userB)) state = follow userA userB state
    | updateState (ReturnFollow user) state =
      let
        val followers = getFollowers user state
      in
        Vector.appi
          (fn (follower, true) => follow user follower state | (_, false) => ())
          followers
      end
    | updateState (FollowFollow user) state =
      let
        val follows = getFollows user state
        val () = Vector.appi
                  (fn (f, true) =>
                      Vector.appi
                        (fn (ff, true) => follow user ff state
                        | (_, false) => ())
                        (getFollows f state)
                  | (_, false) => ())
                  follows
      in
        unfollowSelf state
      end
  fun runQueries reader instream state =
      case reader instream of
          NONE => ()
        | SOME (query, rest)  =>
          (updateState query state; runQueries reader rest state)
  fun boolToFollow true = #"Y" | boolToFollow false = #"N"
  fun stateToString state =
      let
        val charList =
            Array.foldr
              (fn (arr, chars) => Array.foldr
                                    (fn (b, cs) => boolToFollow b :: cs)
                                    (#"\n" :: chars)
                                    arr)
              nil
              state
      in
        CharVector.fromList charList
      end
  fun queryReader intReader input = (* convert from 1 origin to 0 origin *)
      case intReader input of
          SOME (1, rest) =>
          (case (intReader && intReader) rest of
               SOME ((userA, userB), rest2) =>
               SOME (Follow (userA - 1, userB - 1), rest2)
             | NONE => raise Fail "bug: invalid input starting 1")
        | SOME (2, rest) =>
          (case intReader rest of
               SOME (user, rest2) => SOME (ReturnFollow (user - 1), rest2)
             | NONE => raise Fail "bug: invalid input starting 2")
        | SOME (3, rest) =>
          (case intReader rest of
               SOME (user, rest2) => SOME (FollowFollow (user - 1), rest2)
             | NONE => raise Fail "bug: invalid input starting 3")
        | SOME _ => NONE
        | NONE => NONE
end

fun main () =
    let
      val io = TextIO.stdIn
      val instream = TextIO.getInstream io
      val getc = TextIO.StreamIO.input1
      val readInt = Int.scan StringCvt.DEC getc
      val headerReader = readInt && readInt
      val ((accountNum, queryNum), instream) =
          valOf (headerReader instream)
          handle Option.Option => raise Fail "bug: invalid format in header"
      val state = SNS.initState accountNum
      val () = SNS.runQueries (SNS.queryReader readInt) instream state
      (* val () = print (SNS.stateToString state) *)
      val () = print (SNS.stateToString state)
      val () = TextIO.setInstream (io, instream)
    in
      ()
    end
val () = main ()
