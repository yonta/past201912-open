structure Output =
struct
  fun print s = TextIO.print s
  fun println s = TextIO.print (s ^ "\n")
end
