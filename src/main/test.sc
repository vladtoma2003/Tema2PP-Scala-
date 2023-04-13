val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

l(3)

val r = Map(1 -> "one", 2 -> "two", 3 -> "three")

val index = r.get(6)
println(index)

val l2 = List(1, 3, 11, 12, 5)

val commonElems = l.intersect(l2)
val uniqueList = l ++ (l2 diff commonElems)