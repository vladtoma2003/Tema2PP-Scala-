val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

l(3)

val r = Map(1 -> "one", 2 -> "two", 3 -> "three")

val index = r.get(6)
println(index)

//val m1 = List(List("1", "2", "3"), List("4", "5", "6"), List("7", "8", "9"))
//val m2 = List(List("1", "11", "3"), List("20", "199", "6"), List("90", "12", "402"))

//val m1 = List(List("Language","Functional","Note", "Haskel"),
//  List("true","very", "functional","Scala"),
//  List("true","multi-paradigm","Prolog","false"),
//  List("P A I N","Husk","true","v. func."))

val m1 = List(List("Language", "Functional", "Note"),
  List("Haskell", "true", "very functional"),
  List("Scala", "true", "multi-paradigm"),
  List("Prolog", "false", "P A I N"),
  List("Husk", "true", "v. func."))

val m2 = List(List("Language", "Obj", "Note", "a"),
  List("Haskell", "false", "no objects 0/10", "b"),
  List("Scala", "true", "multi-paradigm", "c"),
  List("COOL", "true", "not cool", "d"),
  List("Go", "true", "go parallel", "e"))

val h1 = m1.head
val h2 = m2.head

val common = h1.intersect(h2)
val finalCol = h1 ++ h2.diff(common)


val restOfMatrix1 = m1.tail
val restOfMatrix2 = m2.tail

val NewMatrix = List(finalCol)

val key = "Language"

val indexKey1 = h1.indexOf(key)
val indexKey2 = h2.indexOf(key)
if (indexKey1 == -1 || indexKey2 == -1) List() // nu exista cheia in una din matrici
else {
  val cv = common.filter(x => x != key)
  println("cv:" + cv)
  val keyColumn = key +: (restOfMatrix1 ::: restOfMatrix2).flatMap(x => x.drop(indexKey1).head
    :: Nil).distinct // lista de coloane comune
  println(keyColumn)
  // iau fiecare cheie de pe colaona cheie
  val col = keyColumn.tail.flatMap(x => {
    // indexul cheii curente in matricea 1 si 2
    val index1 = restOfMatrix1.transpose.drop(indexKey1).head.indexOf(x)
    val index2 = restOfMatrix2.transpose.drop(indexKey2).head.indexOf(x)
    cv.map(y => { // fiecare coloana comuna
      if (index2 == -1) { // exista cheia doar in matricea 1
        // iau valoarea de pe coloana comuna curenta din matricea 1
        val v = restOfMatrix1.drop(index1).head.drop(h1.indexOf(y)).head
        v
      } else if (index1 == -1) { // pt 2
        val v = restOfMatrix2.drop(index2).head.drop(h2.indexOf(y)).head
        v
      } else {
        // exista in ambele matrici cheia deci iau valorile
        val v1 = restOfMatrix1.drop(index1).head.drop(h1.indexOf(y)).head
        val v2 = restOfMatrix2.drop(index2).head.drop(h2.indexOf(y)).head
        if (v1 == v2) v1 else v1 + ";" + v2 // daca sunt la fel ramane una altfel amandoua
      }
    })
  }).grouped(cv.size).toList.transpose
      println("common:" + col.map(x => cv.drop(col.indexOf(x)).head +: x)) // adaug si numele
  // coloanei la inceput

  val rest = finalCol.diff(common).diff(key)
  println(rest)

  val m1Dif = h1.filter(x => x != key && !common.contains(x))
  val m2Dif = h2.filter(x => x != key && !common.contains(x))

  val coaie = if(m1Dif.nonEmpty) {
    keyColumn.tail.flatMap(x => {
      val i = restOfMatrix1.transpose.drop(indexKey1).head.indexOf(x) // cheia curenta
      m1Dif.map(y => { // pun valoarea necesara din matricea 1
        if (i == -1) ""
        else {
          val v = restOfMatrix1.drop(i).head.drop(h1.indexOf(y)).head
          v
        }
      })
    }).grouped(m1Dif.size).toList.transpose
  } else List()

  val coaie2 = if(m2Dif.nonEmpty) {
    keyColumn.tail.flatMap(x => {
      val i = restOfMatrix2.transpose.drop(indexKey2).head.indexOf(x)
      m2Dif.map(y => {
        if (i == -1) ""
        else {
          val v = restOfMatrix2.drop(i).head.drop(h2.indexOf(y)).head
          v
        }
      })
    }).grouped(m2Dif.size).toList.transpose
  } else List()


  val mat = (List(keyColumn)
    :: coaie.map(x => m1Dif.drop(coaie.indexOf(x)).head +: x)
    :: col.map(x => cv.drop(col.indexOf(x)).head +: x)
    :: coaie2.map(x => m2Dif.drop(coaie2.indexOf(x)).head +: x)
    :: List())
    .filter(x => x.nonEmpty)
    .flatten
    .transpose

  println("mat: " + mat)

  //
  //    coaie2.map(x => m2Dif.drop(coaie2.indexOf(x)).head +: x)
  //
  // trebuie sa mai iau ce nu e comun
  //    val finalMatrix = keyColumn :: col.grouped(keyColumn.size).toList
  //    println(finalMatrix)

  //    List(distinct)

}
//}

val c = List(List(List("a"), List("c")), List("cv"), List())
