import util.Util.Row
import util.Util.Line
import TestTables.tableImperative
import TestTables.tableFunctional
import TestTables.tableObjectOriented

trait FilterCond {
  // these are useful for the intuitive infix notation
  // e.g. the following expression is a filter condition:
  // Field("PL", x=>true) && Field("PL", x=> false)
  def &&(other: FilterCond): FilterCond = And(this, other)

  def ||(other: FilterCond): FilterCond = Or(this, other)

  // fails if the column name is not present in the row
  def eval(r: Row): Option[Boolean]
}

case class Field(colName: String, predicate: String => Boolean) extends FilterCond {
  // 2.2.
  override def eval(r: Row): Option[Boolean] = {
    val index = r.get(colName) // reutrneaza None daca e nu exista
    if (index.isEmpty) None
    else Option(predicate(index.get))
  }
}

case class And(f1: FilterCond, f2: FilterCond) extends FilterCond {
  // 2.2.
  override def eval(r: Row): Option[Boolean] = {
    // trebuie sa existe amandoua pentru a putea crea Option
    val x = f1.eval(r)
    val y = f2.eval(r)
    if (x.isEmpty || y.isEmpty) None
    else Option(x.get && y.get)
  }
}

case class Or(f1: FilterCond, f2: FilterCond) extends FilterCond {
  // 2.2.
  override def eval(r: Row): Option[Boolean] = {
    // trebuie sa existe amandoua pentru a putea crea Option
    val x = f1.eval(r)
    val y = f2.eval(r)
    if (x.isEmpty || y.isEmpty) None
    else Option(x.get || y.get)
  }
}

trait Query {
  def eval: Option[Table]
}

/*
  Atom query which evaluates to the input table
  Always succeeds
 */
case class Value(t: Table) extends Query {
  override def eval: Option[Table] = Option(t) // poate Some
}

/*
  Selects certain columns from the result of a target query
  Fails with None if some rows are not present in the resulting table
 */
case class Select(columns: Line, target: Query) extends Query {
  override def eval: Option[Table] = target.eval.get.select(columns)
}

/*
  Filters rows from the result of the target query
  Success depends only on the success of the target
 */
case class Filter(condition: FilterCond, target: Query) extends Query {
  override def eval: Option[Table] = target.eval.get.filter(condition)
}

/*
  Creates a new column with default values
  Success depends only on the success of the target
 */
case class NewCol(name: String, defaultVal: String, target: Query) extends Query {
  override def eval: Option[Table] = Option(target.eval.get.newCol(name, defaultVal))
}

/*
  Combines two tables based on a common key
  Success depends on whether the key exists in both tables or not AND on the success of the target
 */
case class Merge(key: String, t1: Query, t2: Query) extends Query {
  override def eval: Option[Table] = t1.eval.get.merge(key, t2.eval.get)
}


class Table(columnNames: Line, tabular: List[List[String]]) {
  def getColumnNames: Line = columnNames

  def getTabular: List[List[String]] = tabular

  // 1.1
  override def toString: String = this.getColumnNames.mkString(",") + "\n" +
    this.getTabular.map(_.mkString(",")).mkString("\n")

  // 2.1
  def select(columns: Line): Option[Table] = {
    // iau indexii colanelor cautate
    val indexOfCollumns = columns.map(col => getColumnNames.indexOf(col))
    if (indexOfCollumns.head == -1) None // verific daca am gasit ceva
    else {
      // iau doar coloanele cautate
      val newTabular = getTabular.map(row => indexOfCollumns.map(col => row(col)))
      // valoarea returnata
      Option(new Table(columns, newTabular))
    }
  }

  // 2.2
  def filter(cond: FilterCond): Option[Table] = {
    // iau randurile care indeplinesc conditia
    val newTabular = getTabular.filter(row =>
      if (cond.eval(getColumnNames.zip(row).toMap) != None) // verific daca exista ceva
        cond.eval(getColumnNames.zip(row).toMap).get // da eroare la None.get
      else false
    )
    if (newTabular.isEmpty) None
    else Option(new Table(getColumnNames, newTabular))
  }

  // 2.3.
  def newCol(name: String, defaultVal: String): Table = {
    val newColumnNames: Line = getColumnNames :+ name // adaug numele coloanei noi
    //adaug valorile default in tabela
    val newTabular: List[List[String]] = getTabular.map(row => row ::: List(defaultVal))
    Table((newColumnNames :: newTabular).map(_.mkString(",")).mkString("\n"))
    //    new Table(newColumnNames, newTabular)
  }

  // 2.4.
  def merge(key: String, other: Table): Option[Table] = {
    val h1 = this.getColumnNames
    val h2 = other.getColumnNames

    val common = h1.intersect(h2)
    val finalCol = h1 ++ h2.diff(common)

    val indexKey1 = h1.indexOf(key)
    val indexKey2 = h2.indexOf(key)
    if (indexKey1 == -1 || indexKey2 == -1) None // nu exista cheia in una din matrici
    else {
      val commonColumnsNoKey = common.filter(x => x != key)
      val keyColumn = key +: (this.getTabular ::: other.getTabular)
        .flatMap(x => x.drop(indexKey1).head :: Nil).distinct // lista de coloane comune
      // iau fiecare cheie de pe colaona cheie
      val col = keyColumn.tail.flatMap(x => {
        // indexul cheii curente in matricea 1 si 2
        val index1 = this.getTabular.transpose.drop(indexKey1).head.indexOf(x)
        val index2 = other.getTabular.transpose.drop(indexKey2).head.indexOf(x)
        commonColumnsNoKey.map(y => { // fiecare coloana comuna
          if (index2 == -1) { // exista cheia doar in matricea 1
            // iau valoarea de pe coloana comuna curenta din matricea 1
            val v = this.getTabular.drop(index1).head.drop(h1.indexOf(y)).head
            v
          } else if (index1 == -1) { // pt 2
            val v = other.getTabular.drop(index2).head.drop(h2.indexOf(y)).head
            v
          } else {
            // exista in ambele matrici cheia deci iau valorile
            val v1 = this.getTabular.drop(index1).head.drop(h1.indexOf(y)).head
            val v2 = other.getTabular.drop(index2).head.drop(h2.indexOf(y)).head
            if (v1 == v2) v1 else v1 + ";" + v2 // daca sunt la fel ramane una altfel amandoua
          }
        })
      }).grouped(commonColumnsNoKey.size).toList.transpose

      val m1Dif = h1.filter(x => x != key && !common.contains(x)) // coloanele ramase din matricea 1
      val m2Dif = h2.filter(x => x != key && !common.contains(x)) // coloanele ramase din matricea 2

      val restOfColumns1 = if (m1Dif.nonEmpty) {
        keyColumn.tail.flatMap(x => {
          val currentKey = this.getTabular.transpose.drop(indexKey1).head.indexOf(x)
          m1Dif.map(y => { // pun valoarea necesara din matricea 1
            if (currentKey == -1) ""
            else {
              val v = this.getTabular.drop(currentKey).head.drop(h1.indexOf(y)).head
              v
            }
          })
        }).grouped(m1Dif.size).toList.transpose
      } else List()

      val restOfColumns2 = if (m2Dif.nonEmpty) {
        keyColumn.tail.flatMap(x => {
          val currentKey = other.getTabular.transpose.drop(indexKey2).head.indexOf(x)
          m2Dif.map(y => {
            if (currentKey == -1) ""
            else {
              val v = other.getTabular.drop(currentKey).head.drop(h2.indexOf(y)).head
              v
            }
          })
        }).grouped(m2Dif.size).toList.transpose
      } else List()

      val newMatrix = (List(keyColumn)
        :: col.map(x => commonColumnsNoKey.drop(col.indexOf(x)).head +: x)
        :: restOfColumns1.map(x => m1Dif.drop(restOfColumns1.indexOf(x)).head +: x)
        :: restOfColumns2.map(x => m2Dif.drop(restOfColumns2.indexOf(x)).head +: x)
        :: List()).filter(x => x.nonEmpty).flatten

      // reordonez coloanele
      val newOrder = finalCol.map(x => {
        val curOrder = newMatrix.map(_.head)
        val curIndex = curOrder.indexOf(x)
        if (curIndex == -1) List()
        else {
          newMatrix(curIndex)
        }
      })

      Option(Table((newOrder.transpose).map(_.mkString(",")).mkString("\n")))
    }
  }
}

object Table {
  // 1.2
  def apply(s: String): Table = {
    val lines = s.split("\n").toList // impart liniile dupa \n si le pun intr-o lista
    val columnNames = lines.head.split(",").toList
    val tabular = lines.tail.map(_.split(",", -1).toList)
    new Table(columnNames, tabular)
  }
}