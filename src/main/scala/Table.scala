import util.Util.Row
import util.Util.Line
import TestTables.tableImperative
import TestTables.tableFunctional
import TestTables.tableObjectOriented

trait FilterCond {
  // these are useful for the intuitive infix notation
  // e.g. the following expression is a filter condition:
  // Field("PL", x=>true) && Field("PL", x=> false)
  def &&(other: FilterCond): FilterCond = ???
  def ||(other: FilterCond): FilterCond = ???
  // fails if the column name is not present in the row
  def eval(r: Row): Option[Boolean]
}
case class Field(colName: String, predicate: String => Boolean) extends FilterCond {
  // 2.2.
  override def eval(r: Row): Option[Boolean] = {
    val index = r.get(colName) // reutrneaza None daca e nu exista
    if(index.isEmpty) None
    else Option(predicate(index.get))
  }
}

case class And(f1: FilterCond, f2: FilterCond) extends FilterCond {
  // 2.2.
  override def eval(r: Row): Option[Boolean] = ???
}

case class Or(f1: FilterCond, f2: FilterCond) extends FilterCond {
  // 2.2.
  override def eval(r: Row): Option[Boolean] = ???
}

trait Query {
  def eval: Option[Table]
}

/*
  Atom query which evaluates to the input table
  Always succeeds
 */
case class Value(t: Table) extends Query {
  override def eval: Option[Table] = ???
}
/*
  Selects certain columns from the result of a target query
  Fails with None if some rows are not present in the resulting table
 */
case class Select(columns: Line, target: Query) extends Query {
  override def eval: Option[Table] = ???
}

/*
  Filters rows from the result of the target query
  Success depends only on the success of the target
 */
case class Filter(condition: FilterCond, target: Query) extends Query {
  override def eval: Option[Table] = ???
}

/*
  Creates a new column with default values
  Success depends only on the success of the target
 */
case class NewCol(name: String, defaultVal: String, target: Query) extends Query {
  override def eval: Option[Table] = ???
}

/*
  Combines two tables based on a common key
  Success depends on whether the key exists in both tables or not AND on the success of the target
 */
case class Merge(key: String, t1: Query, t2: Query) extends Query {
  override def eval: Option[Table] = ???
}


class Table (columnNames: Line, tabular: List[List[String]]) {
  def getColumnNames : Line = columnNames
  def getTabular : List[List[String]] = tabular

  // 1.1
  override def toString: String = this.getColumnNames.mkString(",") + "\n" +
                 this.getTabular.map(_.mkString(",")).mkString("\n")

  // 2.1
  def select(columns: Line): Option[Table] = {
    // iau indexii colanelor cautate
    val indexOfCollumns = columns.map(col => getColumnNames.indexOf(col))
    if(indexOfCollumns.head == -1) None // verific daca am gasit ceva
    else {
      // iau doar coloanele cautate
      val newTabular = getTabular.map(row => indexOfCollumns.map(col => row(col)))
      // valoarea returnata
      Option(new Table(columns, newTabular))
    }
  }

  // 2.2
  def filter(cond: FilterCond): Option[Table] = ???

  // 2.3.
  def newCol(name: String, defaultVal: String): Table = ???

  // 2.4.
  def merge(key: String, other: Table): Option[Table] = ???
}

object Table {
  // 1.2
  def apply(s: String): Table = {
    val lines = s.split("\n").toList // impart liniile dupa \n si le pun intr-o lista
    val columnNames = lines.head.split(",").toList
    val tabular = lines.tail.map(_.split(",").toList)
    new Table(columnNames, tabular)
  }
}
