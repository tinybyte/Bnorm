
/**
 *
 * a package object defines package-wide utility methods and implicit conversions and type aliases
 *
 */
package object anorm {

  implicit def sqlToSimple(sql: SqlQuery): SimpleSql[Row] = sql.asSimple
  implicit def sqlToBatch(sql: SqlQuery): BatchSql = sql.asBatch

  implicit def implicitID[ID](id: Id[ID] with NotNull): ID = id.id

  implicit def toParameterValue[A](a: A)(implicit p: ToStatement[A]): ParameterValue[A] =
    ParameterValue(a, p)

  /**
   * Use the SQL method to start an SQL query
   *
   * {{{
   * import anorm._
   *
   * SQL("Select 1")
   * }}}
   * @param stmt
   * @return
   */
  def SQL(stmt: String) = Sql.sql(stmt)

}