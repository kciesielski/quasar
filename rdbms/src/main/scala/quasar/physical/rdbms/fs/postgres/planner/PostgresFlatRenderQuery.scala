/*
 * Copyright 2014–2017 SlamData Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package quasar.physical.rdbms.fs.postgres.planner

import slamdata.Predef._
import quasar.Planner.PlannerError
import quasar.physical.rdbms.planner.sql.{RenderQuery, SqlExpr}
import quasar.physical.rdbms.planner.sql.SqlExpr.Select._
import matryoshka._
import matryoshka.implicits._

import scalaz._
import Scalaz._

object PostgresFlatRenderQuery extends RenderQuery {
  import SqlExpr._

  def compact[T[_[_]]: BirecursiveT](a: T[SqlExpr]): PlannerError \/ String = {
    val q = a.cataM(alg)

    a.project match {
      case s: Select[T[SqlExpr]] => q ∘ (s => s"select $s")
      case _                  => q ∘ ("select " ⊹ _)
    }
  }

  val alg: AlgebraM[PlannerError \/ ?, SqlExpr, String] = {
    case SqlExpr.Id(v) =>
      s"'$v'".right
    case SqlExpr.Table(v) =>
      v.right
    case Select(fields, table, filterOpt) =>
      val fieldsStr = fields match {
        case AllCols() =>
          "*"
        case WithIds(AllCols()) =>
          s"row_number() over(), *"
        case WithIds(SomeCols(names)) =>
          s"row_number() over(),${names.mkString(",")}"
        case RowIds() =>
          s"row_number() over()"
        case SomeCols(names) =>
          names.mkString(",")
      }
      val filter = ~(filterOpt ∘ (f => s"where ${f.v}"))
      s"$fieldsStr from ${table.expr} $filter".right
  }
}
