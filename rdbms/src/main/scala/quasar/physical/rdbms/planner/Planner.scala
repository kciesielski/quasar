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

package quasar.physical.rdbms.planner

import matryoshka.{AlgebraM, BirecursiveT, ShowT}
import quasar.Planner._
import quasar.contrib.pathy.{ADir, AFile}
import quasar.qscript.{DeadEnd, EquiJoin, ProjectBucket, QScriptCore, Read, ShiftedRead, ThetaJoin}
import slamdata.Predef._

import scalaz.Scalaz._
import scalaz._

trait Planner[F[_], S[_]] extends Serializable {
  import Planner._

  def plan(expr: SqlExprBuilder): AlgebraM[RdbmsState[S, ?], F, Repr]
}

object Planner {
  type Repr = quasar.physical.rdbms.model.Repr
  type RdbmsState[S[_], A] = EitherT[Free[S, ?], PlannerError, A]

  def apply[F[_], S[_]](implicit P: Planner[F, S]): Planner[F, S] = P

  implicit def deadEnd[S[_]]: Planner[Const[DeadEnd, ?], S] = unreachable("deadEnd")
  implicit def read[A, S[_]]: Planner[Const[Read[A], ?], S] = unreachable("read")
  implicit def shiftedReadPath[S[_]]: Planner[Const[ShiftedRead[ADir], ?], S] = unreachable("shifted read of a dir")
  implicit def projectBucket[T[_[_]], S[_]]: Planner[ProjectBucket[T, ?], S] = unreachable("projectBucket")
  implicit def thetaJoin[T[_[_]], S[_]]: Planner[ThetaJoin[T, ?], S] = unreachable("thetajoin")
  implicit def shiftedread[S[_]]: Planner[Const[ShiftedRead[AFile], ?], S] = unreachable("TODO")
  implicit def qscriptCore[T[_[_]]: BirecursiveT: ShowT, S[_]]: Planner[QScriptCore[T, ?], S] = unreachable("TODO")
  implicit def equiJoin[T[_[_]]: BirecursiveT: ShowT, S[_]]: Planner[EquiJoin[T, ?], S] = unreachable("TODO")

  implicit def coproduct[F[_], G[_], S[_]](
      implicit F: Planner[F, S],
      G: Planner[G, S]): Planner[Coproduct[F, G, ?], S] =
    new Planner[Coproduct[F, G, ?], S] {
      def plan(expr: SqlExprBuilder): AlgebraM[RdbmsState[S, ?], Coproduct[F, G, ?], Repr] =
        _.run.fold(F.plan(expr), G.plan(expr))
    }

  private def unreachable[F[_], S[_]](what: String): Planner[F, S] =
    new Planner[F, S] {
      override def plan(expr: SqlExprBuilder): AlgebraM[RdbmsState[S, ?], F, Repr] =
        _ =>
          EitherT(InternalError.fromMsg(s"unreachable $what").left.pure[Free[S, ?]])
    }

}
