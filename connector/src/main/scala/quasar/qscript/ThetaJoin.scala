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

package quasar.qscript

import slamdata.Predef._
import quasar.{RenderTree, NonTerminal, RenderTreeT}, RenderTree.ops._
import quasar.common.JoinType
import quasar.contrib.matryoshka._
import quasar.fp._

import matryoshka._
import matryoshka.data._
import monocle.macros.Lenses
import scalaz._, Scalaz._

/** Applies a function across two datasets, in the cases where the JoinFunc
  * evaluates to true. The branches represent the divergent operations applied
  * to some common src. Each branch references the src exactly once. (Since no
  * constructor has more than one recursive component, it’s guaranteed that
  * neither side references the src _more_ than once.)
  *
  * This case represents a full θJoin, but we could have an algebra that
  * rewrites it as
  *     Filter(_, EquiJoin(...))
  * to simplify behavior for the backend.
  */
@Lenses final case class ThetaJoin[T[_[_]], A](
  src: A,
  lBranch: FreeQS[T],
  rBranch: FreeQS[T],
  on: JoinFunc[T],
  f: JoinType,
  combine: JoinFunc[T])

object ThetaJoin {
  implicit def equal[T[_[_]]: EqualT]: Delay[Equal, ThetaJoin[T, ?]] =
    new Delay[Equal, ThetaJoin[T, ?]] {
      @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
      def apply[A](eq: Equal[A]) =
        Equal.equal {
          case (ThetaJoin(a1, l1, r1, o1, f1, c1), ThetaJoin(a2, l2, r2, o2, f2, c2)) =>
            eq.equal(a1, a2) && l1 ≟ l2 && r1 ≟ r2 && o1 ≟ o2 && f1 ≟ f2 && c1 ≟ c2
        }
    }

  implicit def traverse[T[_[_]]]: Traverse[ThetaJoin[T, ?]] =
    new Traverse[ThetaJoin[T, ?]] {
      def traverseImpl[G[_]: Applicative, A, B](
        fa: ThetaJoin[T, A])(
        f: A => G[B]) =
        f(fa.src) ∘ (ThetaJoin(_, fa.lBranch, fa.rBranch, fa.on, fa.f, fa.combine))
    }

  implicit def show[T[_[_]]: ShowT]: Delay[Show, ThetaJoin[T, ?]] =
    new Delay[Show, ThetaJoin[T, ?]] {
      @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
      def apply[A](showA: Show[A]): Show[ThetaJoin[T, A]] = Show.show {
        case ThetaJoin(src, lBr, rBr, on, f, combine) =>
          Cord("ThetaJoin(") ++
          showA.show(src) ++ Cord(",") ++
          lBr.show ++ Cord(",") ++
          rBr.show ++ Cord(",") ++
          on.show ++ Cord(",") ++
          f.show ++ Cord(",") ++
          combine.show ++ Cord(")")
      }
    }

  implicit def renderTree[T[_[_]]: RenderTreeT: ShowT]: Delay[RenderTree, ThetaJoin[T, ?]] =
    new Delay[RenderTree, ThetaJoin[T, ?]] {
      val nt = List("ThetaJoin")
      @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
      def apply[A](r: RenderTree[A]): RenderTree[ThetaJoin[T, A]] = RenderTree.make {
          case ThetaJoin(src, lBr, rBr, on, f, combine) =>
            NonTerminal(nt, None, List(
              r.render(src),
              lBr.render,
              rBr.render,
              on.render,
              f.render,
              combine.render))
        }
      }

  implicit def mergeable[T[_[_]]: BirecursiveT: EqualT: ShowT]
      : Mergeable.Aux[T, ThetaJoin[T, ?]] =
    new Mergeable[ThetaJoin[T, ?]] {
      type IT[F[_]] = T[F]

      val merge = new Merge[IT]
      val rewrite = new Rewrite[IT]

      def mergeSrcs(
        left: FreeMap[IT],
        right: FreeMap[IT],
        p1: ThetaJoin[IT, ExternallyManaged],
        p2: ThetaJoin[IT, ExternallyManaged]) =
        (p1, p2) match {
          case (ThetaJoin(s1, l1, r1, o1, f1, c1), ThetaJoin(_, l2, r2, o2, f2, c2)) if f1 ≟ f2 => {
            val left1 = rebaseBranch(l1, left)
            val right1 = rebaseBranch(r1, left)
            val left2 = rebaseBranch(l2, right)
            val right2 = rebaseBranch(r2, right)

            def updateJoin(func: JoinFunc[IT], left: FreeMap[IT], right: FreeMap[IT]): JoinFunc[IT] =
              func.flatMap {
                case LeftSide => left.as(LeftSide)
                case RightSide => right.as(RightSide)
              }

            merge.tryMergeBranches(rewrite)(left1, right1, left2, right2).toOption.map {
              case (resL, resR) => {
                val onL: JoinFunc[IT] = updateJoin(o1, resL.lval, resR.lval)
                val onR: JoinFunc[IT] = updateJoin(o2, resL.rval, resR.rval)
                // The implication here is that
                // `(l join r on X) as lj inner join (l join r on Y) as rj on lj = rj`
                // is equivalent to
                // `l join r on X and Y`
                val on: JoinFunc[IT] = (onL ≟ onR).fold(onL, Free.roll(MFC(MapFuncsCore.And(onL, onR))))

                val cL: JoinFunc[IT] = updateJoin(c1, resL.lval, resR.lval)
                val cR: JoinFunc[IT] = updateJoin(c2, resL.rval, resR.rval)
                val (cond, left, right) = concat(cL, cR)

                SrcMerge(ThetaJoin(s1, resL.src, resR.src, on, f1, cond), left, right)
              }
            }
          }

          case (_, _) => None
    }
  }
}
