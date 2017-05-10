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

import slamdata.Predef._
import quasar.common.{PhaseResult, PhaseResultW}
import quasar.connector.CompileM
import quasar.contrib.pathy._
import quasar.effect.Failure
import quasar.fp._
import quasar.fp.ski._
import quasar.fp.numeric._
import quasar.frontend.{SemanticErrors, SemanticErrsT}
import quasar.frontend.logicalplan.{LogicalPlan => LP, Free => _, _}
import quasar.fs.FileSystemError
import quasar.fs.FileSystemError._
import quasar.fs.PathError._
import quasar.fs.mount.Mounting
import quasar.sql._
import quasar.std.StdLib.set._

import matryoshka._
import matryoshka.data.Fix
import matryoshka.implicits._
import scalaz._, Scalaz._

package object quasar {
  private def phase[A: RenderTree](label: String, r: SemanticErrors \/ A):
      CompileM[A] =
    EitherT(r.point[PhaseResultW]) flatMap { a =>
      (a.set(Vector(PhaseResult.tree(label, a)))).liftM[SemanticErrsT]
    }

  /** Compiles a query into raw LogicalPlan, which has not yet been optimized or
    * typechecked.
    */
  // TODO: Move this into the SQL package, provide a type class for it in core.
  def precompile[T: Equal: RenderTree]
    (query: Block[Fix[Sql]], vars: Variables, basePath: ADir)
    (implicit TR: Recursive.Aux[T, LP], TC: Corecursive.Aux[T, LP])
      : CompileM[T] = {
    import SemanticAnalysis._
    for {
      ast      <- phase("SQL AST", query.right)
      substAst <- phase("Variables Substituted", ast.mapExpressionM(Variables.substVars(_, vars)))
      absAst   <- phase("Absolutized", substAst.map(_.mkPathsAbsolute(basePath)).right)
      normed   <- phase("Normalized Projections", absAst.map(normalizeProjections[Fix[Sql]]).right)
      sortProj <- phase("Sort Keys Projected", normed.map(projectSortKeys[Fix[Sql]]).right)
      annBlock <- phase("Annotated Tree", (sortProj.traverse(annotate[Fix[Sql]])))
      logical  <- phase("Logical Plan", Compiler.compile[T](annBlock.expr, annBlock.defs) leftMap (_.wrapNel))
    } yield logical
  }

  private val optimizer = new Optimizer[Fix[LP]]
  private val lpr = optimizer.lpr

  /** Optimizes and typechecks a `LogicalPlan` returning the improved plan.
    */
  def preparePlan(lp: Fix[LP]): CompileM[Fix[LP]] =
    for {
      optimized   <- phase("Optimized", optimizer.optimize(lp).right)
      typechecked <- phase("Typechecked", lpr.ensureCorrectTypes(optimized).disjunction)
      rewritten   <- phase("Rewritten Joins", optimizer.rewriteJoins(typechecked).right)
    } yield rewritten

  /** Identify plans which reduce to a (set of) constant value(s). */
  def refineConstantPlan(lp: Fix[LP]): List[Data] \/ Fix[LP] =
    lp.project match {
      case Constant(Data.Set(records)) => records.left
      case Constant(value)             => List(value).left
      case _                           => lp.right
    }

  // It would be nice if this were in the sql package but that is not possible right now because
  // Mounting is defined in core
  def resolveImports[S[_]](blob: Blob[Fix[Sql]], basePath: ADir)(implicit
    mount: Mounting.Ops[S],
    fsFail: Failure.Ops[FileSystemError, S]
  ): Free[S, Block[Fix[Sql]]] = {
    def resolvePath(path: DPath, base: ADir): ADir = refineTypeAbs(path).fold(ι, base </> _)
    def retrieveDeclarations(module: ADir, allReadyImported: Set[ADir]): Free[S, List[FunctionDecl[Fix[Sql]]]] = {
      if (allReadyImported.contains(module)) List.empty[FunctionDecl[Fix[Sql]]].point[Free[S, ?]]
      else {
        val configOrFailure = mount.lookupModuleConfig(module).toRight(pathErr(pathNotFound(module)))
        for {
          config       <- fsFail.unattemptT(configOrFailure)
          childrenDecl <- config.imports.traverse(i => retrieveDeclarations(resolvePath(i.path, module), allReadyImported + module))
        } yield childrenDecl.join ++ config.declarations
      }
    }
    blob.imports.traverse(i => retrieveDeclarations(resolvePath(i.path, basePath), Set.empty)).map { allDecs =>
      Block(blob.expr, allDecs.join ++ blob.defs)
    }
  }

  /** Returns the `LogicalPlan` for the given SQL^2 query, or a list of
    * results, if the query was foldable to a constant.
    */
  def queryPlan(
    block: Block[Fix[Sql]], vars: Variables, basePath: ADir, off: Natural, lim: Option[Positive]):
      CompileM[List[Data] \/ Fix[LP]] =
    precompile[Fix[LP]](block, vars, basePath)
      .flatMap(lp => preparePlan(addOffsetLimit(lp, off, lim)))
      .map(refineConstantPlan)

  def addOffsetLimit[T]
    (lp: T, off: Natural, lim: Option[Positive])
    (implicit T: Corecursive.Aux[T, LP])
      : T = {
    val skipped = Drop(lp, constant[T](Data.Int(off.value)).embed).embed
    lim.fold(
      skipped)(
      l => Take(skipped, constant[T](Data.Int(l.value)).embed).embed)
  }
}
