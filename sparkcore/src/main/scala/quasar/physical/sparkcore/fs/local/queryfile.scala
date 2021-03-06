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

package quasar.physical.sparkcore.fs.local

import slamdata.Predef._
import quasar.{Data, DataCodec}
import quasar.fs.FileSystemError
import quasar.fs.PathError._
import quasar.fs.FileSystemError._
import quasar.contrib.pathy._
import quasar.fp.ski._
import quasar.effect.Capture
import quasar.physical.sparkcore.fs.SparkConnectorDetails, SparkConnectorDetails._

import java.io.{File, PrintWriter, FileOutputStream}
import java.nio.file._

import org.apache.spark._
import org.apache.spark.rdd._
import pathy.Path._
import scalaz._, Scalaz._

object queryfile {
  
  def rddFrom[F[_]](f: AFile) (implicit
    reader: MonadReader[F, SparkContext]): F[RDD[Data]] =
    reader.asks { sc =>
      sc.textFile(posixCodec.unsafePrintPath(f))
        .map(raw => DataCodec.parse(raw)(DataCodec.Precise).fold(error => Data.NA, ι))
      }

  def store[F[_]:Capture](rdd: RDD[Data], out: AFile): F[Unit] = Capture[F].capture {
    val ioFile = new File(posixCodec.printPath(out))
    val pw = new PrintWriter(new FileOutputStream(ioFile, false))
    rdd.flatMap(DataCodec.render(_)(DataCodec.Precise).toList).collect().foreach(v => pw.write(s"$v\n"))
    pw.close()
  }

  def fileExists[F[_]:Capture](f: AFile): F[Boolean] = Capture[F].capture {
      Files.exists(Paths.get(posixCodec.unsafePrintPath(f)))
  }

  def listContents[F[_]:Capture](d: ADir): EitherT[F, FileSystemError, Set[PathSegment]] =
    EitherT(Capture[F].capture {
    val directory = new File(posixCodec.unsafePrintPath(d))
    if(directory.exists()) {
      \/.fromTryCatchNonFatal{
        directory.listFiles.toSet[File].map {
          case file if file.isFile() => FileName(file.getName()).right[DirName]
          case directory => DirName(directory.getName()).left[FileName]
        }
      }
        .leftMap {
        case e =>
          pathErr(invalidPath(d, e.getMessage()))
      }
    } else pathErr(pathNotFound(d)).left[Set[PathSegment]]
  })

  def detailsInterpreter[F[_]:Capture:MonadReader[?[_], SparkContext]]: SparkConnectorDetails ~> F =
    new (SparkConnectorDetails ~> F) {
      def apply[A](from: SparkConnectorDetails[A]) = from match {
        case FileExists(f)       => fileExists[F](f)
        case ReadChunkSize       => 5000.point[F]
        case StoreData(rdd, out) => store(rdd, out)
        case ListContents(d)     => listContents(d).run
        case RDDFrom(f)          => rddFrom[F](f)
      }
    }
}
