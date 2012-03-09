/*
 *  ____    ____    _____    ____    ___     ____ 
 * |  _ \  |  _ \  | ____|  / ___|  / _/    / ___|        Precog (R)
 * | |_) | | |_) | |  _|   | |     | |  /| | |  _         Advanced Analytics Engine for NoSQL Data
 * |  __/  |  _ <  | |___  | |___  |/ _| | | |_| |        Copyright (C) 2010 - 2013 SlamData, Inc.
 * |_|     |_| \_\ |_____|  \____|   /__/   \____|        All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of the 
 * GNU Affero General Public License as published by the Free Software Foundation, either version 
 * 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See 
 * the GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License along with this 
 * program. If not, see <http://www.gnu.org/licenses/>.
 *
 */
package com.precog.performance

import org.specs2.mutable.Specification

import com.precog.analytics.Path
import com.precog.common._
import com.precog.common.util._
import com.precog.yggdrasil._
import com.precog.yggdrasil.shard._
import com.precog.shard.yggdrasil._

import akka.actor.ActorSystem
import akka.dispatch.ExecutionContext
import akka.dispatch.Await
import akka.util.Duration

import java.io.File

import org.streum.configrity.Configuration

import scalaz._
import Scalaz._

trait YggdrasilPerformanceSpec extends Specification with PerformanceSpec {
  sequential 

  val timeout = Duration(30, "seconds")

  val config = Configuration.parse("""
        precog {
          kafka {
            enabled = true 
            topic {
              events = central_event_store
            }
            consumer {
              zk {
                connect = devqclus03.reportgrid.com:2181 
                connectiontimeout {
                  ms = 1000000
                }
              }
              groupid = shard_consumer
            }
          }
        }
        kafka {
          batch {
            host = devqclus03.reportgrid.com 
            port = 9092
            topic = central_event_store
          }
        }
        zookeeper {
          hosts = devqclus03.reportgrid.com:2181
          basepath = [ "com", "precog", "ingest", "v1" ]
          prefix = test
        } 
      """)  
  val tmpFile = File.createTempFile("insert_test", "_db")
  lazy val shard = new TestShard(config, tmpFile)
  lazy val executor = new TestQueryExecutor(config, shard)
  
  step {    
    tmpFile.delete
    tmpFile.mkdirs
    Await.result(shard.actorsStart, timeout)
  }

  "yggdrasil" should {

    def insert(shard: TestShard, path: Path, batchSize: Int, batches: Int) {

      val batch = new Array[EventMessage](batchSize)

      var id = 0
      var b = 0
      while(b < batches) {
        var i = 0
        while(i < batchSize) {
          val jval = AdSamples.adCampaignSample.sample.get
          val event = Event(path, "token", jval, Map.empty)
          batch(i) = EventMessage(EventId(0, id), event)
          i += 1
          id += 1
        }
        val result = shard.storeBatch(batch, timeout)
        Await.result(result, timeout)
        b += 1
      }
    }

    "insert 10K elements in 7s".performBatch(10000, 7000) { i =>
      val batchSize = 1000
 
      insert(shard, Path("/test/large/"), batchSize, i / batchSize)   
    }
    
    "read 10K elements in 4s".performBatch(10000, 4000) { i =>
      val result = executor.execute("token", "count(load(//test/large))") 
      result match {
        case Success(jval) => 
        case Failure(e) => new RuntimeException("Query result failure")
      }
    }
    
    "read 100 elements 100 times in 5s".performBatch(100, 5000) { i =>
      insert(shard, Path("/test/small1"), 100, 1)
      
      var cnt = 0
      while(cnt < i) {
        val result = executor.execute("token", "count(load(//test/small1))") 
        result match {
          case Success(jval) =>
          case Failure(e) => new RuntimeException("Query result failure")
        }
        cnt += 1
      }
    }
    
    "multi-thread read 100 elements 100 times in 3s".performBatch(10, 2500) { i =>
      insert(shard, Path("/test/small2"), 100, 1)
      val threadCount = 10 
      
      val threads = (0.until(threadCount)) map { _ =>
        new Thread {
          override def run() {
            var cnt = 0
            while(cnt < i) {
              val result = executor.execute("token", "count(load(//test/small2))") 
              result match {
                case Success(jval) =>
                case Failure(e) => new RuntimeException("Query result failure")
              }
              cnt += 1
            }
          }
        } 
      }
      threads.foreach{ _.start }
      threads.foreach{ _.join }
      
    }
  }

  step {
    Await.result(shard.actorsStop, timeout) 
    def delDir(dir : File) {
      dir.listFiles.foreach {
        case d if d.isDirectory => delDir(d)
        case f => f.delete()
      }
      dir.delete()
    }
    delDir(tmpFile)
  }
}

class TestQueryExecutor(config: Configuration, testShard: TestShard) extends YggdrasilQueryExecutor {

  lazy val actorSystem = ActorSystem("test_query_executor")
  implicit lazy val asyncContext = ExecutionContext.defaultExecutionContext(actorSystem)
  lazy val yggConfig = new YggdrasilQueryExecutorConfig {
      val config = TestQueryExecutor.this.config
      val sortWorkDir = scratchDir
      val chunkSerialization = SimpleProjectionSerialization
      val memoizationBufferSize = sortBufferSize
      val memoizationWorkDir = scratchDir
    }  
  type Storage = TestShard
  object ops extends Ops
  object query extends QueryAPI

  val storage = testShard
}

class TestShard(config: Configuration, dataDir: File) extends ActorYggShard with StandaloneActorEcosystem {
  type YggConfig = ProductionActorConfig
  lazy val yggConfig = new ProductionActorConfig {
    lazy val config = TestShard.this.config
  }
  lazy val yggState: YggState = YggState.restore(dataDir).unsafePerformIO.toOption.get 
}
