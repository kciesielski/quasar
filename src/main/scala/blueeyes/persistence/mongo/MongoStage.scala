package blueeyes.persistence.mongo

import scala.collection.mutable.Map

import blueeyes.json.JsonAST._

import blueeyes.persistence.cache.{ExpirationPolicy, CacheSettings, Stage}

case class MongoStageSettings(expirationPolicy: ExpirationPolicy, maximumCapacity: Int)

/** A stage for updates to Mongo.
 */
class MongoStage(database: MongoDatabase, collection: MongoCollection, mongoStageSettings: MongoStageSettings) extends
  Stage[MongoFilter, MongoUpdate] {

  def flush(filter: MongoFilter, update: MongoUpdate) = {
    database[JNothing.type] {
      upsert(collection).set(update).where(filter)
    }
  }

  def expirationPolicy = mongoStageSettings.expirationPolicy

  def maximumCapacity = mongoStageSettings.maximumCapacity
}

object MongoStage {
  def apply(database: MongoDatabase, collection: MongoCollection, mongoStageSettings: MongoStageSettings) = {
    new MongoStage(database, collection, mongoStageSettings)
  }
}