package de.uni_saarland.cs.se
package runtime

import utils.ConfigurationError
import utils.ListStorage
import utils.MapStorage
import utils.Storage
import utils.StorageType

import scala.collection.mutable

/** Configuration class for databases.
  *
  * @param read
  *   whether the database should support read operations
  * @param write
  *   whether the database should support write operations
  * @param transaction
  *   whether the database should support transactions
  * @param logging
  *   whether the database should support logging
  * @param storageType
  *   the type of storage the database should use
  */
class DatabaseConfig(
    val read: Boolean,
    val write: Boolean,
    val transaction: Boolean,
    val logging: Boolean,
    val storageType: StorageType
) {}

/** A runtime-configurable version of our database SPL.
  *
  * @param config
  *   the configuration for the database
  */
class Database(val config: DatabaseConfig) {
  private val storage: Storage = config.storageType match {
    case StorageType.LIST => new ListStorage()
    case StorageType.MAP  => new MapStorage()
  }
  private var tmpStorage: Storage = ListStorage()

  /** Read a value from the database.
    *
    * @param key
    *   the key under which the value is stored
    * @return
    *   the stored value if present or an empty option
    */
  def read(key: String): Option[String] =
    if (config.read) {
      if (config.logging) {
        println(s"Reading value for key '$key'.")
      }
      storage.get(key)
    } else {
      throw new ConfigurationError()
    }

  /** Store a value with a given key.
    *
    * @param key
    *   the key to use for storing
    * @param value
    *   the value to store
    */
  def write(key: String, value: String): Unit =
    if (config.write) {
      if (config.logging) {
        println(s"Writing value '$value' at key '$key'.")
      }
      if (config.transaction) {
        tmpStorage.put(key, value)
      } else {
        storage.put(key, value)
      }
    } else {
      throw new ConfigurationError()
    }

  /** Commit all pending writes.
    *
    * @return
    *   the number of committed write operations.
    */
  def commit(): Int = {
    val size = tmpStorage.size()
    if (config.transaction) {
      if (config.logging) {
        println(s"Committing $size entries.")
      }
      tmpStorage.foreach((k, v) => storage.put(k, v))
      tmpStorage = ListStorage()
      size
    } else {
      throw new ConfigurationError()
    }
  }

  /** Roll back all pending writes.
    *
    * @return
    *   the number of roll-backed write operations.
    */
  def rollback(): Int = {
    val size = tmpStorage.size()
    if (config.transaction) {
      if (config.logging) {
        println(s"Rolling back ${tmpStorage.size()} entries.")
      }
      tmpStorage = ListStorage()
      size
    } else {
      throw new ConfigurationError()
    }
  }

  /** The type of storage used by the database.
    */
  val storageType = storage.storageType
}
