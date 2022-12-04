package de.uni_saarland.cs.se
package traits

import scala.collection.mutable

import utils.{ ListStorage, MapStorage, Storage, StorageType }

/** Database interface for all database implementations and traits.
  */
trait Database {

  /** The database's storage type, i.e., MAP or LIST.
    */
  val storageType: StorageType

  /** Gives subclasses and traits access to the database's storage.
    *
    * @return
    *   the database's storage
    */
  protected def storage(): Storage
}

trait Read extends Database:
  /** Read a value from the database.
    *
    * @param key
    *   the key under which the value is stored
    * @return
    *   the stored value if present or an empty option
    */
  def read(key: String): Option[String] = storage().get(key)

trait ReadWithLogging extends Read:
  override def read(key: String): Option[String] =
    println(s"Reading value for key '$key'.")
    super.read(key)

trait Write extends Database:
  /** Store a value with a given key.
    *
    * @param key
    *   the key to use for storing
    * @param value
    *   the value to store
    */
  def write(key: String, value: String): Unit = storage().put(key, value)

trait WriteWithLogging extends Write:
  override def write(key: String, value: String): Unit =
    println(s"Writing value '$value' at key '$key'.")
    super.write(key, value)

trait Transaction extends Write:
  protected var tmpStorage: Storage = ListStorage()

  /** Store a value with a given key.
    *
    * @param key
    *   the key to use for storing
    * @param value
    *   the value to store
    */
  override def write(key: String, value: String): Unit =
    tmpStorage.put(key, value)

  /** Commit all pending writes.
    *
    * @return
    *   the number of committed write operations.
    */
  def commit(): Int =
    val size = tmpStorage.size()
    tmpStorage.foreach((k, v) => storage().put(k, v))
    tmpStorage = ListStorage()
    size

  /** Roll back all pending writes.
    *
    * @return
    *   the number of roll-backed write operations.
    */
  def rollback(): Int =
    val size = tmpStorage.size()
    tmpStorage = ListStorage()
    size

trait TransactionWithLogging extends Transaction:
  override def commit(): Int =
    val size = tmpStorage.size()
    println(s"Committing $size entries.")
    super.commit()

  override def rollback(): Int =
    val size = tmpStorage.size()
    println(s"Rolling back $size entries.")
    super.rollback()

class MapStoreDatabase extends Database:
  private val mapStorage           = MapStorage()
  val storageType: StorageType     = StorageType.MAP
  protected def storage(): Storage = mapStorage

class ListStoreDatabase extends Database:
  private val listStorage          = ListStorage()
  val storageType: StorageType     = StorageType.LIST
  protected def storage(): Storage = listStorage
