package de.uni_saarland.cs.se
package decorator

import scala.collection.mutable

import utils.ConfigurationError
import utils.{ ListStorage, MapStorage, Storage, StorageType }

/** Interface for the database components and decorators.
  */
trait Database {
  def read(key: String): Option[String]
  def write(key: String, value: String): Unit
  def commit(): Int
  def rollback(): Int
  val storageType: StorageType
  private[decorator] def storage(): Storage
}

class MapStorageDatabase extends Database:
  private val mapStorage =
    MapStorage()
  def read(key: String): Nothing =
    throw ConfigurationError()
  def write(key: String, value: String): Nothing =
    throw ConfigurationError()
  def commit(): Nothing =
    throw ConfigurationError()
  def rollback(): Nothing =
    throw ConfigurationError()
  val storageType: StorageType =
    StorageType.MAP
  private[decorator] def storage(): Storage =
    mapStorage

class ListStorageDatabase extends Database:
  private val listStorage =
    ListStorage()
  def read(key: String): Nothing =
    throw ConfigurationError()
  def write(key: String, value: String): Nothing =
    throw ConfigurationError()
  def commit(): Nothing =
    throw ConfigurationError()
  def rollback(): Nothing =
    throw ConfigurationError()
  val storageType: StorageType =
    StorageType.LIST
  private[decorator] def storage(): Storage =
    listStorage

abstract class DatabaseDecorator(val database: Database) extends Database:
  def read(key: String): Option[String] =
    database.read(key)
  def write(key: String, value: String): Unit =
    database.write(key, value)
  def commit(): Int =
    database.commit()
  def rollback(): Int =
    database.rollback()
  val storageType: StorageType =
    database.storageType
  private[decorator] def storage(): Storage =
    database.storage()

class Read(database: Database) extends DatabaseDecorator(database) with Database:
  override def read(key: String): Option[String] =
    database.storage().get(key)

class Write(database: Database) extends DatabaseDecorator(database) with Database:
  override def write(key: String, value: String): Unit =
    database.storage().put(key, value)

class Transaction(database: Database) extends DatabaseDecorator(database) with Database:
  private var tmpStorage: Storage =
    ListStorage()
  override def write(key: String, value: String): Unit =
    tmpStorage.put(key, value)
  override def commit(): Int =
    val size = tmpStorage.size()
    tmpStorage.foreach((k, v) => storage().put(k, v))
    tmpStorage = ListStorage()
    size
  override def rollback(): Int =
    val size = tmpStorage.size()
    tmpStorage = ListStorage()
    size

class Logging(database: Database) extends DatabaseDecorator(database) with Database:
  override def read(key: String): Option[String] =
    val value = super.read(key)
    println(s"Reading value for key '$key'.")
    value
  override def write(key: String, value: String): Unit =
    super.write(key, value)
    println(s"Writing value '$value' at key '$key'.")
  override def commit(): Int =
    val size = super.commit()
    println(s"Committing $size entries.")
    size
  override def rollback(): Int =
    val size = super.rollback()
    println(s"Rolling back $size entries.")
    size
