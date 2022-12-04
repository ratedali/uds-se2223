package de.uni_saarland.cs.se
package decorator

import utils.ConfigurationError
import utils.{ListStorage, MapStorage, Storage, StorageType}

import scala.collection.mutable


/**
 * Interface for the database components and decorators.
 */
trait Database {
  def read(key: String): Option[String]
  def write(key: String, value: String): Unit
  def commit(): Int
  def rollback(): Int
  val storageType: StorageType
  private[decorator] def storage(): Storage
}


// TODO: implement task 1b