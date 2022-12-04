package de.uni_saarland.cs.se
package traits

import utils.{ListStorage, MapStorage, Storage, StorageType}

import scala.collection.mutable


/**
 * Database interface for all database implementations and traits.
 */
trait Database {
  /**
   * The database's storage type, i.e., MAP or LIST.
   */
  val storageType: StorageType

  /**
   * Gives subclasses and traits access to the database's storage.
   *
   * @return the database's storage
   */
  protected def storage(): Storage
}


// TODO: implement task 1c