package de.uni_saarland.cs.se
package runtime

import utils.ConfigurationError
import utils.{ListStorage, MapStorage, Storage, StorageType}

import scala.collection.mutable


/**
 * Configuration class for databases.
 * 
 * @param read whether the database should support read operations
 * @param write whether the database should support write operations
 * @param transaction whether the database should support transactions
 * @param logging whether the database should support logging
 * @param storageType the type of storage the database should use
 */
class DatabaseConfig(
  val read: Boolean,
  val write: Boolean,
  val transaction: Boolean,
  val logging: Boolean,
  val storageType: StorageType
) {}


/**
 * A runtime-configurable version of our database SPL.
 * 
 * @param config the configuration for the database
 */
class Database(val config: DatabaseConfig) {
  // TODO: implement task 1a
}