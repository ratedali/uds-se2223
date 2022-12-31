package de.uni_saarland.cs.se

import traits.*
import utils.ConfigurationError
import utils.StorageType

import org.scalatest.flatspec.AnyFlatSpec

import java.io.ByteArrayOutputStream

class TraitsDatabaseGradingTest extends AnyFlatSpec {

  class ROMapDB extends MapStoreDatabase with Read {}
  class ROListDB extends ListStoreDatabase with Read {}
  class ROLogMapDB extends MapStoreDatabase with ReadWithLogging {}
  class ROLogListDB extends ListStoreDatabase with ReadWithLogging {}
  class RWMapDB extends MapStoreDatabase with Write with Read {}
  class RWListDB extends ListStoreDatabase with Write with Read {}
  class RWLogMapDB extends MapStoreDatabase with WriteWithLogging with ReadWithLogging {}
  class RWLogListDB extends ListStoreDatabase with WriteWithLogging with ReadWithLogging {}
  class RWTxnMapDB extends MapStoreDatabase with Transaction with Write with Read {}
  class RWTxnListDB extends ListStoreDatabase with Transaction with Write with Read {}
  class RWTxnLogMapDB extends MapStoreDatabase with TransactionWithLogging with WriteWithLogging with ReadWithLogging {}
  class RWTxnLogListDB extends ListStoreDatabase with TransactionWithLogging with WriteWithLogging with ReadWithLogging {}

  class WMapDB extends MapStoreDatabase with Write {}
  class WListDB extends ListStoreDatabase with Write {}
  class WLogMapDB extends MapStoreDatabase with WriteWithLogging {}
  class WLogListDB extends ListStoreDatabase with WriteWithLogging {}
  class WTxnMapDB extends MapStoreDatabase with Transaction with Write {}
  class WTxnListDB extends ListStoreDatabase with Transaction with Write {}
  class WTxnLogMapDB extends MapStoreDatabase with TransactionWithLogging with WriteWithLogging {}
  class WTxnLogListDB extends ListStoreDatabase with TransactionWithLogging with WriteWithLogging {}

  "A read-only map-database" must "only allow read operations." in {
    val db = ROMapDB()
    assert(db.read("foo").isEmpty)
    assert(db.storageType == StorageType.MAP)
  }

  "A read-only list-database" must "report the correct storage type." in {
    val db = ROListDB()
    assert(db.read("foo").isEmpty)
    assert(db.storageType == StorageType.LIST)
  }

  "A read-only map-database with logging" should "log read operations" in {
    val db = ROLogMapDB()
    val stream = new ByteArrayOutputStream()
    Console.withOut(stream) {
      assert(db.read("foo").isEmpty)
      assert(stream.toString().startsWith("Reading value for key 'foo'."))
      stream.reset()
    }
    assert(db.storageType == StorageType.MAP)
  }

  "A read-only list-database with logging" should "log read operations" in {
    val db = ROLogListDB()
    val stream = new ByteArrayOutputStream()
    Console.withOut(stream) {
      assert(db.read("foo").isEmpty)
      assert(stream.toString().startsWith("Reading value for key 'foo'."))
      stream.reset()

      assert(db.read("foo").isEmpty)
      assert(stream.toString().startsWith("Reading value for key 'foo'."))
      stream.reset()

      assert(db.read("foo").isEmpty)
      assert(stream.toString().startsWith("Reading value for key 'foo'."))
      stream.reset()
    }

    assert(db.storageType == StorageType.LIST)
  }

  "A read-write map-database" must "allow read and write operations." in {
    val db = RWMapDB()
    assert(db.read("foo").isEmpty)
    db.write("foo", "bar")
    assert(db.read("foo").contains("bar"))
  }

  "A read-write list-database" must "allow read and write operations." in {
    val db = RWListDB()
    assert(db.read("foo").isEmpty)
    db.write("foo", "bar")
    assert(db.read("foo").contains("bar"))
    db.write("foo", "baz")
    assert(db.read("foo").contains("baz"))
    assert(db.read("asdf").isEmpty)
    assert(db.storageType == StorageType.LIST)
  }

  "A read-write map-database with logging" should "log read and write operations" in {
    val db = RWLogMapDB()
    val stream = new ByteArrayOutputStream()
    Console.withOut(stream) {
      assert(db.read("foo").isEmpty)
      assert(stream.toString().startsWith("Reading value for key 'foo'."))
      stream.reset()

      db.write("foo", "bar")
      assert(stream.toString().startsWith("Writing value 'bar' at key 'foo'."))
      stream.reset()

      assert(db.read("foo").contains("bar"))
      assert(stream.toString().startsWith("Reading value for key 'foo'."))
      stream.reset()
    }
    assert(db.storageType == StorageType.MAP)
  }

  "A read-write list-database with logging" should "log read and write operations" in {
    val db = RWLogListDB()
    val stream = new ByteArrayOutputStream()
    Console.withOut(stream) {
      assert(db.read("foo").isEmpty)
      assert(stream.toString().startsWith("Reading value for key 'foo'."))
      stream.reset()

      db.write("foo", "bar")
      assert(stream.toString().startsWith("Writing value 'bar' at key 'foo'."))
      stream.reset()

      assert(db.read("foo").contains("bar"))
      assert(stream.toString().startsWith("Reading value for key 'foo'."))
      stream.reset()
    }
    assert(db.storageType == StorageType.LIST)
  }

  "A map-database with transactions" should "require committing before reading." in {
    val db = RWTxnMapDB()
    assert(db.read("foo").isEmpty)
    db.write("foo", "bar")
    assert(db.read("foo").isEmpty)
    db.commit()
    assert(db.read("foo").contains("bar"))
  }

  "A list-database with transactions" should "not store roll-backed values." in {
    val db = RWTxnListDB()
    assert(db.read("foo").isEmpty)
    assert(db.read("bar").isEmpty)
    db.write("foo", "1")
    assert(db.read("foo").isEmpty)
    db.rollback()
    db.write("bar", "2")
    db.commit()
    assert(db.read("foo").isEmpty)
    assert(db.read("bar").contains("2"))
  }

  "A map-database with logging" should "log all operations" in {
    val db = RWTxnLogMapDB()
    val stream = new ByteArrayOutputStream()
    Console.withOut(stream) {
      db.write("foo", "1")
      assert(stream.toString().startsWith("Writing value '1' at key 'foo'."))
      stream.reset()

      db.rollback()
      assert(stream.toString().startsWith("Rolling back 1 entries."))
      stream.reset()

      db.write("bar", "2")
      assert(stream.toString().startsWith("Writing value '2' at key 'bar'."))
      stream.reset()

      db.commit()
      assert(stream.toString().startsWith("Committing 1 entries."))
      stream.reset()

      db.read("foo")
      assert(stream.toString().startsWith("Reading value for key 'foo'."))
      stream.reset()

      db.read("bar")
      assert(stream.toString().startsWith("Reading value for key 'bar'."))
      stream.reset()
    }
  }

  "A list-database with logging" should "log all operations" in {
    val db = RWTxnLogListDB()
    assert(db.storageType == StorageType.LIST)

    val stream = new ByteArrayOutputStream()
    Console.withOut(stream) {
      db.write("foo", "1")
      assert(stream.toString().startsWith("Writing value '1' at key 'foo'."))
      stream.reset()

      db.rollback()
      assert(stream.toString().startsWith("Rolling back 1 entries."))
      stream.reset()

      db.write("bar", "2")
      assert(stream.toString().startsWith("Writing value '2' at key 'bar'."))
      stream.reset()

      db.commit()
      assert(stream.toString().startsWith("Committing 1 entries."))
      stream.reset()

      assert(db.read("foo").isEmpty)
      assert(stream.toString().startsWith("Reading value for key 'foo'."))
      stream.reset()

      assert(db.read("bar").contains("2"))
      assert(stream.toString().startsWith("Reading value for key 'bar'."))
      stream.reset()
    }
  }

  "A write-only map-database" should "only allow write operations." in {
    val db = WMapDB()
    db.write("foo", "bar")
    db.write("baz", "1")
    assert(db.storageType == StorageType.MAP)
  }

  "A write-only list-database" should "only allow write operations." in {
    val db = WListDB()
    db.write("foo", "bar")
    assert(db.storageType == StorageType.LIST)
  }

  "A write-only map-database with logging" should "not log read operations" in {
    val db = WLogMapDB()
    val stream = new ByteArrayOutputStream()
    Console.withOut(stream) {
      db.write("foo", "bar")
      assert(stream.toString().startsWith("Writing value 'bar' at key 'foo'."))
      stream.reset()
    }
    assert(db.storageType == StorageType.MAP)
  }

  "A write-only list-database with logging" should "not log read operations" in {
    val db = WLogListDB()
    val stream = new ByteArrayOutputStream()
    Console.withOut(stream) {
      db.write("foo", "bar")
      assert(stream.toString().startsWith("Writing value 'bar' at key 'foo'."))
      stream.reset()
    }
    assert(db.storageType == StorageType.LIST)
  }

  "A map-database with transactions" can "be write-only." in {
    val db = WTxnMapDB()
    db.write("foo", "bar")
    db.commit()
    assert(db.storageType == StorageType.MAP)
  }

  "A list-database with transactions" can "be write-only." in {
    val db = WTxnListDB()
    db.write("foo", "bar")
    db.commit()
    assert(db.storageType == StorageType.LIST)
  }

  "A write-only map-database with transactions and logging" should "log all operations" in {
    val db = WTxnLogMapDB()
    assert(db.storageType == StorageType.MAP)

    val stream = new ByteArrayOutputStream()
    Console.withOut(stream) {
      db.write("foo", "1")
      assert(stream.toString().startsWith("Writing value '1' at key 'foo'."))
      stream.reset()

      db.rollback()
      assert(stream.toString().startsWith("Rolling back 1 entries."))
      stream.reset()

      db.write("bar", "2")
      assert(stream.toString().startsWith("Writing value '2' at key 'bar'."))
      stream.reset()

      db.commit()
      assert(stream.toString().startsWith("Committing 1 entries."))
      stream.reset()
    }
  }

  "A write-only list-database with transactions and logging" should "log all operations" in {
    val db = WTxnLogListDB()
    assert(db.storageType == StorageType.LIST)

    val stream = new ByteArrayOutputStream()
    Console.withOut(stream) {
      db.write("foo", "1")
      assert(stream.toString().startsWith("Writing value '1' at key 'foo'."))
      stream.reset()

      db.rollback()
      assert(stream.toString().startsWith("Rolling back 1 entries."))
      stream.reset()

      db.write("bar", "2")
      assert(stream.toString().startsWith("Writing value '2' at key 'bar'."))
      stream.reset()

      db.commit()
      assert(stream.toString().startsWith("Committing 1 entries."))
      stream.reset()
    }
  }
}
