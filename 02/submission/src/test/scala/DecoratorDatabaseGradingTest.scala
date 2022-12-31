package de.uni_saarland.cs.se

import decorator.*
import utils.ConfigurationError
import utils.StorageType

import org.scalatest.flatspec.AnyFlatSpec

import java.io.ByteArrayOutputStream

class DecoratorDatabaseGradingTest extends AnyFlatSpec {

  "A read-only map-database" must "only allow read operations." in {
    val db = Read(MapStorageDatabase())
    assert(db.read("foo").isEmpty)
    assertThrows[ConfigurationError] {
      db.write("foo", "bar")
    }
    assertThrows[ConfigurationError] {
      db.commit()
    }
    assertThrows[ConfigurationError] {
      db.rollback()
    }
    assert(db.storageType == StorageType.MAP)
  }

  "A read-only list-database" must "report the correct storage type." in {

    val db = Read(ListStorageDatabase())
    assert(db.read("foo").isEmpty)
    assertThrows[ConfigurationError] {
      db.write("foo", "bar")
    }
    assertThrows[ConfigurationError] {
      db.commit()
    }
    assertThrows[ConfigurationError] {
      db.rollback()
    }
    assert(db.storageType == StorageType.LIST)
  }

  "A read-only map-database with logging" should "log read operations" in {
    val db = Logging(Read(MapStorageDatabase()))
    val stream = new ByteArrayOutputStream()
    Console.withOut(stream) {
      assert(db.read("foo").isEmpty)
      assert(stream.toString().startsWith("Reading value for key 'foo'."))
      stream.reset()
    }

    assertThrows[ConfigurationError] {
      db.write("foo", "bar")
    }
    assertThrows[ConfigurationError] {
      db.commit()
    }
    assertThrows[ConfigurationError] {
      db.rollback()
    }
    assert(db.storageType == StorageType.MAP)
  }

  "A read-only list-database with logging" should "log read operations" in {
    val db = Logging(Read(ListStorageDatabase()))
    val stream = new ByteArrayOutputStream()
    Console.withOut(stream) {
      assert(db.read("foo").isEmpty)
      assert(stream.toString().startsWith("Reading value for key 'foo'."))
      stream.reset()

      assertThrows[ConfigurationError] {
        db.write("foo", "bar")
      }
      assert(stream.toString().isEmpty)
      stream.reset()

      assert(db.read("foo").isEmpty)
      assert(stream.toString().startsWith("Reading value for key 'foo'."))
      stream.reset()

      assertThrows[ConfigurationError] {
        db.commit()
      }
      assert(stream.toString().isEmpty)
      stream.reset()
      assertThrows[ConfigurationError] {
        db.rollback()
      }
      assert(stream.toString().isEmpty)
      stream.reset()

      assert(db.read("foo").isEmpty)
      assert(stream.toString().startsWith("Reading value for key 'foo'."))
      stream.reset()
    }

    assert(db.storageType == StorageType.LIST)
  }

  "A read-write map-database" must "allow read and write operations." in {
    val db = Write(Read(MapStorageDatabase()))
    assert(db.read("foo").isEmpty)
    db.write("foo", "bar")
    assert(db.read("foo").contains("bar"))
    assertThrows[ConfigurationError] {
      db.commit()
    }
    assertThrows[ConfigurationError] {
      db.rollback()
    }
  }

  "A read-write list-database" must "allow read and write operations." in {
    val db = Write(Read(ListStorageDatabase()))
    assert(db.read("foo").isEmpty)
    db.write("foo", "bar")
    assert(db.read("foo").contains("bar"))
    db.write("foo", "baz")
    assert(db.read("foo").contains("baz"))
    assert(db.read("asdf").isEmpty)
    assertThrows[ConfigurationError] {
      db.commit()
    }
    assertThrows[ConfigurationError] {
      db.rollback()
    }
    assert(db.storageType == StorageType.LIST)
  }

  "A read-write map-database with logging" should "log read and write operations" in {
    val db = Logging(Write(Read(MapStorageDatabase())))
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

    assertThrows[ConfigurationError] {
      db.commit()
    }
    assertThrows[ConfigurationError] {
      db.rollback()
    }
    assert(db.storageType == StorageType.MAP)
  }

  "A read-write list-database with logging" should "log read and write operations" in {
    val db = Logging(Write(Read(ListStorageDatabase())))
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

    assertThrows[ConfigurationError] {
      db.commit()
    }
    assertThrows[ConfigurationError] {
      db.rollback()
    }
    assert(db.storageType == StorageType.LIST)
  }

  "A map-database with transactions" should "require committing before reading." in {
    val db = Transaction(Write(Read(MapStorageDatabase())))
    assert(db.read("foo").isEmpty)
    db.write("foo", "bar")
    assert(db.read("foo").isEmpty)
    db.commit()
    assert(db.read("foo").contains("bar"))
  }

  "A list-database with transactions" should "not store roll-backed values." in {
    val db = Transaction(Write(Read(ListStorageDatabase())))
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
    val db = Logging(Transaction(Write(Read(MapStorageDatabase()))))
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
    val db = Logging(Transaction(Write(Read(ListStorageDatabase()))))
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
    val db = Write(MapStorageDatabase())
    db.write("foo", "bar")
    db.write("baz", "1")
    assertThrows[ConfigurationError] {
      db.read("foo")
    }
    assertThrows[ConfigurationError] {
      db.commit()
    }
    assertThrows[ConfigurationError] {
      db.rollback()
    }
    assert(db.storageType == StorageType.MAP)
  }

  "A write-only list-database" should "only allow write operations." in {
    val db = Write(ListStorageDatabase())
    db.write("foo", "bar")
    assertThrows[ConfigurationError] {
      db.read("foo")
    }
    assertThrows[ConfigurationError] {
      db.commit()
    }
    assertThrows[ConfigurationError] {
      db.rollback()
    }
    assert(db.storageType == StorageType.LIST)
  }

  "A write-only map-database with logging" should "not log read operations" in {
    val db = Logging(Write(MapStorageDatabase()))
    val stream = new ByteArrayOutputStream()
    Console.withOut(stream) {
      assertThrows[ConfigurationError] {
        db.read("foo")
      }
      assert(stream.toString().isEmpty)
      stream.reset()

      db.write("foo", "bar")
      assert(stream.toString().startsWith("Writing value 'bar' at key 'foo'."))
      stream.reset()

      assertThrows[ConfigurationError] {
        db.read("foo")
      }
      assert(stream.toString().isEmpty)
      stream.reset()
    }

    assertThrows[ConfigurationError] {
      db.commit()
    }
    assertThrows[ConfigurationError] {
      db.rollback()
    }
    assert(db.storageType == StorageType.MAP)
  }

  "A write-only list-database with logging" should "not log read operations" in {
    val db = Logging(Write(ListStorageDatabase()))
    val stream = new ByteArrayOutputStream()
    Console.withOut(stream) {
      assertThrows[ConfigurationError] {
        db.read("foo")
      }
      assert(stream.toString().isEmpty)
      stream.reset()

      db.write("foo", "bar")
      assert(stream.toString().startsWith("Writing value 'bar' at key 'foo'."))
      stream.reset()

      assertThrows[ConfigurationError] {
        db.read("foo")
      }
      assert(stream.toString().isEmpty)
      stream.reset()

      assertThrows[ConfigurationError] {
        db.commit()
      }
      assert(stream.toString().isEmpty)
      stream.reset()
      assertThrows[ConfigurationError] {
        db.rollback()
      }
      assert(stream.toString().isEmpty)
      stream.reset()
    }
    assert(db.storageType == StorageType.LIST)
  }

  "A map-database with transactions" can "be write-only." in {
    val db = Transaction(Write(MapStorageDatabase()))
    db.write("foo", "bar")
    db.commit()
    assertThrows[ConfigurationError] {
      db.read("foo")
    }
    assert(db.storageType == StorageType.MAP)
  }

  "A list-database with transactions" can "be write-only." in {
    val db = Transaction(Write(ListStorageDatabase()))
    db.write("foo", "bar")
    db.commit()
    assertThrows[ConfigurationError] {
      db.read("foo")
    }
    assert(db.storageType == StorageType.LIST)
  }

  "A write-only map-database with transactions and logging" should "log all operations" in {
    val db = Logging(Transaction(Write(MapStorageDatabase())))
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

      assertThrows[ConfigurationError](db.read("foo"))
      assert(stream.toString().isEmpty)
      stream.reset()

      assertThrows[ConfigurationError](db.read("bar"))
      assert(stream.toString().isEmpty)
      stream.reset()
    }
  }

  "A write-only list-database with transactions and logging" should "log all operations" in {
    val db = Logging(Transaction(Write(ListStorageDatabase())))
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

      assertThrows[ConfigurationError](db.read("foo"))
      assert(stream.toString().isEmpty)
      stream.reset()

      assertThrows[ConfigurationError](db.read("bar"))
      assert(stream.toString().isEmpty)
      stream.reset()
    }
  }
}
