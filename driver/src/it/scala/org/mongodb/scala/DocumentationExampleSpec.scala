/*
 * Copyright 2017 MongoDB, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.mongodb.scala

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.language.implicitConversions

import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonString}

//scalastyle:off magic.number
class DocumentationExampleSpec extends RequiresMongoDBISpec {

  // Implicit functions that execute the Observable and return the results
  val waitDuration = Duration(5, "seconds")
  implicit class ObservableExecutor[T](observable: Observable[T]) {
    def execute(): Seq[T] = Await.result(observable, waitDuration)
  }

  implicit class SingleObservableExecutor[T](observable: SingleObservable[T]) {
    def execute(): T = Await.result(observable, waitDuration)
  }

  "The Scala driver" should "be able to insert" in withCollection { collection =>

    // Start Example 1
    collection.insertOne(
      Document("""{ item: "canvas", qty: 100, tags: ["cotton"], size: { h: 28, w: 35.5, uom: "cm" } }""")
    ).execute()
    // End Example 1

    // Start Example 2
    val observable = collection.find(Document("{item: 'canvas'}"))
    // End Example 2

    observable.execute().size should equal(1)

    // Start Example 3
    collection.insertMany(Seq(
      Document("""{ item: "journal", qty: 25, tags: ["blank", "red"], size: { h: 14, w: 21, uom: "cm" } }"""),
      Document("""{ item: "mat", qty: 85, tags: ["gray"], size: { h: 27.9, w: 35.5, uom: "cm" } }"""),
      Document("""{ item: "mousepad", qty: 25, tags: ["gel", "blue"], size: { h: 19, w: 22.85, uom: "cm" } }""")
    )).execute()
    // End Example 3

    collection.count().execute() should equal(4)
  }

  it should "be able to query top level" in withCollection { collection =>

    // Start Example 6
    collection.insertMany(Seq(
      Document("""{ item: "journal", qty: 25, size: { h: 14, w: 21, uom: "cm" }, status: "A" }"""),
      Document("""{ item: "notebook", qty: 50, size: { h: 8.5, w: 11, uom: "in" }, status: "A" }"""),
      Document("""{ item: "paper", qty: 100, size: { h: 8.5, w: 11, uom: "in" }, status: "D" }"""),
      Document("""{ item: "planner", qty: 75, size: { h: 22.85, w: 30, uom: "cm" }, status: "D" }"""),
      Document("""{ item: "postcard", qty: 45, size: { h: 10, w: 15.25, uom: "cm" }, status: "A" }""")
    )).execute()
    // End Example 6

    collection.count().execute() should equal(5)

    // Start Example 7
    var findObservable = collection.find(Document())
    // End Example 7

    findObservable.execute().size should equal(5)

    // Start Example 8
    findObservable = collection.find()
    // End Example 8

    findObservable.execute().size should equal(5)

    // Start Example 9
    import org.mongodb.scala.model.Filters
    findObservable = collection.find(Filters.eq("status", "D"))
    // End Example 9

    findObservable.execute().size should equal(2)

    // Start Example 10
    findObservable = collection.find(Filters.in("status", "A", "D"))
    // End Example 10

    findObservable.execute().size should equal(5)

    // Start Example 11
    findObservable = collection.find(Filters.and(Filters.eq("status", "A"), Filters.lt("qty", 30)))
    // End Example 11

    findObservable.execute().size should equal(1)

    // Start Example 12
    findObservable = collection.find(Filters.or(Filters.eq("status", "A"), Filters.lt("qty", 30)))
    // End Example 12

    findObservable.execute().size should equal(3)

    // Start Example 13
    findObservable = collection.find(Filters.and(
      Filters.eq("status", "A"),
      Filters.or(Filters.lt("qty", 30), Filters.regex("item", "^p")))
    )
    // End Example 13

    findObservable.execute().size should equal(2)
  }

  it should "be able to query embedded documents" in withCollection { collection =>

    // Start Example 14
    collection.insertMany(Seq(
      Document("""{ item: "journal", qty: 25, size: { h: 14, w: 21, uom: "cm" }, status: "A" }"""),
      Document("""{ item: "notebook", qty: 50, size: { h: 8.5, w: 11, uom: "in" }, status: "A" }"""),
      Document("""{ item: "paper", qty: 100, size: { h: 8.5, w: 11, uom: "in" }, status: "D" }"""),
      Document("""{ item: "planner", qty: 75, size: { h: 22.85, w: 30, uom: "cm" }, status: "D" }"""),
      Document("""{ item: "postcard", qty: 45, size: { h: 10, w: 15.25, uom: "cm" }, status: "A" }""")
    )).execute()
    // End Example 14

    collection.count().execute() should equal(5)

    // Start Example 15
    import org.mongodb.scala.model.Filters
    var findObservable = collection.find(Filters.eq("size", Document("h" -> 14, "w" -> 21, "uom" -> "cm")))
    // End Example 15

    findObservable.execute().size should equal(1)

    // Start Example 16
    findObservable = collection.find(Filters.eq("size", Document("w" -> 21, "h" -> 14, "uom" -> "cm")))
    // End Example 16

    findObservable.execute().size should equal(0)

    // Start Example 17
    findObservable = collection.find(Filters.eq("size.uom", "in"))
    // End Example 17

    findObservable.execute().size should equal(2)

    // Start Example 18
    findObservable = collection.find(Filters.lt("size.h", 15))
    // End Example 18

    findObservable.execute().size should equal(4)

    // Start Example 19
    findObservable = collection.find(Filters.and(
      Filters.lt("size.h", 15),
      Filters.eq("size.uom", "in"),
      Filters.eq("status", "D")
    ))
    // End Example 19

    findObservable.execute().size should equal(1)
  }

  it should "be able to query array" in withCollection { collection =>

    //Start Example 20
    collection.insertMany(Seq(
      Document("""{ item: "journal", qty: 25, tags: ["blank", "red"], dim_cm: [ 14, 21 ] }"""),
      Document("""{ item: "notebook", qty: 50, tags: ["red", "blank"], dim_cm: [ 14, 21 ] }"""),
      Document("""{ item: "paper", qty: 100, tags: ["red", "blank", "plain"], dim_cm: [ 14, 21 ] }"""),
      Document("""{ item: "planner", qty: 75, tags: ["blank", "red"], dim_cm: [ 22.85, 30 ] }"""),
      Document("""{ item: "postcard", qty: 45, tags: ["blue"], dim_cm: [ 10, 15.25 ] }""")
    )).execute()
    //End Example 20

    collection.count().execute() should equal(5)

    //Start Example 21
    import org.mongodb.scala.model.Filters
    var findObservable = collection.find(Filters.eq("tags", Seq("red", "blank")))
    //End Example 21

    findObservable.execute().size should equal(1)

    //Start Example 22
    findObservable = collection.find(Filters.all("tags", "red", "blank"))
    //End Example 22

    findObservable.execute().size should equal(4)

    //Start Example 23
    findObservable = collection.find(Filters.eq("tags", "red"))
    //End Example 23

    findObservable.execute().size should equal(4)

    //Start Example 24
    findObservable = collection.find(Filters.gt("dim_cm", 25))
    //End Example 24

    findObservable.execute().size should equal(1)

    //Start Example 25
    findObservable = collection.find(Filters.and(Filters.gt("dim_cm", 15), Filters.lt("dim_cm", 20)))
    //End Example 25

    findObservable.execute().size should equal(4)

    //Start Example 26
    findObservable = collection.find(Filters.elemMatch("dim_cm", Document("$gt" -> 22, "$lt" -> 30)))

    //End Example 26

    findObservable.execute().size should equal(1)

    //Start Example 27
    findObservable = collection.find(Filters.gt("dim_cm.1", 25))
    //End Example 27

    findObservable.execute().size should equal(1)

    //Start Example 28
    findObservable = collection.find(Filters.size("tags", 3))
    //End Example 28

    findObservable.execute().size should equal(1)
  }

  it should "query array of documents" in withCollection { collection =>

    //Start Example 29
    collection.insertMany(Seq(
      Document("""{ item: "journal", instock: [ { warehouse: "A", qty: 5 }, { warehouse: "C", qty: 15 } ] }"""),
      Document("""{ item: "notebook", instock: [ { warehouse: "C", qty: 5 } ] }"""),
      Document("""{ item: "paper", instock: [ { warehouse: "A", qty: 60 }, { warehouse: "B", qty: 15 } ] }"""),
      Document("""{ item: "planner", instock: [ { warehouse: "A", qty: 40 }, { warehouse: "B", qty: 5 } ] }"""),
      Document("""{ item: "postcard", instock: [ { warehouse: "B", qty: 15 }, { warehouse: "C", qty: 35 } ] }""")
    )).execute()
    //End Example 29

    collection.count().execute() should equal(5)

    //Start Example 30
    import org.mongodb.scala.model.Filters
    var findObservable = collection.find(Filters.eq("instock", Document("warehouse" -> "A", "qty" -> 5)))
    //End Example 30

    findObservable.execute().size should equal(1)

    //Start Example 31
    findObservable = collection.find(Filters.eq("instock", Document("qty" -> 5, "warehouse" -> "A")))
    //End Example 31

    findObservable.execute().size should equal(0)

    //Start Example 32
    findObservable = collection.find(Filters.lte("instock.0.qty", 20))
    //End Example 32

    findObservable.execute().size should equal(3)

    //Start Example 33
    findObservable = collection.find(Filters.lte("instock.qty", 20))
    //End Example 33

    findObservable.execute().size should equal(5)

    //Start Example 34
    findObservable = collection.find(Filters.elemMatch("instock", Document("qty" -> 5, "warehouse" -> "A")))
    //End Example 34

    findObservable.execute().size should equal(1)

    //Start Example 35
    findObservable = collection.find(Filters.elemMatch("instock", Document("""{ qty: { $gt: 10, $lte: 20 } }""")))
    //End Example 35

    findObservable.execute().size should equal(3)

    //Start Example 36
    findObservable = collection.find(Filters.and(Filters.gt("instock.qty", 10), Filters.lte("instock.qty", 20)))
    //End Example 36

    findObservable.execute().size should equal(4)

    //Start Example 37
    findObservable = collection.find(Filters.and(Filters.eq("instock.qty", 5), Filters.eq("instock.warehouse", "A")))
    //End Example 37

    findObservable.execute().size should equal(2)
  }

  it should "query null and missing fields" in withCollection { collection =>

    //Start Example 38
    collection.insertMany(Seq(
      Document("""{"_id": 1, "item": null}"""),
      Document("""{"_id": 2}""")
    )).execute()
    //End Example 38

    collection.count().execute() should equal(2)

    //Start Example 39
    var findObservable = collection.find(Document("item" -> None))
    //End Example 39

    findObservable.execute().size should equal(2)

    //Start Example 40
    import org.bson.BsonType

    import org.mongodb.scala.model.Filters
    findObservable = collection.find(Filters.bsonType("item", BsonType.NULL))
    //End Example 40

    findObservable.execute().size should equal(1)

    //Start Example 41
    findObservable = collection.find(Filters.exists("item", exists = false))
    //End Example 41

    findObservable.execute().size should equal(1)
  }

  it should "be able to project fields" in withCollection { collection =>

    //Start Example 42
    collection.insertMany(Seq(
      Document("""{ item: "journal", status: "A", size: { h: 14, w: 21, uom: "cm" }, instock: [ { warehouse: "A", qty: 5 } ] }"""),
      Document("""{ item: "notebook", status: "A",  size: { h: 8.5, w: 11, uom: "in" }, instock: [ { warehouse: "C", qty: 5 } ] }"""),
      Document("""{ item: "paper", status: "D", size: { h: 8.5, w: 11, uom: "in" }, instock: [ { warehouse: "A", qty: 60 } ] }"""),
      Document("""{ item: "planner", status: "D", size: { h: 22.85, w: 30, uom: "cm" }, instock: [ { warehouse: "A", qty: 40 } ] }"""),
      Document(
        """{ item: "postcard", status: "A", size: { h: 10, w: 15.25, uom: "cm" },
                    instock: [ { warehouse: "B", qty: 15 }, { warehouse: "C", qty: 35 } ] }""")

    )).execute()
    //End Example 42

    collection.count().execute() should equal(5)

    //Start Example 43
    import org.mongodb.scala.model.Filters
    var findObservable = collection.find(Filters.eq("status", "A"))
    //End Example 43

    findObservable.execute().size should equal(3)

    //Start Example 44
    import org.mongodb.scala.model.Projections
    findObservable = collection.find(Filters.eq("status", "A")).projection(Projections.include("item", "status"))
    //End Example 44

    findObservable.execute().foreach(doc => doc.keys should contain only("_id", "item", "status"))

    //Start Example 45
    findObservable = collection.find(Filters.eq("status", "A"))
      .projection(Projections.fields(Projections.include("item", "status"), Projections.excludeId()))
    //End Example 45

    findObservable.execute().foreach(doc => doc.keys should contain only("item", "status"))

    //Start Example 46
    findObservable = collection.find(Filters.eq("status", "A")).projection(Projections.exclude("item", "status"))
    //End Example 46

    findObservable.execute().foreach(doc => doc.keys should contain only("_id", "size", "instock"))

    //Start Example 47
    findObservable = collection.find(Filters.eq("status", "A")).projection(Projections.include("item", "status", "size.uom"))
    //End Example 47

    findObservable.execute().foreach(doc => {
      doc.keys should contain only("_id", "item", "status", "size")
      doc.get[BsonDocument]("size").get.keys should contain only "uom"
    })

    //Start Example 48
    findObservable = collection.find(Filters.eq("status", "A")).projection(Projections.exclude("size.uom"))
    //End Example 48

    findObservable.execute().foreach(doc => {
      doc.keys should contain only("_id", "item", "instock", "status", "size")
      doc.get[BsonDocument]("size").get.keys should contain only("h", "w")
    })

    //Start Example 49
    findObservable = collection.find(Filters.eq("status", "A")).projection(Projections.include("item", "status", "instock.qty"))
    //End Example 49

    findObservable.execute().foreach(doc => {
      doc.keys should contain only("_id", "item", "instock", "status")
      doc.get[BsonArray]("instock").get.forEach(doc => doc.asInstanceOf[BsonDocument].keys should contain only "qty")
    })

    //Start Example 50
    findObservable = collection.find(Filters.eq("status", "A"))
      .projection(Projections.fields(Projections.include("item", "status"), Projections.slice("instock", -1)))
    //End Example 50

    findObservable.execute().foreach(doc => {
      doc.keys should contain only("_id", "item", "instock", "status")
      doc.get[BsonArray]("instock").get.size() should equal(1)
    })
  }

  it should "be able to update" in withCollection { collection =>

    //Start Example 51
    collection.insertMany(Seq(
      Document("""{ item: "canvas", qty: 100, size: { h: 28, w: 35.5, uom: "cm" }, status: "A" }"""),
      Document("""{ item: "journal", qty: 25, size: { h: 14, w: 21, uom: "cm" }, status: "A" }"""),
      Document("""{ item: "mat", qty: 85, size: { h: 27.9, w: 35.5, uom: "cm" }, status: "A" }"""),
      Document("""{ item: "mousepad", qty: 25, size: { h: 19, w: 22.85, uom: "cm" }, status: "P" }"""),
      Document("""{ item: "notebook", qty: 50, size: { h: 8.5, w: 11, uom: "in" }, status: "P" }"""),
      Document("""{ item: "paper", qty: 100, size: { h: 8.5, w: 11, uom: "in" }, status: "D" }"""),
      Document("""{ item: "planner", qty: 75, size: { h: 22.85, w: 30, uom: "cm" }, status: "D" }"""),
      Document("""{ item: "postcard", qty: 45, size: { h: 10, w: 15.25, uom: "cm" }, status: "A" }"""),
      Document("""{ item: "sketchbook", qty: 80, size: { h: 14, w: 21, uom: "cm" }, status: "A" }"""),
      Document("""{ item: "sketch pad", qty: 95, size: { h: 22.85, w: 30.5, uom: "cm" }, status: "A" }""")
    )).execute()
    //End Example 51

    collection.count().execute() should equal(10)

    //Start Example 52
    import org.mongodb.scala.model.Filters
    import org.mongodb.scala.model.Updates
    collection.updateOne(Filters.eq("item", "paper"),
      Updates.combine(Updates.set("size.uom", "cm"), Updates.set("status", "P"), Updates.currentDate("lastModified"))
    ).execute()
    //End Example 52

    collection.find(Filters.eq("item", "paper")).execute().foreach(doc => {
      doc.get[BsonDocument]("size").get.get("uom") should equal(BsonString("cm"))
      doc.get[BsonString]("status").get should equal(BsonString("P"))
      doc.containsKey("lastModified") shouldBe true
    })

    //Start Example 53
    collection.updateMany(Filters.lt("qty", 50),
      Updates.combine(Updates.set("size.uom", "in"), Updates.set("status", "P"), Updates.currentDate("lastModified"))
    ).execute()
    //End Example 53

    collection.find(Filters.lt("qty", 50)).execute().foreach(doc => {
      doc.get[BsonDocument]("size").get.get("uom") should equal(BsonString("in"))
      doc.get[BsonString]("status").get should equal(BsonString("P"))
      doc.containsKey("lastModified") shouldBe true
    })

    //Start Example 54
    collection.replaceOne(Filters.eq("item", "paper"),
      Document("""{ item: "paper", instock: [ { warehouse: "A", qty: 60 }, { warehouse: "B", qty: 40 } ] }""")
    ).execute()
    //End Example 54

    import org.mongodb.scala.model.Projections
    collection.find(Filters.eq("item", "paper")).projection(Projections.excludeId()).execute().foreach(doc =>
      doc should equal(Document("""{ item: "paper", instock: [ { warehouse: "A", qty: 60 }, { warehouse: "B", qty: 40 } ] }"""))
    )
  }
  
  it should "be able to delete" in withCollection { collection =>

    //Start Example 55
    collection.insertMany(Seq(
      Document("""{ item: "journal", qty: 25, size: { h: 14, w: 21, uom: "cm" }, status: "A" }"""),
      Document("""{ item: "notebook", qty: 50, size: { h: 8.5, w: 11, uom: "in" }, status: "A" }"""),
      Document("""{ item: "paper", qty: 100, size: { h: 8.5, w: 11, uom: "in" }, status: "D" }"""),
      Document("""{ item: "planner", qty: 75, size: { h: 22.85, w: 30, uom: "cm" }, status: "D" }"""),
      Document("""{ item: "postcard", qty: 45, size: { h: 10, w: 15.25, uom: "cm" }, status: "A" }""")
    )).execute()
    //End Example 55

    collection.count().execute() should equal(5)

    //Start Example 56
    collection.deleteMany(Document()).execute()
    //End Example 56

    collection.count().execute() should equal(0)

    collection.insertMany(Seq(
      Document("""{ item: "journal", qty: 25, size: { h: 14, w: 21, uom: "cm" }, status: "A" }"""),
      Document("""{ item: "notebook", qty: 50, size: { h: 8.5, w: 11, uom: "in" }, status: "A" }"""),
      Document("""{ item: "paper", qty: 100, size: { h: 8.5, w: 11, uom: "in" }, status: "D" }"""),
      Document("""{ item: "planner", qty: 75, size: { h: 22.85, w: 30, uom: "cm" }, status: "D" }"""),
      Document("""{ item: "postcard", qty: 45, size: { h: 10, w: 15.25, uom: "cm" }, status: "A" }""")
    )).execute()

    //Start Example 57
    import org.mongodb.scala.model.Filters
    collection.deleteMany(Filters.eq("status", "A")).execute()
    //End Example 57

    collection.count().execute() should equal(2)

    //Start Example 58
    collection.deleteOne(Filters.eq("status", "D")).execute()
    //End Example 58

    collection.count().execute() should equal(1)
  }
}
