+++
date = "2015-03-17T15:36:56Z"
title = "MongoDB Scala Driver"
type = "index"
+++

## MongoDB Scala Driver Documentation

Welcome to the MongoDB Scala driver documentation hub.

### Getting Started

The [Getting Started]({{< relref "getting-started/index.md" >}}) guide contains installation instructions
and a simple tutorial to get up  and running quickly.


{{% note %}}
This implementation is built upon the MongoDB Async Driver and mirrors the official API. 

The Scala driver builds upon the Java library and provides a native feeling driver, whilst using the core driver to have the latests MongoDB Support.

In general, you should **only need** the `org.mongodb.scala` and `org.bson` namespaces.  You should not need to import from the `com.mongodb` namespace as there are equivalent type aliases and companion objects in the Scala driver. The main exception is for advanced configuration via the builders in [MongoClientSettings]({{< apiref "org.mongodb.scala.MongoClientSettings$">}}) which is considered to be for advanced users.
{{% /note %}}


