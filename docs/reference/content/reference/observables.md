+++
date = "2015-05-14T08:01:00-00:00"
title = "Observables"
[menu.main]
  parent = "Reference"
  identifier = "Observables"
  weight = 75
  pre = "<i class='fa'></i>"
+++

## Observables

The MongoDB Scala Driver is an asynchronous and non blocking driver. Using the `Observable` model asynchronous events become simple, composable operations, freed from the complexity of nested callbacks.  

For asynchronous operations there are three interfaces [`Observable`]({{< apiref "org.mongodb.scala.Observable" >}}), [`Subscription`]({{< apiref "org.mongodb.scala.Subscription" >}}) and [`Observer`]({{< apiref "org.mongodb.scala.Observer" >}}).

{{% note %}}
The interfaces are the similar to `Publisher`, `Subscription` and `Subscriber` interfaces from the [reactive streams](http://www.reactive-streams.org/) JVM implementation.  However, we prefer the name `Observerable` to `Publisher` and `Observer` to `Subscriber` for readability purposes.
{{% /note %}}

## Observable
The [`Observable`]({{< apiref "org.mongodb.scala.Observable" >}}) represents a MongoDB operation which emits its results to the `Observer` based on demand requested by the `Subscription` to the `Observable`. 

## Subscription

A [`Subscription`]({{< apiref "org.mongodb.scala.Subscription" >}}) represents a one-to-one lifecycle of an `Observer` subscribing to an `Observable`.  A `Subscription` to an `Observable` can only be used by a single `Observer`.  The purpose of a `Subscription` is to control demand and to allow unsubscribing from the `Observable`.

## Observer

An [`Observer`]({{< apiref "org.mongodb.scala.Observer" >}}) provides the mechanism for receiving push-based notifications from the
`Observable`.  Demand for these events is signalled by its `Subscription`.

On subscription to an `Observable[TResult]` the `Observer` will be passed the `Subscription` via the 
`onSubscribe(subscription: Subscription)`. Demand for results is signaled via the `Subscription` and any results are passed to the 
`onNext(result: TResult)` method.  If there is an error for any reason the `onError(e: Throwable)` will be 
called and no more events passed to the `Observer`. Alternatively, when the `Observer` has consumed all the results from the `Observable` 
the `onComplete()` method is called.


## Back Pressure

In the following example, the `Subscription` is used to control demand when iterating an `Observable`. The default `Observer` implementation
automatically requests all the data. Below we override the `onSubscribe` method custom so we can manage the demand driven iteration of the 
`Observable`:

 ```scala
collection.find().subscribe(new Observer[Document](){

  var batchSize: Long = 10
  var seen: Long = 0
  var subscription: Option[Subscription] = None
  
  override def onSubscribe(subscription: Subscription): Unit = {
    this.subscription = Some(subscription)
    subscription.request(batchSize)
  }
  
  override def onNext(result: Document): Unit = {
    println(document.toJson())
    seen += 1
    if (seen == batchSize) {
      seen = 0
      subscription.get.request(batchSize)
    }
  }

  override def onError(e: Throwable): Unit = println(s"Error: $e")

  override def onComplete(): Unit = println("Completed")
})
```
## Observable Helpers

The `org.mongodb.scala` package provides improved interaction with the 
[Java `Observable`]({{< coreapi "com/mongodb/async/client/Observable.html" >}}) class via the 
[`ScalaObservable`]({{< apiref "org.mongodb.scala.ScalaObservable" >}}) implicit class. The extended functionality includes simple 
subscription via anonymous functions:

```scala
// Subscribe with custom onNext:
collection.find().subscribe((doc: Document) => println(doc.toJson()))

// Subscribe with custom onNext and onError
collection.find().subscribe((doc: Document) => println(doc.toJson()),
                            (e: Throwable) => println(s"There was an error: $e"))

// Subscribe with custom onNext, onError and onComplete
collection.find().subscribe((doc: Document) => println(doc.toJson()),
                            (e: Throwable) => println(s"There was an error: $e"),
                            () => println("Completed!"))
```

The `ObservableImplicits` trait also provides Monadic operators to make chaining and work with `Observable` instances simpler:

 - *[`andThen`]({{< apiref "org.mongodb.scala.ObservableImplicits$ScalaObservable@andThen[U](pf:PartialFunction[scala.util.Try[T],U]):org.mongodb.scala.Observable[T]">}})*: 
    Allows the chaining of Observables. 
 - *[`collect`]({{< apiref "org.mongodb.scala.ObservableImplicits$ScalaObservable@collect[S]():org.mongodb.scala.Observable[Seq[T]]">}})* :
    Collects all the results into a sequence.
 - *[`fallbackTo`]({{< apiref "org.mongodb.scala.ObservableImplicits$ScalaObservable@fallbackTo[U>:T](that:org.mongodb.scala.Observable[U]):org.mongodb.scala.Observable[U]">}})* :
    Allows falling back to an alternative `Observable` if there is a failure
 - *[`filter`]({{< apiref "org.mongodb.scala.ObservableImplicits$ScalaObservable@filter(predicate:T=>Boolean):org.mongodb.scala.Observable[T]">}})* :
    Filters results of the `Observable`.
 - *[`flatMap`]({{< apiref "org.mongodb.scala.ObservableImplicits$ScalaObservable@flatMap[S](mapFunction:T=>org.mongodb.scala.Observable[S]):org.mongodb.scala.Observable[S]">}})* :
    Create a new `Observable` by applying a function to each result of the `Observable`.
 - *[`foldLeft`]({{< apiref "org.mongodb.scala.ObservableImplicits$ScalaObservable@foldLeft[S](initialValue:S)(accumulator:(S,T)=>S):org.mongodb.scala.Observable[S]">}})* :
    Creates a new Observable that contains the single result of the applied accumulator function.
 - *[`foreach`]({{< apiref "org.mongodb.scala.ObservableImplicits$ScalaObservable@foreach[U](doOnEach:T=>U):Unit">}})* :
    Applies a function applied to each emitted result.
 - *[`head`]({{< apiref "org.mongodb.scala.ObservableImplicits$ScalaObservable@head():scala.concurrent.Future[T]">}})* :
    Returns the head of the `Observable` in a `Future`.
 - *[`map`]({{< apiref "org.mongodb.scala.ObservableImplicits$ScalaObservable@map[S](mapFunction:T=>S):org.mongodb.scala.Observable[S]">}})* :
    Creates a new Observable by applying a function to each emitted result of the Observable.
 - *[`recover`]({{< apiref "org.mongodb.scala.ObservableImplicits$ScalaObservable@recover[U>:T](pf:PartialFunction[Throwable,U]):org.mongodb.scala.Observable[U]">}})* :
    Creates a new `Observable` that will handle any matching throwable that this `Observable` might contain by assigning it a value of 
    another `Observable`.
 - *[`recoverWith`]({{< apiref "org.mongodb.scala.ObservableImplicits$ScalaObservable@recoverWith[U>:T](pf:PartialFunction[Throwable,org.mongodb.scala.Observable[U]]):org.mongodb.scala.Observable[U]">}})* :
    Creates a new Observable that will handle any matching throwable that this Observable might contain.
 - *[`toFuture`]({{< apiref "org.mongodb.scala.ObservableImplicits$ScalaObservable@toFuture():scala.concurrent.Future[Seq[T]]">}})* :
    Collects the `Observable` results and converts to a `Future`.
 - *[`transform`]({{< apiref "org.mongodb.scala.ObservableImplicits$ScalaObservable@transform[S](mapFunction:T=>S,errorMapFunction:Throwable=>Throwable):org.mongodb.scala.Observable[S]">}})* :
    Creates a new `Observable` by applying the resultFunction function to each emitted result.
 - *[`withFilter`]({{< apiref "org.mongodb.scala.ObservableImplicits$ScalaObservable@withFilter(p:T=>Boolean):org.mongodb.scala.Observable[T]">}})* :
    Provides for-comprehensions support to Observables.
 - *[`zip`]({{< apiref "org.mongodb.scala.ObservableImplicits$ScalaObservable@zip[U](that:org.mongodb.scala.Observable[U]):org.mongodb.scala.Observable[(T,U)]">}})* :
    Zips the values of this and that `Observable`, and creates a new `Observable` holding the tuple of their results.

