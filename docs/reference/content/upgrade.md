+++
date = "2015-11-18T09:56:14Z"
title = "Changelog"
[menu.main]
  weight = 95
  pre = "<i class='fa fa-wrench'></i>"
+++

## Upgrade

### 2.0.0

#### SingleObservable
    
The addition of the `SingleObservable` trait allows for easy identification of `Observables` that return only a single element. 
For a SingleObservables `toFuture()` will return a `Future[T]` instead of `Future[Seq[T]]`, any code relying on this will need to be 
updated to reflect the new result type.

