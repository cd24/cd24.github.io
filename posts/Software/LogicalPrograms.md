---
title: Logical Protocols - Making Defaults Work
date: 2018-02-04
---

Every software engineer likes free code. The less code you write, the easier it is to reason about and test your code. This post will talk about yet one more way to make more of your code reusable. Let's consider the example of ordering a list in Swift. For the sake of clarity, let us assume we are implementing a standard library, with no existing code or libraries. We really don't care what's in the list, but rather that what's in the list is `Orderable`. To capture this behavior we can create the following protocol:

~~~swift
enum Order {
  case Greater
  case Equal
  case Less
}
protocol Orderable {
  func order(comparing: Self) -> Order
}
~~~

This enables us to write a new function on array for sorting elements in the list. Once the celebration has ended, we are now tasked with implementing a new function to find all members which are equal to a specified value `allEqual(to: Self) -> [Self]`. You may notice that the protocol `Orderable` can give us this information, we can just filter the list for those members which return `Equal`. However, this protocol also offers additional logic for determining if the elements are greater or less than. Requiring this additional logic for determining equality would be cumbersome, so we define a new protocol:

```Swift
protocol Equatable {
  func equals(other: Self) -> Bool
}
```

This protocol allows us to only have knowledge of equality and still get our wonderful new `allEquals` function. However, now all the `Orderable` kinds must also implement this functionality (presumably to return `order(:) == .Equal`). We might be tempted to implement `Equatable` for the `Orderable` type:

```Swift
extension Orderable: Equatable {
  func equals(other: Self) -> Bool {
    return self.order(comparing: other) == .Equal
  }
}
```

However, this is a compiler error: `Protocol Orderable cannot have an inheritance clause`. However, we don't want to have to reimplement `Equatable` for every `Orderable` instance... What we can do is define a new protocol in terms of the other two! Consider the following `Orderable_Equatable` protocol:

```Swift
protocol Orderable_Equatable: Orderable, Equatable {
}

extension Orderable_Equatable {
   func equals(other: Self) -> Bool {
    return self.order(comparing: other) == .Equal
  }
}
```

What this does is create a protocol which conforms to both `Orderable` and `Equatable`, but only provides the `Equatable` implementation. Now, for any type conforming to orderable, we can padd equality with this one liner:

```Swift
extension MyConcreteOrderableType: Orderable_Equatable {}
```

Applying this protocol to a type which doesn't implement `Orderable` will result in a compiler error because there is no instance for `order(comparing:)`. This allows you to share the implementation without adding the implementation to an inheritance tree, preserving composibility.

Even though this is helpful, it's rather unsightly. We can do better. Instead of creating a new protocol, we can extend the existing protocol with Swifts conditional Conformance. We can define an extension to `Equatable` which provides an implementation for `Ordered` instances:

```Swift
extension Equatable where Self: Ordered {
  func equals(other: Self) -> Bool {
    return self.order(comparing: other) == .Equal
  }
}
extension MyOrderedType: Equatable {}
```

Now we can just add the protocol we want, without having to remember which protocol provides the default implementation. If a default exists, we get it for free. 

However, this still has problems. If there are multiple default implementations Swift will require that you provide an implementation for the protocol. Otherwise the implementation is ambiguous.
