# Type Classes with Functional Dependencies

TODO: Got lost on sections 5 and onwards, should revisit

### 5.1

Following standard terminology, a _relation R_ over and indexed family of sets {D(i)}(i ∈ I) is just a set of _tuples_, each of which is an indexed family of values {t(i)}(i∈I) such that t(i) ∈ D(i) for each i ∈ I. More formally, _R_ is just a subset of Π i ∈ I. D(i), where a tuple t ∈ (Π i ∈ I. D(i)) is a function that maps each index value i ∈ I to a value t(i) ∈ D(i).
In the special case where I = {1, ..., n}, this reduces to the familiar special case where tuples are values (t(1), ..., t(n)) ∈ D(i) × ... × D (n).
If X ⊆ I, then we write t(X), pronounced "t at X", for the restriction of a tuple t to X.
Intuitively, t(X) just picks out the values of t for the indices appearing in X, and discards any remaining components.

### 5.2 Functional Dependencies

index set I
X -> Y (X determines Y)
