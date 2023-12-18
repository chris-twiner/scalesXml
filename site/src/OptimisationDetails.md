= Scales Xml Optimisation Details =

== ImmutableArrayProxy ==

In order to reduce memory consumption Scales uses an abstraction over array like structures - ImmutableArrayProxy.  Vector is appropriate for large structures (> 30 children) but inappropriate for smaller collections, taking many Mb of unnecessary memory usage.

The same is true of a simple immutable array, the costs are too high for items less than 4 in size.

As such Scales provides a One, Two and Three Seq, an ImmutableArray for less than 32 and a Vector wrapper for greater than 32.  There is also ImmutableArrayAll, which reduces offset information to further reduce memory usage.

The builder itself is also optimised to allocate as little as possible and allow for re-use in the common case.

== EitherLike ==

A simple concession to memory performance (and CPU performance) was the replacement of Either as a container and the creating of EitherLike.  EitherLike has the similar properties but is a trait applied to classes, fold and projections are still present but the memory usage from the level of indirection is removed.  Tests showed that 10-15% of the memory usage was held purely by the Either ADT, and performance was around 4-5% impact across the board due to the one level of indirection.

EitherLike is currently not used for pull parsing due to Scalas / JVMs understandable erasure limitation of inheriting twice with different type parameters.

== TreeOptimisation ==

The TreeOptimisation trait provides a simple way to mix in optimisations on Tree, see the [./doc/scales/xml/parser/strategies/TreeOptimisation.html docs] and [./api.sxr/scales/xml/parser/strategies/TreeOptimisations.scala.html source] for example implementations (QNameTreeOptimisation and QNameElemTreeOptimisation).

The base Tree type is itself (as of 0.3) just an interface, allowing reduced memory usage in common cases (eg. repeated name value style elements) and a simple xml object conversion approach, where children may be lazily mapped.

The ParsingPerformance tests also demonstrate the possibility to optimise away the element itself if a given document often repeats element content.

When an optimised tree is modified (via copy) then it may still be further optimised or default to a normal Tree when no simple optimisation is possible.

== QName and Elem Memory Usage ==

The QName and Elem structures are optimised to only keep references to items that are needed.  NoNamespaceQName will only have a single data member reference to the local name and UnprefixedQName has no prefixed stored.

Elem is more interesting, given it has 4 relevant states, but takes the same approach.  If the attributes are empty but there are namespaces upon creation then the resulting Elem (NoAttribsElem) will not contain the reference.

These simple optimisations positively affect memory usage considerably for large documents.

== TreeProxies and Builders ==

The Scales XmlParser infrastructure attempts to cache builders at each level of the tree.  If there are more than 3 children at any given element then the underlying builder (VectorBuilder / Pointer or ImmutableArray) cannot be re-used.

The vast majority of XML will typically end at the leafs with an elem that has only one logical text node, the builders generating these leaves can be re-used.

This level based builder caching and the heavily inlined ImmutableArrayProxy builders and TreeProxies together result in somewhat non idiomatic code but increases performance by 5-8%.