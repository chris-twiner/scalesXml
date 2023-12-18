= XmlComparison - What, Where & How Was It Different =

The XmlComparison typeclass is fairly simple with one function:

${cscala}
  def compare( calculate : Boolean , context : ComparisonContext, left : T, right : T) : Option[(XmlDifference[_], ComparisonContext)]
${cend}

All instances are created via defs and implicit lookup.  As such, where runtime performance of comparison is a concern you should cache an appropriately scoped instance via `implicitly`.

The reason for using defs is that the behaviour of any one XmlComparison instance is configurable.  Substituting the `defaultQNameTokenComparison` in a given scope will affect both XmlItem and Attribute value comparison.

To retrieve the full information about what changed use the compare function (mixed into the scales.xml package):

${cscala}
  def compare[T : XmlComparison]( left : T, right : T) : Option[(XmlDifference[_], ComparisonContext)] =
    implicitly[XmlComparison[T]].compare(true, ComparisonContext(), left, right)
${cend}

As can be seen, this simply provides a starting context and informs the framework to calculate both the path to a difference and a detailed ADT for what was different (XmlDifference).

== The compare Function ==

Any implementing instance of XmlComparison must provide a compare implementation, for example a user provided XmlItem comparison, it is recommended to respect the following conventions:

# When calculate is false, its a signal that no detailed result is expected and is likely called from ===
# Use the comparison context to handle namespace lookups
# Respect defaultQNameTokenComparison, if code has defined it in scope you must assume the developer wants at least the context to be generated (irrespective of calculate being false or not).

=== The calculate Parameter ===

The calculate parameter when false indicates that any difference returned as scales.xml.equals.SomeDifference.noCalculation, i.e. a dummy Some value.  The purpose of the false parameter is for a simple comparison via === and the derived Equal type class.

A value of false will also disable, unless a defaultQNameTokenComparison is defined in scope, the generation of relevant ComparisonContexts, further reducing allocation for simple equality checks.

However when set true it instructs both the use of ComparisonContexts and that the return value is as detailed as possible.

=== ComparisonContext ===

ComparisonContext is a stack of potential namespace contexts (for both the left and right side), the parent context and the BasicPath from the start of the compare.  BasicPath is defined as:

${cscala}
  // {ns}Local -> count
  type BasicPathA = (QName, Map[String, Int])
  type BasicPath = List[BasicPathA] 
${cend}

and maintains a count against each QName as it navigates down a given tree.  ComparisonContext provides a simplified string path via the pathString function, for example the following output:

/{}root[1]/{uri:prefixed}elem[2]/{uri:prefixed}elem[1]

In addition to identifying where something is different it could be used to decide if the difference is relevant within a custom XmlComparison instance.

NamespaceContext is used by both XmlComparison and by the serialisation mechanisms and acts a stack of prefix -> namespace mappings that have been defined in the given trees.  The namespace prefixes used by PrefixedQName attributes and element are combined with any defined prefixes from the elements namespace map.

When comparing a stream the position within that iterator is also set upon returning from comparison - streamPosition.  Calling a ComparisonContexts toDifferenceAsStream with one of the compared xml objects will provide a <nowiki>Stream[PullType]</nowiki> with the xml from the start of the document to the diffence.  Callers are responsible for ensuring the input is restartable (i.e. if it was over an http stream that a bufferred stream was used) and that the conversion function matches (by default it follows the same convention of joinTextAndCData that the stream comparison uses).  For example:

${cscala}
    val x = loadXml(scales.utils.resource(this, "/data/Nested.xml")).rootElem

    // create a difference
    val y = x.fold_!( _.\*.\*("urn:default"::"ShouldRedeclare") )(_ => Remove())

    val Some((diff, context)) = compare[XmlTree](x, y)

    // the stream from the start of the document to the difference
    val upTo = context.toDifferenceAsStream(x)

    // as a string
    val upToStr = asString(upTo.iterator)

    assertEquals(232, upToStr.size)
${cend}

=== Return Value ===

The return value indicates:

# The presence of a difference (Some vs None)
# The difference itself (XmlDifference), and
# The relevant context for this difference (allowing access to namespaces and path)

If the calculate parameter is false the default implementations return scales.xml.equals.SomeDifference.noCalculation.  Custom XmlComparison instances my choose to return other values but it is not recommended.

== XmlDifference ==

The XmlDifference ADT provides information about the type of difference and provides the objects themselves that contained the difference.  The XmlComparison framework attempts to provide, when using calculate true, finely detailed information about what was different via case classes, allowing simplified pattern matching to analyse differences.

The full ADT is present via the [./doc/scales/xml/equals/XmlDifference.html scala docs, Known Subclasses] or directly via [./api.sxr/scales/xml/equals/XmlDifference.scala.html the source]. To aid explanation the following are presented:

* DifferentTypes( left : PullType, right : PullType)
* AttributeValueDifference( left : Attribute, right : Attribute )
* DifferentNumberOfMiscs( left : Miscs, right : Miscs, isProlog : Boolean )
* ElemAttributeDifference( left : Elem, right : Elem, attributesDifference : AttributesDifference )

DifferentTypes is returned when a given left PullType is of different type to the right PullType.  AttributeValueDifference indicates the names are the same but the values differ between the two attributes.

DifferentNumberOfMiscs indicates that the prolog (isProlog == true) or end miscallaeneous (isProlog == false) have a different count and provides the Miscs themselves for further investigation.  ElemAttributeDifference contains the elements that contained an AttributesDifference, which in turn has a number of possible matching types (DifferentNumberOfAttributes, DifferentValueAttributes and MissingAttributes).

== QName Token Handling ==

QName Tokens are attributes or Text nodes that contain a prefixed qname such as:

${cxml}
  <elem xmlns:pre="uri:test" value="pre:alocal">pre:local</elem>
${cend}

Applications are free to redefine the prefix used between runs, making things difficult for comparison.  Scales provides two simple mechanism to help solve this problem:

# ComparisonContext
# defaultQNameTokenComparison

The former is covered above and provides the NamespaceContext's for the left and right side objects under comparison.  This information is calculated either when the  defaultQNameTokenComparison is defined or calculate is true.

The defaultQNameTokenComparison is defined as:

${cscala}
  implicit val defaultQNameTokenComparison : Option[(ComparisonContext, String, String) => Boolean] = None
${cend}

Simply defining this function will also enable the creation of the contexts (irrespective of the value of calculate).  The parameters are simply the ComparisonContext together with the left and right object's string values.

As such a simple way to enable qname comparisons is to define and override, within an appropriate scope:

${cscala}
  implicit val defaultQNameTokenComparison = Option(qnamesEqual _)
${cend}

qnamesEqual is defined as:

${cscala}
  def qnamesEqual(context : ComparisonContext, str : String, str2 : String) = {
    // split both, if one has and the other not, then its false anyway
    val sp1 = str.split(":")
    val sp2 = str2.split(":")
    if (sp1.size == 2 && sp2.size == 2) {
      sp1(1) == sp2(1) && { // values match
	// look up prefixes
	(for{ lnc <- context.leftNamespaceContext
	     rnc <- context.rightNamespaceContext
	     lns <- lnc.mappings.get(sp1(0))
	     rns <- rnc.mappings.get(sp2(0))
	   } yield lns == rns
        ).
	getOrElse(false)
      }
    } else 
      str == str2
  }
${cend}

Which should show an obvious limitation, it can only work when there is a single qname in the text.  This would stop it being able to handle embedded XPaths for example.

As such the functionality is exposed to the library users to customise.  Any useful other implementations are more than welcome as contributions :-)