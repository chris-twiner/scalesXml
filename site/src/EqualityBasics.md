= XML Equality Basics =

The Scales Xml equality framework aims to help with both testing applications (similar to XmlUnit) that use Scales and also for runtime comparison activities, e.g. if an element has this attribute with this value do X.

The Scales equality framework does not throw, but returns information allowing decisions to be made.  In the case of `===` a simple boolean is returned, in the case of `compare` a full ADT and path can be returned diagnosing the first difference found.

The `compare` function is used by the Equal instances and is documented [XmlComparison.html#The_compare_Function here].

'''NB''' Scala 2.8.x support for the Equality framework is experimental.  Importing FromEqualsImplicit._ enables the use of === from Scalaz.  Unfortunately due to 2.8.x compiler issues the implicit resolution does not correctly function and may cause compiler crashes. 

== How To Use ==

Scales Xml Equality leverages two type classes, XmlComparison and the Scalaz Equal typeclass to provide comparison via a simple === .  As such Scalaz must be imported (best after Scales imports to avoid Tree import issues):

${cscala}
  import scalaz._
  import Scalaz._
${cend}

Testing equality is therefore as simple as:

${cscala}
  val t1 = Text("fred")
  val t2 = Text("fred")

  assertTrue("t1 == t2", t1 == t2) // equals
  assertTrue("t1 === t2", t1 === t2) // Scalaz Equal type class

  assertTrue("t1 compare t2", 
    compare(Nil, t1, t2).isEmpty ) // XmlComparison type class
${cend}

Wherever an XmlComparison exists an Equal instance can be created.  The results of a compare include both a path to the difference and a fully pattern matchable XML difference ADT.

== Types Covered ==

The full set of the Scales Xml Model is covered by the equality framework:

* QName
* Attribute
* Attributes
* XmlItem

* Elem
* XmlTree
* XmlPath
* Anything that can be seen as <nowiki>Iterator[PullType]</nowiki>

* Doc and various DocLike implementations

QNames by default do not compare with the prefix(unlike Canonical Xml, where string comparisons including prefixes are expected), only the namespace (as per =:=).  This implies that documents created by different systems using different prefixes are still comparable, a different implicit default <nowiki>Equal[QName]</nowiki> instance can change that behaviour.

XmlTrees/XmlPath's etc are converted to <nowiki>Iterator[PullType]</nowiki> in order to compare.  No attempt to match DTDs or encoding are made, but the rest of a given document (Doc and DocLike implementations) will be.

Within the comparison framework the comparison for all the types are combined, the QName Equal typeclass is used throughout, including for the Attribute comparison, which is used in turn by the Elem - which is finally used by Stream comparisons.  

This lookup is performed implicitly, allowing for individual parts to be swapped out, if the developer wants prefixes to be tested.  Either use name based overriding in the relevant scope or mix the traits differently to provide custom behaviour (and not import ScalesXml._)

_Note_ The three different kinds of QNames each have a different type and, as such, using === to compare different types will not work.  Using compare, however, will:

${cscala}
  val ns = Namespace("uri:prefixed")

  val p = ns.prefixed("p")

  val nonPrefixedQName = ns("a1") 
  // prefixed
  val prefixedQName = p("a1")

  // both of the above are semantically the same
  assertTrue("compare(nonPrefixedQName, prefixedQName).isEmpty", compare(nonPrefixedQName, prefixedQName).isEmpty)
${cend}

== Why Join Adjacent Text and CData? ==

Scales Xml equality makes three default design decisions, prefixes aren't generally relevant only the namespace is (unless you tell it to use [XmlComparison.html#QName_Token_Handling QName Token comparison]) and to join adjacent CData and Text.

The reason for joining adjacent CData and Text nodes is to simplify the comparison of text.  CData can always be written as Text nodes, and a parser is free to "split" a single logical Text node into multiple smaller text nodes.  Scales neither forces joining of the text nodes at parse time nor when adding child nodes, as such to usefully compare they must be joined.

This also allows testing content from different sources without issue.

== Removing Comments And PIs == 

The default comparison logic treats both Comments and PIs as relevant for comparison.  This design choice meets expectations for the majority of XML documents.

In the event that Comments and PIs should not be compared the implicits can be overridden in scope with:

${cscala}
  val root = po("root")
  val child = po("child")

  import LogicalFilters._

  implicit def toDefaultStreamComparison[T](
  	   implicit tv : T => StreamComparable[T], 
	   ic : XmlComparison[XmlItem],
	   ec : XmlComparison[Elem], 
	   qe : Equal[QName], , qe : Equal[QName], 
	   qnameTokenComparison : Option[(ComparisonContext, String, String) => Boolean]) : XmlComparison[T] = 
      new StreamComparisonWrapper( 
        new StreamComparison( 
          x => removePIAndComments(joinText(x))
        )( ic, ec, qe, qnameTokenComparison) 
      )

  val x1 = 
      <(root) /( "0", "1", CData("2"), Comment("c2"), "3", "4", PI("i","s"),
	child /( 
	  "s1", CData("s2"), Comment("cs2"), "s3"
	),
      	child /( 
	  CData("s22"), Comment("cs22"), PI("i","s"), "s23" 
	),
        PI("i","s"), "5", CData("6"), Comment("c6") )


  val x2 = 
      <(root) /( "0","1",CData("2"), "3","4", 
	child /( 
	    "s1", CData("s2"), "s3" 
	),
	child /( 
	    CData("s22"), "s23" 
	),
	"5", CData("6") )
    
  assertTrue( " x1 === x2 ", x1 === x2)
${cend}

== Why Not Use Canonical XML? ==

Testing Xml Equality is not always straightforward, a standard approach however exists : Canonical Xml - a defined w3c standard approach to serialization.  Canonical Xml treats QName prefixes themselves as relevant, if an XML processor changes a prefix, that document is no longer comparable under Canonical Xml (No Namespace Prefix Rewriting).

Whilst the justifications for the prefix rewriting rule in Canonical Xml is, within the context of embedded XPaths or XML QNames (their prefixes only make sense within that document), understandable Scales takes the position that this is far rarer an occasion than simple Xml as a data transport usage.  However, as with the rest of Scales, this default too is customisable.

The problem, and it stops the Canonical Xml reasoning as well, is that a producing application can re-write the prefixes for embedded QNames or XPaths before sending.  WSDM applications often meet this (see Apache Muse for examples of this).  Scales is of course, by default, not aware of such usage - see [XmlComparison.html#QName_Token_Handling here] for details on how to configure Scales to be QName token aware.  In short, it can be as simple as declaring this in the correct scope:

${cscala}
  implicit val defaultQNameTokenComparison = Option(qnamesEqual _)
${cend}

Canonical Xml also forces redundant namespace declarations to be removed (Superfluous Namespace Declarations).  Scales typically only uses namespace declarations for predictable document processing - i.e. loading and saving should be 1:1 in usage - however it also can if necessary leverage declarations for [XmlComparison.html#QName_Token_Handling QName Token handling] in both Attribute values and Text/CData nodes.

Similarly the following approaches to Canonical Xml actually may break data assumptions of equality:

* normalize line feeds
* normalize attribute values
* Default attributes are added to each element

The latter may cause issues with certain receiving processors and depends on a validation / schema enrichment working.  Scales concerns itself with the documents actually being compared.

In short - Scales offers a typed and more flexible approach to equality than Canonical Xml handling.