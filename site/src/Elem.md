= Elem =

== XML Elements ==

Elements in XML are markup that scopes other parts of the document, typically this scoping is seen as a containment relationship in XML object models.  Classical DOM follows this approach as does Scala XML.

== Declaring ==

${cscala}
  val ns = Namespace("uri:namespace")
  val pre = ns.prefixed("pre")
  val name = pre("localName")

  val attr = pre("attribute")

  val elem = Elem(name, 
    Attribs("a" -> "non namespaced", 
      attr -> "prefixed attribute"))
${cend}

''See [Attributes.html#Implicitly attributes] for an explanation of how -> works.''

== QName And Namespace Correctness ==

Elements also contain both declarations of namespaces, introducing new prefixes or default namespaces, and [Attributes.html Attributes]. Elements are declared with a given QName, which can also be a non-prefixed but namespaced.

${cxml}
  <!-- this element name has no prefix but a namespace -->
  <elem xmlns="uri:test"> 
    <!-- this element name also has no prefix but a namespace -->
    <child>
      <!-- this element name has no namespace and declares 
           that all child elements, by default, have no namespace -->
      <grandchild xmlns=""> 
      .....
    <!-- this element contains an attribute (without a namespace) 
	 but has its name with a namespace -->
    <child attribute="value">
    ....
    <!-- this element declares a new 
    	 namespace and prefix mapping. -->
    <child xmlns:pre="uri:anotherNamespace">
      <!-- this elements name is prefixed and within a namespace 
	   but the element declares a default namespace. -->
      <pre:grandchild xmlns="">
${cend}

As can be seen there are a number of complex combinations with namespaces and attributes that are possible and that the attribute mechanism itself is used to declare namespaces.

When supporting XML 1.0 only it can also be seen that storing the namespaces declared on an element is not necessary if all the [QNames.html QNames] are typed.  XML 1.1, however, allows the removal of a namespace prefix definition and requires that this information is stored.  If nothing else its quite helpful to be able to define where a namespace prefix declaration should appear.

Scales, however, does not conflate the ideas of namespace declaration, [Attributes.html attributes] or the [QNames.html qualified names] of elements.

== Elems Are Reusable ==

Scales Elem contains the QName, Attributes and optionally prefix declarations.  They do not have any notion of containment and as such are re-usable not only within a document but across documents.  Servers processing high volumes of related data can of course benefit from the reduced allocation costs, but the code can also benefit.

Declaring an Elem once and simply including it within a tree definition is not only made possible but encouraged:

${cscala}
  val unprefixedQName = "uri:namespace" :: "localName"
  val elem = Elem(unprefixedQName)
    
  val root = <(elem) /( 
    elem, elem, elem, 
    elem /( 
      elem 
    )
  )
  
  asString(root)
${cend}

gives (''formatting added to match root's above definition''):

${cxml}
<?xml version="1.0" encoding="UTF-8"?>
<localName xmlns="uri:namespace">
  <localName/><localName/><localName/>
  <localName>
    <localName/>
  </localName>
</localName>
${cend}

== Runtime Validation Checks ==

As Elem is created with a QName it is subject to all of QNames [QNames.html#QNames_in_Scales_-_Let_the_compliler_help_us correctness] and [QNames.html#Runtime_Validation runtime checks].

In addition Scales enforces that you cannot create an Elem with a prefix of xmlns or xml.
