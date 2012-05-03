package scales.xml.xpath

import scales.xml._
import scales.utils._
import scala.collection.generic.CanBuildFrom
import scales.utils.one

import ScalesXml._

/**
 * Functions for use with attributes
 */
@deprecated(message="Please import Functions._ instead - since 0.3")
object Attributes {

    /**
     */
    @deprecated(message="Please import Functions._ instead - since 0.3")
    object Functions {

      /** curried to allow direct drop in for predicates */
      def localName(localName: String)(implicit attribute: AttributePath): Boolean = attribute.attribute.name.local == localName

      /** curried to allow direct drop in for predicates */
      def exact(qname: QName)(implicit attribute: AttributePath): Boolean = attribute.attribute.name ==== qname

      /** returns the qname of an AttributePath, using implicit scope */
      def qname(implicit attribute: AttributePath): String = attribute.attribute.name.qName

      /** returns the pqName of an AttributePath, using implicit scope */
      def pqName(implicit attribute: AttributePath): String = attribute.attribute.name.pqName

      /** returns the value of an AttributePath, using implicit scope */
      def value(implicit attribute: AttributePath): String = attribute.attribute.value

      /** returns the value of an AttributePath, using implicit scope */
      def text(implicit attribute: AttributePath) = value

      /** true if the element contains this attribute */
      def *@(qName: QName)(implicit path: XmlPath) = path.tree.section.attributes.contains(qName)

    }

    val *@ = (qName: QName) => (path: XmlPath) => Functions.*@(qName)(path)
    val localName = (l: String) => (a: AttributePath) => Functions.localName(l)(a)
    val exact = (q: QName) => (a: AttributePath) => Functions.exact(q)(a)
  }

  /**
   * Functions related to text nodes
   */ 
  @deprecated(message="Please import Functions._ instead - since 0.3")
  object OldTextFunctions {

    /**
     * Either the value of an XmlItem or the direct text subchildren.
     * @param xmlpath
     * @return
     */
    def value(implicit xmlpath: XmlPath) =
      if (xmlpath.isItem)
        xmlpath.item.value
      else
        xmlpath.flatMap(x => if (isText(x)) Some(x.item.value)
        else None).addString(new StringBuilder()).toString

  }

  /**
   * Functions related to Elems, including the string( . ) xpath function
   */ 
  @deprecated(message="Please import Functions._ instead - since 0.3")
  object Elements {

    /**
     * Only use these items on XmlPaths that are actual trees, doing otherwise is a runtime exception, if used in XPath predicates then they will always be trees.
     *
     * Note the majority of these functions have an implicit arguement that matches the current context of a given predicate.  This allows:
     *
     *
     */
    @deprecated(message="Please import Functions._ instead - since 0.3")
    object Functions {

      /** curried to allow direct drop in for predicates, if it is an item then it will return false */
      def localName(localName: String)(implicit path: XmlPath): Boolean = path.tree.section.name.local == localName

      /** curried to allow direct drop in for predicates, if it is an item then it will return false */
      def exact(qname: QName)(implicit path: XmlPath): Boolean = path.tree.section.name ==== qname

      /** returns the localname of an XmlPath, using implicit scope */
      def localName(implicit path: XmlPath): String = path.tree.section.name.local

      /** returns the qname (pre:local) of an XmlPath, using implicit scope */
      def qname(implicit path: XmlPath): String = path.tree.section.name.qName

      /** returns the qualified name ({namespace}local) of an XmlPath, using implicit scope */
      def qualifiedName(implicit path: XmlPath): String = path.tree.section.name.qualifiedName

      /** returns the qname of an XmlPath with prefix if defined pre:{ns}qname, using implicit scope */
      def pqName(implicit path: XmlPath): String = path.tree.section.name.pqName

      /** returns the text of an XmlPath, using implicit scope. Returns all text children joined, does not trim whitespace etc */
      def text(implicit path: XmlPath): String = {
        path.tree.fold(new StringBuilder()) { (walker, sb) =>
          if (walker.isLeft) {
            walker.left.get match {
              case Text(text) => sb.append(text)
              case _ => ()
            }
          }
          sb
        }.toString
      }

      /**
       * XPath standard name
       */ 
      def string(implicit path: XmlPath): String = text

      def normalizeSpace(implicit path: XmlPath) = normalizeSpaceS(text)

    }

    val !! = (f: (XmlPath) => Boolean) => (a: XmlPath) => {
      val res = f(a)
      !res
    }
    val localName = (l: String) => (a: XmlPath) => Functions.localName(l)(a)
    val exact = (q: QName) => (a: XmlPath) => Functions.exact(q)(a)
  }
