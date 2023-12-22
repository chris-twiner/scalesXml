# Pulling Repeated Sections

Scales leverages and extends Scalaz Iteratees to allow resuming an Iteratee.  This resuming action is simply returning the current value and the next continuation when done ([ResumableIter](../../site/scales-xml_{{site_scala_compat()}}/scaladocs/index.html#scales.utils.package@ResumableIter%5bE,A%5d:ResumableIter%5bE,A%5d)).  The [iterate function](../../site/scales-xml_{{site_scala_compat()}}/scaladocs/index.html#scales.xml.package@iterate(List%5bQName%5d,Iterator%5bPullType%5d):FlatMapIterator%5bXmlPath%5d), as shown [here](PullParsing.md#simple-reading-of-repeated-sections), uses this approach to provide a single path repeating section.

Many documents however have a more complex structure, of many repeated or alternating structures, the following shows the various structures supported by the combination of onDone and onQNames:

## Supported Repeating Section Examples

Its far easier to discuss the solution with a few examples of the problem: 

### Alternating and Repeating Elements 

```xml
  <root>
    <nested>
      <ofInterest> <!-- Collect all of these -->
        <lotsOfInterestingSubTree>
        </lotsOfInterestingSubTree>
      </ofInterest>
      <alsoOfInterest> <!-- Collect all of these -->
	just some text
      </alsoOfInterest>
    </nested>
...
    <nested>
....
  </root>
```

__It should be noted that monadic serial composition of onQNames would also work here, onDone is not absolutely necessary, although as we will see it is more general..__

### Grouped Repeating

```xml
  <root>
    <nested>
      <ofInterest> <!-- Collect all of these -->
        <lotsOfInterestingSubTree>
        </lotsOfInterestingSubTree>
      </ofInterest>      
    </nested>
...
    <nested>
      <alsoOfInterest> <!-- Collect all of these -->
	just some text
      </alsoOfInterest>	
    </nested>
....
  </root>
```

### Repeating Nested

```xml
  <root>
    <nested>
      <ofInterest> <!-- Collect all of these -->
        <lotsOfInterestingSubTree>
          <smallKeyValues> <!-- Collect all of these -->
            <key>toLock</key>
            <value>fred</value>
          </smallKeyValues>
        </lotsOfInterestingSubTree>
      </ofInterest>
    </nested>
...
    <nested>
....
  </root>
```

### Sectioned Grouped Repeating

```xml
  <root>
    <section>
      <!-- Necessary for processing the below events -->
      <sectionHeader>header 1</sectionHeader>

      <ofInterest> <!-- Collect all of these -->
        <lotsOfInterestingSubTree>
	  <value>1</value>
        </lotsOfInterestingSubTree>
      </ofInterest>
      <ofInterest> <!-- Collect all of these -->
        <lotsOfInterestingSubTree>
	  <value>2</value>
        </lotsOfInterestingSubTree>
      </ofInterest>
      <ofInterest> <!-- Collect all of these -->
        <lotsOfInterestingSubTree>
	  <value>3</value>
        </lotsOfInterestingSubTree>
      </ofInterest>
    </sectionHeader>
...
    <sectionHeader>
      <!-- Necessary for processing the below events -->
      <sectionHeader>header 2</sectionHeader>
....
  </root>
```

## Pull Parsing ResumableIter'atees

ResumableIter is an Iteratee over E that instead of returning just a <nowiki>Done[R] returns Done[(R, NextResumableIter)]</nowiki>.  The next ResumableIter stores the calculation up until the point of returning, allowing the calculation to be resumed.

To process the above examples we make use of this and the [onDone Iteratee](../../site/scales-xml_{{site_scala_compat()}}/scaladocs/index.html#scales.utils.package@onDone%5bE,A%5d(List%5bResumableIter%5bE,A%5d%5d):ResumableIterList%5bE,A%5d).  This takes a list of ResumableIter and applies the input element to each of the Iteratees in that list, Done here returns both a list of the Iteratees which evaluate to Done for that input and (of course) the next continuation of onDone.

A simple, and recommended, way to leverage onDone is with the [foldOnDone function](../../site/scales-xml_{{site_scala_compat()}}/scaladocs/index.html#scales.utils.package@foldOnDone%5bE,A,ACC,F%5b_%5d%5d(F%5bE%5d)(ACC,ResumableIter%5bE,A%5d)((ACC,A)⇒ACC)(Enumerator%5bF%5d):ACC):

```scala
  val Headers = List("root"l,"section"l,"sectionHeader"l)
  val OfInterest = List("root"l,"section"l,"ofInterest"l)

  val ofInterestOnDone = onDone(List(onQNames(Headers), onQNames(OfInterest)))

  val total = foldOnDone(xml)( (0, 0), ofInterestOnDone ){ 
    (t, qnamesMatch) =>
    if (qnamesMatch.size == 0) {
      t // no matches
    } else {
      // only one at a time possible for xml matches (unless multiple identical onQNames are passed to onDone).
      assertEquals(1, qnamesMatch.size)
      val head = qnamesMatch.head
      assertTrue("Should have been defined",head._2.isDefined)
	  
      // we should never have more than one child in the parent
      // and thats us
      assertEquals(1, head._2.get.zipUp.children.size)

      val i = text(head._2.get).toInt
      // onQNames always returns the list as well as the XmlPath to allow matching against the input.
      if (head._1 eq Headers) {
	assertEquals(t._1, t._2)
	// get new section
	(i, 1)
      } else (t._1, i)
    }
  }
 
  assertEquals(total._1, total._2)
```
