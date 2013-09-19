package scales.utils.collection
import array._

class ArraySetTest extends junit.framework.TestCase {

  import junit.framework.Assert._

  object StringS {
    implicit val equal = scalaz.Equal.equal { (a: String, b: String) => a == b }
    def arrM = implicitly[ClassManifest[String]]
  }

  type A = String

  trait Impl extends ArraySetsFactory[A]{
    /**
     * @returns the equality Equal type class instance used
     */ 
    def equal: scalaz.Equal[A] = StringS.equal

    implicit def arrayManifest: ClassManifest[A] = StringS.arrM

    def emptySet: ArraySet[A] = new EmptyArraySet[A] with Impl

    def one(a: A): ArraySet[A] = new ArraySetOne[A] with Impl {
      val one = a
    }

    def two(a: A, b: A): ArraySet[A] = new ArraySetTwo[A] with Impl {
      val one = a
      val two = b
    }

    def three(a: A, b: A, c: A): ArraySet[A] = new ArraySetThree[A] with Impl {
      val one = a
      val two = b
      val three = c
    }

    def four(a: A, b: A, c: A, d: A): ArraySet[A] = new ArraySetFour[A] with Impl {
      val one = a
      val two = b
      val three = c
      val four = d
    }

    def five(a: A, b: A, c: A, d: A, e: A): ArraySet[A] = new ArraySetFive[A] with Impl {
      val one = a
      val two = b
      val three = c
      val four = d
      val five = e
    }

    def more(allTheAs: Array[A]): ArraySet[A] = new ArraySetArray[A] with Impl {
      val ar = allTheAs
    }
  }

  val StringSet = new Impl {}

  
  class HolderSetData {
    val emptyIS = StringSet.emptySet
    val withOne = emptyIS + "1"
    val withTwo = withOne + "2"
    val withThree = withTwo + "3"
    val withFour = withThree + "4"
    val withFive = withFour + "5"
    val withThreeArray = StringSet.more(Array("1","2","3"))
    val withFourArray = StringSet.more(Array("1","2","3","4"))
    val withFiveArray = StringSet.more(Array("1","2","3","4", "5"))
    val withSixArray = StringSet.more(Array("1","2","3","4", "5", "6"))
    val withSevenArray = StringSet.more(Array("1","2","3","4", "5", "6", "7"))
    val withEightArray = StringSet.more(Array("1","2","3","4", "5", "6", "7", "8"))
    val withNineArray = StringSet.more(Array("1","2","3","4", "5", "6", "7", "8", "9"))
    val withTenArray = StringSet.more(Array("1","2","3","4", "5", "6", "7", "8", "9", "10"))
    val withElevenArray = StringSet.more(Array("1","2","3","4", "5", "6", "7", "8", "9", "10", "11"))
    val withTwelveArray = StringSet.more(Array("1","2","3","4", "5", "6", "7", "8", "9", "10", "11", "12"))
    val withThirteenArray = StringSet.more(Array("1","2","3","4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))
  }

  val setHolder = new HolderSetData()

  def testSetContent: Unit = {
    import setHolder._
    assertSetS("Empty should have size 0", 0, emptyIS)
    assertSetS("One", 1, withOne)
    assertSetS("Two", 2, withTwo)
    assertSetS("Three",3, withThree)
    assertSetS("Four", 4, withFour)
    assertSetS("Five", 5, withFive)
    assertSetS("Six", 6, withSixArray)
    assertSetS("Ten", 10, withTenArray)
    assertSetS("Thirteen", 13, withThirteenArray)
  }

  def nSet(upTo: Int) = (1 to upTo).map(_.toString).toSet

  def assertSetS(reason: String, expected: Int, set: ArraySet[A]) {
    assertSet(reason, nSet(expected), set)
  }

  def assertSet[AN](reason: String, expected: Set[AN], set: ArraySet[AN]) {
    val comp = expected
    val orig = set.toSet
    if (comp.size != orig.size)
      fail(reason + " - had differing size, expected "+ expected +" got "+orig.size)
    else {
      val diff = comp &~ orig
      if (diff.nonEmpty) {
	fail(reason + " - had differing elements than expected: " + diff)
      }
    }
  }

  def testFiveToManyToFive: Unit = {
    import setHolder._
    assertTrue("fix is not more", withFive.isInstanceOf[ArraySetFive[String]])
    val six = withFive + "6"
    assertTrue("six is more", six.isInstanceOf[ArraySetArray[String]])

    assertSetS("six",6, six)

    val andback6 = six - "6"
    assertSetS("and back 6", 5, andback6)

    def --(in: Int) {
      val str = in.toString
      val back = six - str
      assertTrue("and back was not more", back.isInstanceOf[ArraySetFive[String]])
      assertSet("and back "+str, nSet(6) - str, back)
    }

    // test each of the positions
    1 to 5 foreach --
  }

  def testArraysRemove: Unit = {
    import setHolder._

    def -=-(in: Int) {
      val str = in.toString
      val back = withThirteenArray - str
      assertSet("and removed "+str, nSet(13) - str, back)
    }

    // test each of the positions
    1 to 13 foreach -=-
  }

  def testArraysRemoveNonExisting: Unit = {
    import setHolder._

    def rem(in: Int) {
      val str = in.toString
      val back = withThirteenArray - str
      assertTrue("Should be the original", back eq withThirteenArray)
    }

    // test each of the positions
    rem(14)
  }

  def testArraysExistingAdd: Unit = {
    import setHolder._
    // each of the positions with an add that is existing
    def +=+(in: Int) {
      val str = in.toString
      val back = withThirteenArray + str
      assertSet("and add existing "+str, nSet(13) + str, back)
    }
    
    1 to 13 foreach +=+
  }

  def testArraysAdd: Unit = {
    import setHolder._
    // each of the positions with an add that is existing
    def +=+(in: Int) {
      val str = in.toString
      val back = withThirteenArray + str
      assertSet("and add existing "+str, nSet(13) + str, back)
    }
    
    14 to 20 foreach +=+
  }

  def testArraysBulk: Unit = {
    import setHolder._

    // add a group
    val abunch = List("15","16","17","18","19")
    assertSet("Add a bunch", nSet(13) ++ abunch, withThirteenArray ++ abunch)

    // remove a group
    val remove = List("1","2","8","10")
    assertSet("Remove a bunch", nSet(13) -- abunch, withThirteenArray -- abunch)
  }

  def testArraysGets: Unit = {
    import setHolder._

    // gets
    def =?=(in: Int) {
      import StringS.equal
      implicit val streq = new scales.utils.Equiv[String]

      val str = in.toString
      val found = withThirteenArray( str )
      assertTrue("nothing was found "+str, found.isDefined)
      assertEquals("did not find "+str, str, found.get)
    }

    1 to 13 foreach =?=
  }

  def testUpToFiveRemoves: Unit = {
    import setHolder._

    val onem = withOne - "1"
    assertTrue("one - 1", onem.isInstanceOf[EmptyArraySet[String]])

    def -=-(start: ArraySet[String], clazz: Class[_])(in: Int) {
      val str = in.toString
      val startS = nSet(start.size)
      val back = start - str
      assertSet("and removed "+start.size+" - "+str, startS - str, back)
      assertTrue("removed has wrong type "+start.size+" expected "+clazz.getName, clazz.isInstance(back))
    }

    1 to 2 foreach (-=-(withTwo, classOf[ArraySetOne[String]]))
    1 to 3 foreach (-=-(withThree, classOf[ArraySetTwo[String]]))
    1 to 4 foreach (-=-(withFour, classOf[ArraySetThree[String]]))
    1 to 5 foreach (-=-(withFive, classOf[ArraySetFour[String]]))
  }

  def testUpToFiveApplys: Unit = {
    import setHolder._

    // gets
    def =?=(start: ArraySet[String])(in: Int) {
      import StringS.equal
      implicit val streq = new scales.utils.Equiv[String]

      val str = in.toString
      val found = start( str )
      assertTrue("nothing was found "+str, found.isDefined)
      assertEquals("did not find "+start.size+" - "+str, str, found.get)
    }

    1 to 2 foreach (=?=(withTwo))
    1 to 3 foreach (=?=(withThree))
    1 to 4 foreach (=?=(withFour))
    1 to 5 foreach (=?=(withFive))
  }

  def testUpToFiveApplyIntEquiv: Unit = {
    import setHolder._

    implicit val intEq = scalaz.Equal.equal { (a: Int, b: Int) => a == b }
    implicit val intEquiv = new scales.utils.Equiv[Int]
    implicit val intToStr = (i: Int) => i.toString
    implicit val aToInt = (i: A) => i.toInt
    
    // should not finds
    assertFalse("Should not find empty", emptyIS(40).isDefined)
    assertFalse("Should not find two", withTwo(40).isDefined)
    assertFalse("Should not find three", withThree(40).isDefined)
    assertFalse("Should not find four", withFour(40).isDefined)
    assertFalse("Should not find five", withFive(40).isDefined)
    // sanity
    assertTrue("Should not find five", withFive(4).isDefined)

  }

  def testUpToFiveRemoveNonExisting: Unit = {
    import setHolder._

    val emptym = emptyIS - "1"
    assertTrue("empty - 1 is empty", emptym eq emptyIS)

    def -=-(start: ArraySet[String]) {
      val str = "202"
      val back = start - str
      assertTrue("back should be eq to start "+str, back eq start)
    }

    -=-(withTwo)
    -=-(withThree)
    -=-(withFour)
    -=-(withFive)
  }

  def testUpToFiveAdds: Unit = {
    import setHolder._

    val onem = withOne - "1"
    assertTrue("one - 1", onem.isInstanceOf[EmptyArraySet[String]])

    def +=+(start: ArraySet[String], clazz: Class[_])(in: Int) {
      val str = in.toString
      val startS = nSet(start.size)
      val back = start + str
      assertSet("and add "+start.size+" - "+str, startS + str, back)
      assertTrue("added has wrong type "+start.size+" expected "+clazz.getName, clazz.isInstance(back))
    }

    1 to 2 foreach (+=+(emptyIS, classOf[ArraySetOne[String]]))
    2 to 3 foreach (+=+(withOne, classOf[ArraySetTwo[String]]))
    3 to 4 foreach (+=+(withTwo, classOf[ArraySetThree[String]]))
    4 to 6 foreach (+=+(withThree, classOf[ArraySetFour[String]]))
    5 to 7 foreach (+=+(withFour, classOf[ArraySetFive[String]]))
    6 to 8 foreach (+=+(withFive, classOf[ArraySetArray[String]]))
  }

  
}
