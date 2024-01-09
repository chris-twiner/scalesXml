package scales.xml

import scalaz.EphemeralStream.emptyEphemeralStream
import scalaz.Free.Trampoline
import scalaz.Id.Id
import scalaz.Monad
import scalaz.effect.IO
import scalaz.iteratee.{EnumeratorT, IterateeT, StepT}
import scalaz.iteratee.Input.{Eof, elInput}
import scalaz.iteratee.Iteratee.{done, elInput, enumEofT, enumIndexedSeq, enumIterator, foldM, head, iterateeT => siteratee}
import scalaz.iteratee.StepT.{Cont, Done}
import scales.utils.IterateeTests.{enumEphemeralStreamF, enumWeakStreamF, maxIterations}
import scales.utils.StreamHelpers.iTo
import scales.xml.impl.NoVersionXmlReaderFactoryPool
import scales.xml.serializers.XmlOutput

import scala.annotation.tailrec
class PullTest extends junit.framework.TestCase {

  import junit.framework.Assert._
  import java.io._

  import scales.utils._
  import io._
  import ScalesUtils._
  import ScalesXml._

  import resources._

  val Default = Namespace("urn:default")
  val DefaultRoot = Default("Default")

  import scalaz.EphemeralStream
  import scalaz.iteratee.{Iteratee, Enumerator, Input}
  
  import scales.utils.{resource => sresource}

  def testSimpleLoad = {
    val pull = pullXml(sresource(this, "/data/BaseXmlTest.xml"))
    assertTrue("didn't have any events",pull.hasNext)
    assertFalse("should not have been closed", pull.isClosed)
    var next = pull.next
    assertTrue("should have been left", next.isLeft)
    
    next.left.get match {
      case Elem(DefaultRoot,_,_) => () // is ok 
      case a @ _ => fail("Should have been {urn:default}Default was "+a)
    }

    pull.close
    assertTrue("Should have been closed", pull.isClosed)
  }

  def testResourceLoad = {
    val (resource, pull) = pullXmlResource(sresource(this, "/data/BaseXmlTest.xml"))
    assertTrue("didn't have any events",pull.hasNext)
    assertFalse("should not have been closed", resource.isClosed)
    var next = pull.next
    assertTrue("should have been left", next.isLeft)

    next.left.get match {
      case Elem(DefaultRoot,_,_) => () // is ok
      case a @ _ => fail("Should have been {urn:default}Default was "+a)
    }

    resource.closeResource
    assertTrue("Should have been closed", resource.isClosed)
  }

  def testProxiedCloser = {
    val strr = new java.io.StringReader("My String")
    val proxied = new ProxiedCloseOnNeedReader(strr)

    proxied.close() // should be a noop
    assertFalse("Was closed for some reason", proxied.isClosed)

    var arr = Array[Char](10)

    val count = proxied.read(arr)
    assertEquals("My String".substring(0,count),arr.mkString(""))

    proxied.closeResource
    assertTrue("Should have been closed", proxied.isClosed)
  }

  def testChainedCloseOnNeed = {
    import java.io.StringReader
    def getOne = new StringReader("My String") with CloseOnNeed {
      def doClose() {
	super[StringReader].close()
      }
    }
    val str1 = getOne
    val str2 = getOne
    val str3 = getOne

    val joined = str1 ++ str2 ++ str3
    assertFalse("1 Should not have been closed", str1.isClosed)
    assertFalse("2 Should not have been closed", str2.isClosed)
    assertFalse("3 Should not have been closed", str3.isClosed)
    assertFalse("Joined Should not have been closed", joined.isClosed)

    joined.closeResource

    assertTrue("1 Should have been closed", str1.isClosed)
    assertTrue("2 Should have been closed", str2.isClosed)
    assertTrue("3 Should have been closed", str3.isClosed)
    assertTrue("Joined Should have been closed", joined.isClosed)
  }

  def testDump = {
    val pull = pullXml(sresource(this, "/data/BaseXmlTest.xml"))

    def out(it : String) : Unit =
      ()//it

    for{event <- pull}{
      event match {
        case Left(x) => x match {
          case Elem(qname, attrs, ns) =>
            out("<" + qname + attrs.map( x => " "+x.name +"='"+x.value+"'" ).mkString(" ") + ">")

          case item : XmlItem =>
            out("item "+item)
        }
        case Right(EndElem(qname, ns)) =>
          out("</"+ qname +">")

      }
    }

    assertTrue("Should have been closed", pull.isClosed)
  }

  def testDropWhileIterateeOwnEnum = {
    val pull = pullXml(sresource(this, "/data/BaseXmlTest.xml"))

    val isShouldRedeclare = (x : PullType) => x match {
      case Left(Elem(qname, _, _) ) if qname.local == "ShouldRedeclare" =>
        false
      case _ => true
    }
    val isEndDefault = (x : PullType) => x match {
      case Right(EndElem(qname, _) ) if qname.local == "Default" =>
        false
      case _ => true
    }
    val iteratee = dropWhile[PullType, Trampoline]( isShouldRedeclare )

    def sanityCheck(sanity: Option[PullType]) = {

      assertTrue("Should have been some ShouldDeclare end", sanity.isDefined)
      assertTrue("Should have been an Element end "+sanity.get, sanity.get.isRight)
      assertTrue("Should have been a ShouldDeclare end", sanity.get.right.get.name.local == "ShouldRedeclare")
    }

    val iteratee2 = dropWhile[PullType, Trampoline]( isEndDefault )//(pull).run

    val p =
      for {
        res <- (iteratee &= iteratorEnumerator(pull.it)) run

        _ = assertTrue("Should have been some", res.isDefined)
        //println(res.get)
        _ = assertTrue("Should have been a ShouldRedeclare", !isShouldRedeclare(res.get))

        // sanity check, we should now have an endelem
        sanity <- (head[PullType, Trampoline] &= iteratorEnumerator(pull.it)) run

        _ = sanityCheck(sanity)
        // check again, only works if we cache the values, which is possibly bad for memory usage
        _ = sanityCheck(sanity)

        res2 <- (iteratee2 &= iteratorEnumerator(pull.it)) run
      } yield {
        assertTrue("Should have been some - Default end", res2.isDefined)
        assertTrue("Should have been a Default end", !isEndDefault(res2.get))

        assertFalse("should have no next", pull.hasNext)
        assertTrue("Should have been closed", pull.isClosed)
      }

    p run
  }

  /**
   * this test relies on the default memory of the given system, no way to test from within.
   * It also tests from outside in through the stax interface itself, hence the rather unpleasent acrobatics below.
   */
  def testConstantSpace = {

    //5 events each chunk
    val out = "<root><sub><dummy></dummy></sub>"

    val reader = new java.io.PushbackReader( new java.io.StringReader(out), 4000 )

    val itr = evalWith[Int, Trampoline, Int]( (x: Int) => {
      if ((x % 5 == 0) && x < maxIterations) {
        val xml = "<sub><child>"+x+"</child></sub>"

        reader.unread(xml.toCharArray())
      }
      if (x == maxIterations) {
        // get the rest of the reader and add the root to it
        var res = 0;
        val read = new java.lang.StringBuilder
        try{
        while( res != -1 ) {
          res = reader.read
          if (res != -1)
            read.append(res)
        }
        } catch {
          case _ : Throwable => println(read.toString)
        }

        reader.unread(read.append("</root>").toString.toCharArray());
      }
      x
    })

    val count = (1 to maxIterations).iterator

    val isEndRoot = (x: PullType) => {

      // pump more events
      val t = (itr &= iteratorEnumerator(count)).eval
      Monad[Trampoline].map(t.value) { _ =>
        x match {
          case Right(EndElem(qname, _)) if qname.local == "root" =>
            false
          case _ =>
            true
        }
      }
    }

    val p =
      for {
        _ <- (itr &= iteratorEnumerator(count)) run

        pull = pullXml(reader)

        iteratee2 = dropWhileM[PullType, Trampoline](isEndRoot)

        res2 <- (iteratee2 &= iteratorEnumerator[PullType, Trampoline](pull.it)) run

        res2End <- isEndRoot(res2.get)
      } yield {
        assertTrue("Should have been some - root end", res2.isDefined)

        assertTrue("Should have been a root end", !res2End)

        assertFalse("should have no next", pull.hasNext)
        assertTrue("Should have been closed", pull.isClosed)

      }

    p run
  }

  /*

There are three main use cases to do with how the pull logic should combine.

<root><many>... <deep><deeper><deepest></deepest></deeper></deep>
<others><of><interest>...
...</many></root>

1) Only elements matching a given path should be taken:

ie. take only deepests that are below <deep><deeper>

2) Take elements matching a path but don't exhaust them

same example above but after processing all deepests we want the <interest>s as well

3) Take alternating results

Here there are repetitions of <deep>..<others><deep><others...

and we want to always match them correctly

The barrage of tests below test the resumable IterV version, onDone resumable and then finally a chunk of xml tests
on both the qname matching (3 of them) and then the above combos

   */

/*
  /**
   * enumIndexedSeq doesn't return EOF
   * @param a
   * @param min
   * @param max
   * @tparam E
   * @tparam F
   * @return
   */

  def enumIndexedSeq2[E, F[_]: Monad](a : IndexedSeq[E], min: Int = 0, max: Option[Int] = None) : EnumeratorT[E, F] =
    new EnumeratorT[E, F] {
      private[this] val limit = max.map(_ min (a.length)).getOrElse(a.length)
      def apply[A] = {
        def loop(pos : Int): StepT[E, F, A] => IterateeT[E, F, A] = {
          s =>
            s(done = (x,y) => {
              val isEof = y.isEof
              val isEL = y.isEl
              val isEmpty = y.isEmpty
              if (limit == pos)
                done(x, Eof[E])
              else
                s.pointI
            }
              ,
            cont =
              k =>
                if (limit > pos)
                  k(scalaz.iteratee.Iteratee.elInput(a(pos))) >>== loop(pos + 1)
                else
                  s.pointI
            )
        }
        loop(min)
      }
    } */

  def childEvents(i : Int, max : Int) : WeakStream[PullType] =
    childEvents(i, max, childEvents(i + 1, max))

  def childEvents(i : Int, max : Int, next : => WeakStream[PullType]) : WeakStream[PullType] = {

    def ichildEvents(i : Int, max : Int) : WeakStream[PullType] =
    if (i < max) {
       WeakStream[PullType](
        Elem("unloved"l),
          Text("unloved content"),
        EndElem("unloved"l),
        Elem("child"l),
          Elem("interesting"l),
            Text("interesting content "+i),
          EndElem("interesting"l),
          Elem("interesting"l),
	    Text("interesting "),
            Elem("interesting"l),
              Text("content "+(i + 1)),
	    EndElem("interesting"l),
          EndElem("interesting"l),
        EndElem("child"l)
      ) ++ next
    }
    else WeakStream.empty[PullType]

    ichildEvents(i, max)
  }
  def events(children : => WeakStream[PullType]) : WeakStream[PullType] =
    WeakStream[PullType](Left(Elem("root"l))) ++ (children) ++ (WeakStream[PullType](Right(EndElem("root"l))))

  def events(max : Int) : WeakStream[PullType] =
    events(childEvents(1 ,max))

  def echildEvents(i : Int, max : Int) : EphemeralStream[PullType] =
    echildEvents(i, max, echildEvents(i + 1, max))

  def echildEvents(i : Int, max : Int, next : => EphemeralStream[PullType]) : EphemeralStream[PullType] = {

    def ichildEvents(i : Int, max : Int) : EphemeralStream[PullType] =
    if (i < max) {
      EphemeralStream[PullType](
        Left(Elem("unloved"l)),
          Left(Text("unloved content")),
        Right(EndElem("unloved"l)),
        Left(Elem("child"l)),
          Left(Elem("interesting"l)),
            Left(Text("interesting content "+i)),
          Right(EndElem("interesting"l)),
          Left(Elem("interesting"l)),
	    Left(Text("interesting ")),
            Left(Elem("interesting"l)),
              Left(Text("content "+(i + 1))),
	    Right(EndElem("interesting"l)),
          Right(EndElem("interesting"l)),
        Right(EndElem("child"l))
      ) +:+ next
    }
    else EphemeralStream[PullType]()

    ichildEvents(i, max)
  }
  def eevents(children : => EphemeralStream[PullType]) : EphemeralStream[PullType] =
    EphemeralStream[PullType](Left(Elem("root"l))) ++ (children) ++ (EphemeralStream[PullType](Right(EndElem("root"l))))

  def eevents(max : Int) : EphemeralStream[PullType] =
    eevents(echildEvents(1 ,max))

/* Stream blows the stack */

  def schildEvents(i : Int, max : Int) : Stream[PullType] =
    schildEvents(i, max, schildEvents(i + 1, max))

  def schildEvents(i : Int, max : Int, next : => Stream[PullType]) : Stream[PullType] = {

    def ichildEvents(i : Int, max : Int) : Stream[PullType] =
    if (i < max) {
      Stream[PullType](
        Left(Elem("unloved"l)),
          Left(Text("unloved content")),
        Right(EndElem("unloved"l)),
        Left(Elem("child"l)),
          Left(Elem("interesting"l)),
            Left(Text("interesting content "+i)),
          Right(EndElem("interesting"l)),
          Left(Elem("interesting"l)),
	    Left(Text("interesting ")),
            Left(Elem("interesting"l)),
              Left(Text("content "+(i + 1))),
	    Right(EndElem("interesting"l)),
          Right(EndElem("interesting"l)),
        Right(EndElem("child"l))
      ) ++ next
    }
    else Stream[PullType]()

    ichildEvents(i, max)
  }
  def sevents(children : => Stream[PullType]) : Stream[PullType] =
    Stream[PullType](Left(Elem("root"l))) ++ (children) ++ (Stream[PullType](Right(EndElem("root"l))))

  def sevents(max : Int) : Stream[PullType] =
    sevents(schildEvents(1 ,max))




  def extraChildEvents(i : Int, max : Int, next : => WeakStream[PullType]) : WeakStream[PullType] =
    if (i < max)
      WeakStream[PullType](
        Elem("anotherChild"l),
          Elem("stillInteresting"l),
            Text("interesting content "+i),
          EndElem("stillInteresting"l),
        EndElem("anotherChild"l)
      ) ++ next
    else WeakStream.empty[PullType]

  def testOnQNamesLastElement():Unit = {
    // ourMax of 9000000 and WeakStream processess in 512s without issue
    // It truly seems something is different here, with the others
    // we won't get past iterator...
    val ourMax = maxIterations / 10 // full takes too long but does work in constant space

    type TheF[X] = Trampoline[X]

    var at = -1

    try {

      var iter = eevents(ourMax)
      //var iter = WeakStream.iterTo(EphemeralStream.toIterable(eevents(ourMax)).iterator)
      val func = (e: EphemeralStream[PullType]) => {iter = e}

      def enum(e: EphemeralStream[PullType]) = enumEphemeralStreamF[PullType, TheF](func)(e)

      at = 0
      val QNames = List("root" l, "child" l, "interesting" l)

      val ionDone = onDone[PullType, TheF, QNamesMatch](List(onQNames(QNames)))

      def isDone[F[_]](i: Int, res: ResumableIterList[PullType, F, QNamesMatch])(implicit F: Monad[F]) =
        F.map(res.value) { step =>
          step(
            done = (x, y) => (x, y) match {
              case (((QNames, Some(x)) :: Nil, cont), y) =>
                assertEquals("interesting content " + i, text(x))
                assertEquals(1, x.zipUp.children.size)
                assertTrue("should have been Empty " + i, y.isEmpty)
              case ((list, cont), y) =>
                fail("was done with " + i + " " + list + " and input " + y + " iter isEmpty == " + iter.isEmpty)

            },
            cont = _ => fail("was not done " + i + " was " + res)
          )
        }
/* Id works, shows the functionality is correct
      val starter = (ionDone &= iteratorEnumerator(iter)).eval
      isDone(1, starter)

      val res = (extractCont(starter) &= iteratorEnumerator(iter)).eval
      isDone(1 + 1, res)
      val r = (foldM[Int, TheF, ResumableIterList[PullType, TheF, QNamesMatch]](res) { (itr, i) =>

            val n1 = (extractCont(itr) &= iteratorEnumerator(iter)).eval
            Monad[TheF].bind(
              Monad[TheF].map(n1.value){
                _ =>
                  isDone(i, n1)
                  n1
              }
            ){ n1 =>
              val n2 = (extractCont(n1) &= iteratorEnumerator(iter)).eval
              Monad[TheF].map(n1.value){
                _ =>
                  isDone(i + 1, n2)
                  at += 1
                  n2
              }
            }
          } &= iteratorEnumerator((2 to (ourMax - 1)).iterator)) run

      val lastres = (extractCont(r) &= iteratorEnumerator(iter)).eval
      val step = lastres.value
      step(
        done = (x,y) => (x,y) match {
          case ((Nil,cont), y) =>
            assertTrue("should have been EOL", y.isEof)
        },
        cont = _ => fail("was not done with empty")
      )
*/
      /* trampoline does not */
      val starter = (ionDone &= enum(iter)).eval
      val p =
        for {
          _ <-  isDone(1, starter)

          res = (extractCont(starter) &= enum(iter)).eval
          _ <- isDone(1 + 1, res)
          // check it does not blow up the stack and/or mem
          _ = { at = 2 }

          r <- (foldM[Int, TheF, ResumableIterList[PullType, TheF, QNamesMatch]](res) { (itr, i) =>

            val n1 = (extractCont(itr) &= enum(iter)).eval
            Monad[TheF].bind(
              Monad[TheF].map(n1.value){
                _ =>
                  isDone(i, n1)
                  n1
              }
            ){ n1 =>
              val n2 = (extractCont(n1) &= enum(iter)).eval
              Monad[TheF].map(n1.value){
                _ =>
                  isDone(i + 1, n2)
                  at += 1
                  n2
              }
            }
          } &= iteratorEnumerator((2 to (ourMax - 1)).iterator)) run

          lastres = (extractCont(r) &= enum(iter)).eval
          step <- lastres.value
        } yield {
          step(
            done = (x,y) => (x,y) match {
              case ((list,cont), y) =>
                val l = list
                assertTrue("should have been EOF", y.isEof)
            },
            cont = _ => fail("was not done with empty")
          )
        }

      p run
    } catch {
      case e : StackOverflowError => println("got to " + at)
    }
  }

  def testOnQNamesManyElementsBelow = {

    val ourMax = maxIterations / 10 // full takes too long but does work in constant space

    var iter = events(ourMax)
    val func = (e: WeakStream[PullType]) => {iter = e}

    def enum(e: WeakStream[PullType]) = enumWeakStreamF[PullType, Trampoline](func)(e)

    val QNames = List("root"l, "child"l)
    val ionDone = onDone(List(onQNames[Trampoline](QNames)))

    def isDone( i : Int, res : ResumableIterList[PullType, Trampoline, QNamesMatch]) =
      Monad[Trampoline].map(res.value){_( done = (x,y) => (x,y) match {
        case (((QNames, Some(x)) :: Nil,cont), y)  =>
          // we want to see both sub text nodes
          assertEquals( "interesting content "+ i +"interesting content "+ (i + 1)
                 , text(x))
          val count = x.zipUp.children.size
          if (count != 1){
            x.zipUp.children.foreach{x => println(x);println()}
            printTree(rootPath(x).tree)
            fail("more than one " + count +" at "+ println(elem(x)))
          }
          assertTrue("should have been Empty "+i, y.isEmpty)
        case ((list, cont), y) =>
          fail("was done with "+ i+" "+list+" and input "+ y +" iter empty == "+iter.empty)

      }, cont = _ => fail("was not done "+i+" was " + res ))}

    val starter = (ionDone &= enum(iter)).eval

    val p =
      for {
        _ <- isDone(1, starter)
        // check it does not blow up the stack and/or mem

        r <- (foldM[Int, Trampoline, ResumableIterList[PullType, Trampoline, QNamesMatch]](starter){ (itr, i) =>
            val res = (extractCont(itr) &= enum (iter)).eval
            Monad[Trampoline].bind(res.value) {
              _ =>
                val rc = isDone(i, res)
                Monad[Trampoline].map(rc) { _ =>
                  res
                }
            }
          } &= iteratorEnumerator((2 to (ourMax - 1)).iterator)).run

        last = (extractCont(r) &= enum(iter)).eval
        step <- last.value
      } yield {
        step(done = (x,y) => (x,y) match {
          case ((Nil,cont), y)  =>
            assertTrue("should have been EOF", y.isEof)

        }, cont = _ => fail("was not done with empty"))
      }

    p run
  }

  def testOnQNamesRepeatedQNames(): Unit = {

    val ourMax = maxIterations / 10 // full takes too long but does work in constant space


    var iter = events(ourMax)
    val func = (e: WeakStream[PullType]) => {iter = e}

    def enum(e: WeakStream[PullType]) = enumWeakStreamF[PullType, Trampoline](func)(e)
//    val iter = events(ourMax).iterator

    val ionDone = onDone(List(onQNames[Trampoline](repeatingQNames)))

    def isDone( i : Int, res : ResumableIterList[PullType,Trampoline,QNamesMatch]) =
      Monad[Trampoline].map(res.value){_( done = (x,y) => (x,y) match {
        case (((repeatingQNames, Some(x)) :: Nil,cont), y)  =>
          // we want to see both sub text nodes
          assertEquals( "content "+ (i + 1)
                 , text(x))
          assertEquals(1, x.zipUp.children.size)
          assertTrue("should have been Empty "+i, y.isEmpty)
        case ((list, cont), y) =>
          fail("was done with "+ i+" "+list+" and input "+ y +" iter isEmpty == "+iter.isEmpty)
      },  cont =  _ => fail("was not done "+i+" was " + res ))}

    val starter = (ionDone &= enum(iter)).eval
    val p =
      for {
        _ <- isDone(1, starter)
        // check it does not blow up the stack and/or mem

        r <- (foldM[Int, Trampoline, ResumableIterList[PullType, Trampoline, QNamesMatch]](starter) { (itr, i) =>
          val res = (extractCont(itr) &= enum (iter)).eval
          Monad[Trampoline].bind(res.value) {
            _ =>
              val rc = isDone(i, res)
              Monad[Trampoline].map(rc) { _ =>
                res
              }
          }
        } &= iteratorEnumerator((2 to (ourMax - 1)).iterator) ).run

        last = (extractCont(r) &= enum(iter)).eval
        step <- last.value
      } yield {
        step(done = (x, y) => (x, y) match {
          case ((Nil, cont), y) =>
            assertTrue("should have been EOf", y.isEof)
        }, cont = _ => fail("was not done with empty"))
      }

    p run
  }

  def testAlternating(): Unit = {

    val ourMax = maxIterations / 10 // full takes too long but does work in constant space

    def alternate( i : Int, max : Int) : WeakStream[PullType] =
      extraChildEvents(i, ourMax, childEvents(i, ourMax,
		       alternate(i+1, max) ))

    def alternating = events( alternate(1, ourMax) )

    //alternating.foreach(println)

    var iter = alternating
    val func = (e: WeakStream[PullType]) => {iter = e}

    def enum(e: WeakStream[PullType]) = enumWeakStreamF[PullType, Trampoline](func)(e)

    var starter = (altOnDone &= enum(iter)).eval

    val p =
      for {
        _ <- isInteresting(1, starter)
        res = (extractCont(starter) &= enum(iter)).eval
        _ <- isContent(2, res)
        // check it does not blow up the stack and/or mem

        r <- (foldM[Int, Trampoline, ResumableIterList[PullType, Trampoline, QNamesMatch]](res){ (itr,i) =>
          val res = (extractCont(itr) &= enum(iter)).eval
          for {
            _ <- isInteresting(i, res)
            res2 = (extractCont(res) &= enum(iter)).eval
            _ <- isContent(i+1, res2)
          } yield {
            res2
          }
        } &= iteratorEnumerator((2 to (ourMax - 1)).iterator)).run

        last = (extractCont(r) &= enum(iter)).eval
        step <- last.value
      } yield {
        step(done = (x, y) => (x, y) match {
          case ((Nil, cont), y) =>
            assertTrue("should have been EOF", y.isEof)

        }, cont = _ => fail("was not done with empty"))
      }

    p run
  }

  val repeatingQNames = List("root"l, "child"l, "interesting"l, "interesting"l)
  val stillInterestingQNames = List( "root"l, "anotherChild"l, "stillInteresting"l )

  val altOnDone = onDone(List(onQNames[Trampoline](repeatingQNames),
			      onQNames[Trampoline](stillInterestingQNames)))

  val (isInteresting, isContent) = {

    def isDone(content : String, QNames : List[QName])( i : Int, res : ResumableIterList[PullType, Trampoline, QNamesMatch]) =
      Monad[Trampoline].map(res.value){_( done = (x,y) => (x,y) match {
        case (((QNames, Some(x)) :: Nil,cont), y)  =>
          // we want to see both sub text nodes
          assertEquals( content+" "+ i
                 , text(x))
          assertEquals(1, x.zipUp.children.size)
          assertTrue("should have been Empty "+i, y.isEmpty)
        case ((list, cont), y) =>
          fail("was "+content+" done with "+ i+" "+list+" and input "+ y)

      }, cont = _ => fail("was not "+content+" done "+i+" was " + res ))}

    (isDone("interesting content", stillInterestingQNames) _,
      isDone("content", repeatingQNames) _)
  }

  def testFirstThenNext():Unit = {

    val ourMax = maxIterations / 10 // full takes too long but does work in constant space

    def extraChildren( i : Int, max : Int) : WeakStream[PullType] =
      extraChildEvents( i, max, extraChildren( i+1 , max ))

    def alternating = events( childEvents(1, ourMax/2) ++ (
      extraChildren(1, ourMax/2) ))

    //alternating.foreach(println)

    var iter = alternating
    val func = (e: WeakStream[PullType]) => {iter = e}

    def enum(e: WeakStream[PullType]) = enumWeakStreamF[PullType, Trampoline](func)(e)

    val starter = (altOnDone &= enum(iter)).eval
    val p =
      for {
        _ <- isContent(2, starter)
        // check it does not blow up the stack and/or mem

        content <- (foldM[Int, Trampoline, ResumableIterList[PullType, Trampoline, QNamesMatch]](starter) { (itr, i) =>
          val res = (extractCont(itr) &= enum(iter)).eval
          for {
            _ <- isContent(i + 1, res)
          } yield res
        } &= iteratorEnumerator((2 to (ourMax/2 - 1)).iterator)).run

        interesting <- (foldM[Int, Trampoline, ResumableIterList[PullType, Trampoline, QNamesMatch]](content) { (itr, i) =>
          val res = (extractCont(itr) &= enum(iter)).eval
          for {
            _ <- isInteresting(i, res)
          } yield res
        } &= iteratorEnumerator((1 to (ourMax/2 - 1)).iterator)).run

        last = (extractCont(interesting) &= enum(iter)).eval
        step <- last.value

      } yield {
        step(done = (x, y) => (x, y) match {
          case ((Nil, cont), y) =>
            assertTrue("should have been EOF", y.isEof)
        }, cont = _ => fail("was not done with empty"))
      }

    p run
  }


  def ofInterestEvents(i : Int, max : Int) : WeakStream[PullType] = {
    def iOfInterestEvents( i : Int, max :Int) : WeakStream[PullType] =
    if (i < max)
      WeakStream[PullType](
          Left(Elem("ofInterest"l)),
	    Left(Elem("value"l)),
              Left(Text((i + 1).toString)),
	    Right(EndElem("value"l)),
          Right(EndElem("ofInterest"l))
      ) ++ iOfInterestEvents(i + 1, max)
    else WeakStream.empty[PullType]

    iOfInterestEvents(i, max)
  }

  def sectionEvents(i : Int, max : Int, next : => WeakStream[PullType]) : WeakStream[PullType] =
    if (i < max)
      WeakStream[PullType](
        Left(Elem("section"l)),
          Left(Elem("sectionHeader"l)),
            Left(Text(i.toString)),
          Right(EndElem("sectionHeader"l))
      ) ++ (ofInterestEvents(0, i)) ++ (WeakStream[PullType](Right(EndElem("section"l)))) ++ ( next )
    else WeakStream.empty[PullType]

  def sectionEvents(i : Int, max : Int) : WeakStream[PullType] =
    sectionEvents(i, max, sectionEvents(i + 1,max))

  def withHeaders(max : Int) : WeakStream[PullType] =
    events(sectionEvents(1 ,max))

  val Headers = List("root"l,"section"l,"sectionHeader"l)
  val OfInterest = List("root"l,"section"l,"ofInterest"l)

  val ofInterestOnDone = onDone(List(onQNames[Trampoline](Headers),
    onQNames[Trampoline](OfInterest)))


  def testFoldOnDone(): Unit = {

    val ionDone = ofInterestOnDone

    /*
     * this test is more than a little intensive, given we are
     * purposefully quadratic so it explodes memory creation
     * as such we aren't linked to maxIterations.
     *
     */
    val ourMax = 50

    var iter = withHeaders(ourMax)
    val func = (e: WeakStream[PullType]) => {iter = e}

    def enum(e: WeakStream[PullType]) = enumWeakStreamF[PullType, Trampoline](func)(e)

    val total = foldOnDone[PullType, Iterable[QNamesMatch], Trampoline, (Int,Int)](enum(iter))( (0, 0), ionDone ){
      (t, qnamesMatch) =>
        if (qnamesMatch.size == 0) {
          t // no matches
        } else {
          // only one at a time
          assertEquals(1, qnamesMatch.size)
          val head = qnamesMatch.head
          assertTrue("Should have been defined",head._2.isDefined)

          // we should never have more than one child in the parent
          // and thats us
          assertEquals(1, head._2.get.zipUp.children.size)
      /*	  val count = head._2.get.zipUp.children.size
          if (count != 1) {
            head._2.get.zipUp.children.foreach(println)
            fail("Had more children "+ count)
          }*/
          val i =  text(head._2.get).toInt
          if (head._1 eq Headers) {
            assertEquals(t._1, t._2)
            // get new section
            (i, 1)
          } else {
            (t._1, i)
          }
	      }
    }

    val p =
      for {
        pair <- total
      } yield {
        assertEquals(ourMax - 1, pair._1)
        assertEquals(pair._1, pair._2)
      }

    p run
  }
/*
  type FiveStrings = (String,String,String,String,String)

  def testIterator = {
    val pull = pullXml(sresource(this, "/data/svnLogIteratorEg.xml"))
    val LogEntries = List("log"l,"logentry"l)

    var i = 0

    val it = scales.xml.iterate(LogEntries, pull.it)
    val bits = for{ entry : XmlPath <- it
	revision <- entry.\.*@("revision"l).one
	author <- entry.\*("author"l).one
	path <- entry.\*("paths"l).|>{x=> i+=1;x}.\*("path"l)
	kind <- path.\.*@("kind"l)
	action <- path.\.*@("action"l)
    } yield (text(revision), value(author), text(kind), text(action), value(path))

    val t = bits.next//iterator.next
    //println( t._1+ " " + t._2 + " " + t._3 + " " + t._4 + " " + t._5 )
    assertEquals(1, i)
    assertEquals(expectedHead, t)

    // sanity check
    if (it.isTraversableAgain) assertTrue(!it.isEmpty)

    pull.close
  }

  val expectedHead = ("264","chris.twiner","dir","M","/trunk/scalesUtils")

  def testIteratorCombo = {
    val pull = pullXml(sresource(this, "/data/svnLogIteratorEg.xml"))
    val LogEntries = List("log"l,"logentry"l)
    val ionDone = onDone(List(onQNames(LogEntries)))

    val entries = foldOnDone(iteratorEnumerator(pull.it))( List[FiveStrings](), ionDone ){
      (t, qnamesMatch) =>
        if (qnamesMatch.size == 0) {
          t // no matches
        } else {
          val entry = qnamesMatch.head._2.get
          val bits = for{
             revision <- entry.\.*@("revision"l).one
             author <- entry.\*("author"l).one
             path <- entry.\*("paths"l).\*("path"l)
             kind <- path.\.*@("kind"l)
             action <- path.\.*@("action"l)
                 } yield (text(revision), value(author), text(kind), text(action), value(path))
          t ++ bits
        }
    }

    assertTrue("Pull was not closed",pull.isClosed)
    assertEquals(expectedHead, entries.head)
    assertEquals(("51","chris.twiner","dir","A","/trunk/scalesUtils"),entries.last)
    //entries.foreach(println)
  }

  def testSkipTop = {
    val iter = events(10).iterator

    var res = (skip(List())  &= iteratorEnumerator(iter)) run

    assertEquals("{}root", qualifiedName(res.get))

    res = (skip(List()) &= iteratorEnumerator(iter)) run

    assertEquals("{}unloved", qualifiedName(res.get))
  }

  def testSkipSoap = {
    val iter = events(10).iterator

    var res = (skip(List(2, 1)) &= iteratorEnumerator(iter)) run

    val path = res.get
    assertEquals("{}interesting", qualifiedName(path))
    assertEquals(1, path.zipUp.children.size)
    assertEquals(1, path.zipUp.zipUp.children.size)

  }

  def testSkipTooFar = {
    val iter = events(2).iterator

    val res = (skip(List(20, 1)) &= iteratorEnumerator(iter)) run

    assertTrue("Should not have found anything", res.isEmpty)
  }

  def testSkipNoMatch = {
    def iter = events(2).iterator

    var res = (skip(List(1, 20)) &= iteratorEnumerator(iter)) run

    assertTrue("Should not have found anything", res.isEmpty)

    res = (skipv(1, 1, 20) &= iteratorEnumerator(iter) )run

    assertTrue("Should not have found anything", res.isEmpty)
  }

  def testIteratorForStrictSelectivePulling() = {
    val expectedPlants = List(
      ("English ivy","Hedera helix", "3", "Mostly Shady", "$9.99", "000100"),
      ("Dwarf periwinkle","Vinca minor", "3", "Mostly Shady", "$12.10", "000409")
    )

    val pull = pullXml(
      sresource(this, "/data/nature.xml"),
      strictPath = List(
        NoNamespaceQName("CATALOG"),
        NoNamespaceQName("EUKARYOTE"),
        NoNamespaceQName("PLANT")
      )
    )
    val PlantEntries = List("CATALOG"l, "PLANT"l)

    val it = scales.xml.iterate(PlantEntries, pull.it)
    val plantsIt = for {
      entry: XmlPath <- it
      common <- entry.\*("COMMON"l).one
      botanical <- entry.\*("BOTANICAL"l).one
      zone <- entry.\*("ZONE"l).one
      light <- entry.\*("LIGHT"l).one
      price <- entry.\*("PRICE"l).one
      availability <- entry.\*("AVAILABILITY"l).one
    } yield (value(common), value(botanical), value(zone), value(light), value(price), value(availability))

    val plants = plantsIt.toList

    assertEquals(expectedPlants.size, plants.size)
    for ((plant, idx) <- plants.zipWithIndex)
      assertEquals(expectedPlants(idx), plant)

    // sanity check
    if (it.isTraversableAgain) assertTrue(it.nonEmpty)

    pull.close
  }
*/
}

