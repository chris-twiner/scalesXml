package scales.xml.jaxen;

import junit.framework.TestCase;

/**
 * I know this is bloody obvious but every now and then I see Java code that makes me doubt my sanity, and its usually when I don't have internet access.  As such, to verify the insanity of NodeComparator using == for document order, I made this.
 */
public class EqTest extends TestCase {
    class Testee {
	final public String myval;
	Testee(String in) {
	    myval = in;
	}
	public boolean equals(Object other) {
	    if (other instanceof Testee) {
		return ((Testee)other).myval.equals(myval);
	    } else {return false;}
	}
    }

    public void testEq(){
	Testee one1 = new Testee("one");
	Testee one2 = new Testee("one");
	assertFalse("Cannot be true man, are we secretly in Scala?", one1 == one2);
	assertTrue("Dis should never fail", one1.equals(one2));
    }
} 