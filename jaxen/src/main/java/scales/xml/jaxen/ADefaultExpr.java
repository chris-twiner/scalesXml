package scales.xml.jaxen;

import org.jaxen.expr.Expr;
import org.jaxen.util.SingleObjectIterator;
import org.jaxen.util.SingletonList;

import java.util.Iterator;
import java.util.List;

// made innaccessible in later jaxens
abstract class ADefaultExpr implements Expr {
    ADefaultExpr() {
    }

    public Expr simplify() {
        return this;
    }

    public static Iterator convertToIterator(Object obj) {
        if (obj instanceof Iterator) {
            return (Iterator)obj;
        } else {
            return (Iterator)(obj instanceof List ? ((List)obj).iterator() : new SingleObjectIterator(obj));
        }
    }

    public static List convertToList(Object obj) {
        return (List)(obj instanceof List ? (List)obj : new SingletonList(obj));
    }
}