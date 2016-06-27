package jcode;
import java.util.EventObject;
public class EventRelay extends EventObject {
    Object clojureStuff;
    public void setClojure(Object x) {
        clojureStuff = x;
    }
    public Object getClojure() {
       return clojureStuff;
    }
    public EventRelay(Object x) {
       super(x);
    }
}