package jcode;
import javax.swing.tree.TreePath;
public class Treej {
    // seems to be needed to get around a bug in clojure figuring out the type of args to call to java.
    public static TreePath fromArray(Object[] path) {
        return new TreePath(path);
    }

}