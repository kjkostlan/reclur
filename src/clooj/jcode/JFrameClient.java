// JFrame does not have client properties! We add that here with a simple extends class.
package jcode;
import javax.swing.JFrame;
import java.util.HashMap;
public class JFrameClient extends JFrame {
    HashMap<Object,Object> clientProps = new HashMap<Object, Object>();

    public Object getClientProperty(Object ky) {
        return clientProps.get(ky);
    }
    public void putClientProperty(Object ky, Object val) {
        clientProps.put(ky, val);
    } 
    public String toString() {
        return "JFrame";
    }
}
