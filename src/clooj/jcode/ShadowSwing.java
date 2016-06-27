// Tests the effect of event listeners shadowing each-other.
// Both moved and resized set the location to 200 200.
// But does moved shadow resized?
// (do (import '[jcode ShadowSwing]) (import '[javax.swing SwingUtilities]) (SwingUtilities/invokeLater #(ShadowSwing/test)))
// Result: it is NOT shadowed so why are we shadowed?
package jcode;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JButton;
import java.awt.Point;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;

public class ShadowSwing extends JFrame implements ComponentListener {

  public static void test() {
     ShadowSwing f = new ShadowSwing();
     f.addComponentListener(f);
     f.pack();
     f.setVisible(true);
  }

  public void componentHidden(ComponentEvent e) {
      
  }
  public void componentMoved(ComponentEvent e) {
      this.setLocation(new Point(200,200));
  }
  public void componentResized(ComponentEvent e) {
      this.setLocation(new Point(200,200));
  }
  public void componentShown(ComponentEvent e) {
      
  }    


}