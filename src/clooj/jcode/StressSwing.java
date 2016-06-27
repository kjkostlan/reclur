// Reference to how fast the performance can be. If we are similar there is little benifit in performance optimizations.
package jcode;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JButton;

public class StressSwing {

  public static void test(int n) {
     JFrame f = new JFrame();
     JPanel p = new JPanel();
     
     for (int i=0; i<n; i++) {
         JButton b = new JButton("Java stress test #" + i);
         p.add(b);  
     }
     f.add(p);
     f.pack();
     f.setVisible(true);
  }

}