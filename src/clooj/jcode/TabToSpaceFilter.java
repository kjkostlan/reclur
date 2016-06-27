package jcode;
//http://javadots.blogspot.com/2010/02/swing-tabs-to-spaces.html
// A nice example.


import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;
import javax.swing.text.JTextComponent;

public class TabToSpaceFilter extends DocumentFilter {
    
    private String spaces = ""; // one tab = this string.
    private JTextComponent textComponent;
    
    public TabToSpaceFilter(int tabSize, JTextComponent tc) {
        textComponent = tc;
        for(int i = 0; i < tabSize; ++i)
            spaces += " ";
    }
    
    @Override
    public void insertString(FilterBypass fb, int offset, String text,
                             AttributeSet attr) throws BadLocationException {
        super.insertString(fb, offset, translate(offset, text), attr);
    }
    
    @Override
    public void replace(FilterBypass fb, int offset, int length,
                        String text, AttributeSet attr) throws BadLocationException {
        super.replace(fb, offset, length, translate(offset, text), attr);
    }
    
    private String translate(int offset, String s) {
        int col = columnOf(offset);
        
        StringBuilder sb = new StringBuilder();
        int top = s.length();
        for(int i = 0; i < top; ++i, ++col) {
            char c = s.charAt(i);
            if(c == '\t')
                sb.append(spaces.substring(col % spaces.length()));
            else
                sb.append(c);
        }
        
        return sb.toString();
    }
    
    private int columnOf(int i) {
        String s = textComponent.getText();
        if(i == 0)
            return 0;
        int prev = s.lastIndexOf("\n", i-1);
        if(prev < 0)
            return i;
        
        return (i-prev)-1;
    }
    
    public static void install(int tabSize, JTextComponent area) {
        AbstractDocument ad = (AbstractDocument) area.getDocument();
        ad.setDocumentFilter(new TabToSpaceFilter(tabSize, area));
    }
}