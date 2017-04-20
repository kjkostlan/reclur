; Raw paste-ins from documentation.
(ns clooj.guinew.cutnpaste)

; Go to https://docs.oracle.com/javase/tutorial/uiswing/events/api.html,
; copy into open office, then copy here.
; remove "(extends MouseListener andMouseMotionListener".
; Replace ", " and "," with "".
; Add this line at the end: 
; "AdjustmentListener    none    adjustmentValueChanged(AdjustmentEvent)" 
; TODO: more events are missing.
; Add one pair of quotes.
(def listener-list-raw
"ActionListener none    actionPerformed(ActionEvent)
AncestorListener  none ancestorAdded(AncestorEvent)
ancestorMoved(AncestorEvent)
ancestorRemoved(AncestorEvent)
CaretListener  none caretUpdate(CaretEvent)
CellEditorListener  none editingStopped(ChangeEvent)
editingCanceled(ChangeEvent)
ChangeListener none    stateChanged(ChangeEvent)
ComponentListener    ComponentAdapter   componentHidden(ComponentEvent)
componentMoved(ComponentEvent)
componentResized(ComponentEvent)
componentShown(ComponentEvent)
ContainerListener  ContainerAdapter componentAdded(ContainerEvent)
componentRemoved(ContainerEvent)
DocumentListener    none   changedUpdate(DocumentEvent)
insertUpdate(DocumentEvent)
removeUpdate(DocumentEvent)
ExceptionListener    none   exceptionThrown(Exception)
FocusListener  FocusAdapter focusGained(FocusEvent)
focusLost(FocusEvent)
HierarchyBoundsListener   HierarchyBoundsAdapter    ancestorMoved(HierarchyEvent)
ancestorResized(HierarchyEvent)
HierarchyListener    none   hierarchyChanged(HierarchyEvent)
HyperlinkListener    none   hyperlinkUpdate(HyperlinkEvent)
InputMethodListener   none  caretPositionChanged(InputMethodEvent)
inputMethodTextChanged(InputMethodEvent)
InternalFrameListener    InternalFrameAdapter   internalFrameActivated(InternalFrameEvent)
internalFrameClosed(InternalFrameEvent)
internalFrameClosing(InternalFrameEvent)
internalFrameDeactivated(InternalFrameEvent)
internalFrameDeiconified(InternalFrameEvent)
internalFrameIconified(InternalFrameEvent)
internalFrameOpened(InternalFrameEvent)
ItemListener none    itemStateChanged(ItemEvent)
KeyListener    KeyAdapter keyPressed(KeyEvent)
keyReleased(KeyEvent)
keyTyped(KeyEvent)
ListDataListener  none contentsChanged(ListDataEvent)
intervalAdded(ListDataEvent)
intervalRemoved(ListDataEvent)
ListSelectionListener    none   valueChanged(ListSelectionEvent)
MenuDragMouseListener    none   menuDragMouseDragged(MenuDragMouseEvent)
menuDragMouseEntered(MenuDragMouseEvent)
menuDragMouseExited(MenuDragMouseEvent)
menuDragMouseReleased(MenuDragMouseEvent)
MenuKeyListener   none  menuKeyPressed(MenuKeyEvent)
menuKeyReleased(MenuKeyEvent)
menuKeyTyped(MenuKeyEvent)
MenuListener   none  menuCanceled(MenuEvent)
menuDeselected(MenuEvent)
menuSelected(MenuEvent)
MouseInputListener   MouseInputAdapter
MouseAdapter    mouseClicked(MouseEvent)
mouseEntered(MouseEvent)
mouseExited(MouseEvent)
mousePressed(MouseEvent)
mouseReleased(MouseEvent)
mouseDragged(MouseEvent)
mouseMoved(MouseEvent)
MouseAdapter(MouseEvent)
MouseListener    MouseAdapterMouseInputAdapter    mouseClicked(MouseEvent)
mouseEntered(MouseEvent)
mouseExited(MouseEvent)
mousePressed(MouseEvent)
mouseReleased(MouseEvent)
MouseMotionListener   MouseMotionAdapterMouseInputAdapter  mouseDragged(MouseEvent)
mouseMoved(MouseEvent)
MouseWheelListener   MouseAdapter  mouseWheelMoved(MouseWheelEvent)
MouseAdapter<MouseEvent>
PopupMenuListener  none popupMenuCanceled(PopupMenuEvent)
popupMenuWillBecomeInvisible(PopupMenuEvent)
popupMenuWillBecomeVisible(PopupMenuEvent)
PropertyChangeListener    none   propertyChange(PropertyChangeEvent)
TableColumnModelListener  none columnAdded(TableColumnModelEvent)
columnMoved(TableColumnModelEvent)
columnRemoved(TableColumnModelEvent)
columnMarginChanged(ChangeEvent)
columnSelectionChanged(ListSelectionEvent)
TableModelListener   none  tableChanged(TableModelEvent)
TreeExpansionListener  none treeCollapsed(TreeExpansionEvent)
treeExpanded(TreeExpansionEvent)
TreeModelListener    none   treeNodesChanged(TreeModelEvent)
treeNodesInserted(TreeModelEvent)
treeNodesRemoved(TreeModelEvent)
treeStructureChanged(TreeModelEvent)
TreeSelectionListener    none   valueChanged(TreeSelectionEvent)
TreeWillExpandListener   none  treeWillCollapse(TreeExpansionEvent)
treeWillExpand(TreeExpansionEvent)
UndoableEditListener none    undoableEditHappened(UndoableEditEvent)
VetoableChangeListener none    vetoableChange(PropertyChangeEvent)
WindowFocusListener    WindowAdapter  windowGainedFocus(WindowEvent)
windowLostFocus(WindowEvent)
WindowListener   WindowAdapter windowActivated(WindowEvent)
windowClosed(WindowEvent)
windowClosing(WindowEvent)
windowDeactivated(WindowEvent)
windowDeiconified(WindowEvent)
windowIconified(WindowEvent)
windowOpened(WindowEvent)
WindowStateListener  WindowAdapter    windowStateChanged(WindowEvent)
AdjustmentListener    none    adjustmentValueChanged(AdjustmentEvent)
")

; https://docs.oracle.com/javase/7/docs/api/javax/swing/event/package-summary.html
; Paste into open office, then copy and paste the first column.
; Removed the DocumentEvent.ElementChange line.
; Add one pair of quotes.
; This is needed for importing. listener-list-raw doesn't tell us what to import.
(def javax-swing-event-listener-classes
"AncestorListener
CaretListener
CellEditorListener
ChangeListener
DocumentEvent
DocumentListener
HyperlinkListener
InternalFrameListener
ListDataListener
ListSelectionListener
MenuDragMouseListener
MenuKeyListener
MenuListener
MouseInputListener
PopupMenuListener
RowSorterListener
TableColumnModelListener
TableModelListener
TreeExpansionListener
TreeModelListener
TreeSelectionListener
TreeWillExpandListener
UndoableEditListener")

; https://docs.oracle.com/javase/7/docs/api/java/awt/event/package-summary.html
; Paste into open office, then copy and paste the first column.
; Add one pair of quotes.
(def java-awt-event-listener-classes 
"ActionListener
AdjustmentListener
AWTEventListener
ComponentListener
ContainerListener
FocusListener
HierarchyBoundsListener
HierarchyListener
InputMethodListener
ItemListener
KeyListener
MouseListener
MouseMotionListener
MouseWheelListener
TextListener
WindowFocusListener
WindowListener
WindowStateListener
"
)

; Go to https://docs.oracle.com/javase/7/docs/api/javax/swing/package-summary.html.
; Copy everything that begins with J (it's alphebetic) into open office calc.
; Copy the first column here.
; Add one pair of quotes.
(def javax-swing-widgets 
"JApplet
JButton
JCheckBox
JCheckBoxMenuItem
JColorChooser
JComboBox<E>
JComponent
JDesktopPane
JDialog
JEditorPane
JFileChooser
JFormattedTextField
JFormattedTextField.AbstractFormatter
JFormattedTextField.AbstractFormatterFactory
JFrame
JInternalFrame
JInternalFrame.JDesktopIcon
JLabel
JLayer<V extends Component>
JLayeredPane
JList<E>
JList.DropLocation
JMenu
JMenuBar
JMenuItem
JOptionPane
JPanel
JPasswordField
JPopupMenu
JPopupMenu.Separator
JProgressBar
JRadioButton
JRadioButtonMenuItem
JRootPane
JScrollBar
JScrollPane
JSeparator
JSlider
JSpinner
JSpinner.DateEditor
JSpinner.DefaultEditor
JSpinner.ListEditor
JSpinner.NumberEditor
JSplitPane
JTabbedPane
JTable
JTable.DropLocation
JTextArea
JTextField
JTextPane
JToggleButton
JToggleButton.ToggleButtonModel
JToolBar
JToolBar.Separator
JToolTip
JTree
JTree.DropLocation
JTree.DynamicUtilTreeNode
JTree.EmptySelectionModel
JViewport")

; Go to https://docs.oracle.com/javase/tutorial/uiswing/layout/visual.html
; Copy the bullet point layouts.
; Add one pair of quotes.
(def java-awt-layouts
"BorderLayout
BoxLayout
CardLayout
FlowLayout
GridBagLayout
GridLayout
GroupLayout
SpringLayout")