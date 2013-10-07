/**
 * 
 */
package org.mmarini.fredy.swing;

import javax.swing.DefaultCellEditor;
import javax.swing.JComboBox;
import javax.swing.JTable;

import org.mmarini.fuzzy.FuzzyBoolean;

/**
 * @author us00852
 * 
 */
public class PredicateTable extends JTable {
	private static final long serialVersionUID = -1065914803071456093L;

	/**
	 * 
	 */
	public PredicateTable() {
		super(new PredicateTableModel());
	}

	/**
	 * 
	 * @param tableModel
	 */
	public PredicateTable(PredicateTableModel tableModel) {
		super(tableModel);
		JComboBox editor = new JComboBox(new FuzzyBoolean[] {
				FuzzyBoolean.FALSE, FuzzyBoolean.QUITE_FALSE,
				FuzzyBoolean.UNKNOWN, FuzzyBoolean.QUITE_TRUE,
				FuzzyBoolean.TRUE });
		editor.setRenderer(new FuzzyBooleanListRenderer());
		setDefaultEditor(FuzzyBoolean.class, new DefaultCellEditor(editor));
		setDefaultRenderer(String.class, new FuzzyBooleanTableCellRenderer());
		setDefaultRenderer(FuzzyBoolean.class,
				new FuzzyBooleanTableCellRenderer());
	}
}
