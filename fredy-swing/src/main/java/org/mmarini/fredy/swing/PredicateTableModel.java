/**
 * 
 */
package org.mmarini.fredy.swing;

import static org.mmarini.fredy.swing.MessagesUtils.format;

import java.util.List;

import javax.swing.table.AbstractTableModel;

import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.PredicateValue;

/**
 * @author us00852
 * 
 */
public class PredicateTableModel extends AbstractTableModel {
	private static final long serialVersionUID = 1714091983473406882L;
	private String[] colName;
	private List<PredicateValue> predicates;
	private boolean editable;

	/**
	 * 
	 */
	public PredicateTableModel() {
		colName = new String[] {
				format("PredicateTableModel.predicate"), format("PredicateTableModel.value") }; //$NON-NLS-1$ //$NON-NLS-2$
	}

	/**
	 * @see javax.swing.table.AbstractTableModel#getColumnClass(int)
	 */
	@Override
	public Class<?> getColumnClass(int columnIndex) {
		switch (columnIndex) {
		case 1:
			return FuzzyBoolean.class;
		default:
			return String.class;
		}
	}

	/**
	 * @see javax.swing.table.TableModel#getColumnCount()
	 */
	@Override
	public int getColumnCount() {
		return 2;
	}

	/**
	 * @see javax.swing.table.AbstractTableModel#getColumnName(int)
	 */
	@Override
	public String getColumnName(int i) {
		return colName[i];
	}

	/**
	 * @return the predicates
	 */
	public List<PredicateValue> getPredicates() {
		return predicates;
	}

	/**
	 * @see javax.swing.table.TableModel#getRowCount()
	 */
	@Override
	public int getRowCount() {
		return predicates != null ? predicates.size() : 0;
	}

	/**
	 * @see javax.swing.table.TableModel#getValueAt(int, int)
	 */
	@Override
	public Object getValueAt(int rowIndex, int columnIndex) {
		PredicateValue p = predicates.get(rowIndex);
		FuzzyBoolean v = p.getValue();
		switch (columnIndex) {
		case 0:
			return format("Predicates." + p.getPredicate());
		case 1:
			return v;
		default:
			return "???";
		}
	}

	/**
	 * @see javax.swing.table.AbstractTableModel#isCellEditable(int, int)
	 */
	@Override
	public boolean isCellEditable(int rowIndex, int columnIndex) {
		return columnIndex == 1 && editable;
	}

	/**
	 * @return the editable
	 */
	public boolean isEditable() {
		return editable;
	}

	/**
	 * @param editable
	 *            the editable to set
	 */
	public void setEditable(boolean editable) {
		this.editable = editable;
		fireTableStructureChanged();
	}

	/**
	 * 
	 * @param values
	 */
	public void setPredicates(List<PredicateValue> values) {
		predicates = values;
		fireTableDataChanged();
	}

	/**
	 * @see javax.swing.table.AbstractTableModel#setValueAt(java.lang.Object,
	 *      int, int)
	 */
	@Override
	public void setValueAt(Object value, int rowIndex, int columnIndex) {
		if (value != null && value instanceof FuzzyBoolean && columnIndex == 1) {
			predicates.get(rowIndex).setValue((FuzzyBoolean) value);
			fireTableCellUpdated(rowIndex, columnIndex);
		}
	}
}
