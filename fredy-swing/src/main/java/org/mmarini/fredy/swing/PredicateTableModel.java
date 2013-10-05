/**
 * 
 */
package org.mmarini.fredy.swing;

import java.util.List;
import static org.mmarini.fredy.swing.MessagesUtils.format;

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

	/**
	 * 
	 */
	public PredicateTableModel() {
		colName = new String[] {
				format("PredicateTableModel.predicate"), format("PredicateTableModel.value") }; //$NON-NLS-1$ //$NON-NLS-2$
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
	 * @see javax.swing.table.TableModel#getRowCount()
	 */
	@Override
	public int getRowCount() {
		return predicates != null ? predicates.size() : 0;
	}

	/**
	 * @see javax.swing.table.TableModel#getColumnCount()
	 */
	@Override
	public int getColumnCount() {
		return 2;
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
			return format("FuzzyBoolean." + v.getDescription(), v.getValue());
		default:
			return "???";
		}
	}

	/**
	 * @see javax.swing.table.AbstractTableModel#getColumnName(int)
	 */
	@Override
	public String getColumnName(int i) {
		return colName[i];
	}
}
