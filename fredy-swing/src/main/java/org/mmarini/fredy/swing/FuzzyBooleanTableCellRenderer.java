/**
 * 
 */
package org.mmarini.fredy.swing;

import static org.mmarini.fredy.swing.MessagesUtils.format;

import java.awt.Color;
import java.awt.Component;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;

import org.mmarini.fuzzy.FuzzyBoolean;

/**
 * @author us00852
 * 
 */
public class FuzzyBooleanTableCellRenderer extends DefaultTableCellRenderer {
	private static final long serialVersionUID = -681880681487606466L;
	private static Map<String, Color> colorMap;

	/**
	 * 
	 * @param value
	 * @return
	 */
	public static Color getColorByValue(FuzzyBoolean value) {
		return colorMap.get(value.getDescription());
	}

	{
		colorMap = new HashMap<String, Color>();
		colorMap.put(FuzzyBoolean.FALSE_DESCRIPTION, new Color(0xFFCCCC));
		colorMap.put(FuzzyBoolean.QUITE_FALSE_DESCRIPTION, new Color(0xFFEEEE));
		colorMap.put(FuzzyBoolean.UNKNOWN_DESCRIPTION, new Color(0xF8F8F8));
		colorMap.put(FuzzyBoolean.QUITE_TRUE_DESCRIPTION, new Color(0xeeffee));
		colorMap.put(FuzzyBoolean.TRUE_DESCRIPTION, new Color(0xCCFFCC));
	}

	/**
	 * 
	 */
	public FuzzyBooleanTableCellRenderer() {
	}

	/**
	 * @see javax.swing.table.DefaultTableCellRenderer#getTableCellRendererComponent
	 *      (javax.swing.JTable, java.lang.Object, boolean, boolean, int, int)
	 */
	@Override
	public Component getTableCellRendererComponent(JTable table, Object value,
			boolean isSelected, boolean hasCellFocus, int row, int column) {
		if (value != null && value instanceof FuzzyBoolean) {
			value = format(
					"FuzzyBoolean." + ((FuzzyBoolean) value).getDescription(),
					((FuzzyBoolean) value).getValue());
		}
		Color c = getColorByValue((FuzzyBoolean) table.getValueAt(row, 1));
		Component r = super.getTableCellRendererComponent(table, value,
				isSelected, hasCellFocus, row, column);
		r.setBackground(c);
		return r;
	}

}
