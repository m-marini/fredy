/**
 * 
 */
package org.mmarini.fredy.swing;

import static org.mmarini.fredy.swing.MessagesUtils.format;

import java.awt.Color;
import java.awt.Component;

import javax.swing.DefaultListCellRenderer;
import javax.swing.JList;

import org.mmarini.fuzzy.FuzzyBoolean;

/**
 * @author us00852
 * 
 */
public class FuzzyBooleanListRenderer extends DefaultListCellRenderer {
	private static final long serialVersionUID = 8436848767289373373L;

	/**
	 * 
	 */
	public FuzzyBooleanListRenderer() {
	}

	/**
	 * @see javax.swing.DefaultListCellRenderer#getListCellRendererComponent(javax
	 *      .swing.JList, java.lang.Object, int, boolean, boolean)
	 */
	@Override
	public Component getListCellRendererComponent(JList list, Object value,
			int index, boolean isSelected, boolean cellHasFocus) {
		if (value != null && value instanceof FuzzyBoolean) {
			Color c = FuzzyBooleanTableCellRenderer
					.getColorByValue((FuzzyBoolean) value);
			value = format(
					"FuzzyBoolean." + ((FuzzyBoolean) value).getDescription(),
					((FuzzyBoolean) value).getValue());
			Component r = super.getListCellRendererComponent(list, value,
					index, isSelected, cellHasFocus);
			r.setBackground(c);
			return r;
		} else {
			return super.getListCellRendererComponent(list, value, index,
					isSelected, cellHasFocus);
		}
	}
}
