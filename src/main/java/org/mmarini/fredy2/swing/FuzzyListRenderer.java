/**
 *
 */
package org.mmarini.fredy2.swing;

import javax.swing.*;
import java.awt.*;

/**
 * Renders list of fuzzy value
 */
public class FuzzyListRenderer extends DefaultListCellRenderer {

    /**
     * Creates the renderer
     */
    public FuzzyListRenderer() {
    }

    @Override
    public Component getListCellRendererComponent(JList list, Object value,
                                                  int index, boolean isSelected, boolean cellHasFocus) {
        if (value instanceof Double) {
            Color c = FuzzyTableCellRenderer
                    .getColor((Double) value);
            value = FuzzyTableCellRenderer.getFuzzyDescription((Double) value);
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
