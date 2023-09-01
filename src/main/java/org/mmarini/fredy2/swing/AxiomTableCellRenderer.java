/**
 *
 */
package org.mmarini.fredy2.swing;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.*;

import static org.mmarini.fredy2.swing.AxiomTableModel.*;

/**
 * Renders the table call for fuzzy values
 */
public class AxiomTableCellRenderer extends DefaultTableCellRenderer {
    public static final AxiomTableCellRenderer AXIOM_TABLE_CELL_RENDERER = new AxiomTableCellRenderer();
    private static final Logger logger = LoggerFactory.getLogger(AxiomTableCellRenderer.class);
    private final JRadioButton checkBox;

    /**
     * Creates the axiom cell renderer
     */
    public AxiomTableCellRenderer() {
        this.checkBox = new JRadioButton();
    }

    @Override
    public Component getTableCellRendererComponent(JTable table, Object value,
                                                   boolean isSelected, boolean hasCellFocus, int row, int column) {
        int selectedCol = UNKNOWN_COLUMN;
        for (int i = TRUE_COLUMN; i <= FALSE_COLUMN; i++) {
            if ((Boolean) table.getValueAt(row, i)) {
                selectedCol = i;
                break;
            }
        }
        double truth = TRUTH_VALUES_BY_COLUMN[selectedCol];
        Color bg = FuzzyTableCellRenderer.getBackgroundColor(truth);
        Color fg = FuzzyTableCellRenderer.getForegroundColor(truth);
        if (value instanceof Boolean) {
            checkBox.setSelected((Boolean) value);
            checkBox.setBackground(bg);
            checkBox.setForeground(fg);
            return checkBox;
        } else {
            Component r = super.getTableCellRendererComponent(table, value,
                    isSelected, hasCellFocus, row, column);
            r.setBackground(bg);
            r.setForeground(fg);
            return r;
        }
    }

}
