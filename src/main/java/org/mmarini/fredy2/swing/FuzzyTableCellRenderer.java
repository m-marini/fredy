/**
 *
 */
package org.mmarini.fredy2.swing;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.*;

import static java.lang.Math.*;

/**
 * Renders the table call for fuzzy values
 */
public class FuzzyTableCellRenderer extends DefaultTableCellRenderer {
    public static final String FALSE_ID = "false";
    public static final String QUITE_FALSE_ID = "quite.false";
    public static final String UNKNOWN_ID = "unknown";
    public static final String QUITE_TRUE_ID = "quite.true";
    public static final String TRUE_ID = "true";
    public static final FuzzyTableCellRenderer FUZZY_TABLE_CELL_RENDERER = new FuzzyTableCellRenderer();
    private static final Color[] BG_COLORS = new Color[]{
            new Color(0xdc3545),
            new Color(0xffc107),
            new Color(0xf8f9fa),
            new Color(0x0dcaf0),
            new Color(0x198754)
    };
    private static final Color[] FG_COLORS = new Color[]{
            Color.WHITE,
            Color.BLACK,
            Color.BLACK,
            Color.BLACK,
            Color.WHITE
    };
    private static final String[] IDENTIFIERS = new String[]{
            FALSE_ID,
            QUITE_FALSE_ID,
            UNKNOWN_ID,
            QUITE_TRUE_ID,
            TRUE_ID
    };

    /**
     * Returns the color by value
     *
     * @param value the fuzzy value
     */
    public static Color getBackgroundColor(double value) {
        return BG_COLORS[getFuzzyIndex(value)];
    }

    /**
     * Returns the foreground color by value
     *
     * @param value the fuzzy value
     */
    public static Color getForegroundColor(double value) {
        return FG_COLORS[getFuzzyIndex(value)];
    }

    /**
     * Returns the identifier of fuzzy value
     *
     * @param value the value
     */
    public static String getFuzzyDescription(double value) {
        return MessagesUtils.format(
                "FuzzyBoolean." + getFuzzyId(value),
                value);
    }

    /**
     * Returns the identifier of fuzzy value
     *
     * @param value the value
     */
    public static String getFuzzyId(double value) {
        return IDENTIFIERS[getFuzzyIndex(value)];
    }

    /**
     * Returns the index of fuzzy value
     *
     * @param value the value
     */
    public static int getFuzzyIndex(double value) {
        int idx = (int) round(value * (BG_COLORS.length - 1));
        return min(max(0, idx), BG_COLORS.length - 1);
    }

    /**
     *
     */
    public FuzzyTableCellRenderer() {
    }

    @Override
    public Component getTableCellRendererComponent(JTable table, Object value,
                                                   boolean isSelected, boolean hasCellFocus, int row, int column) {
        if (value instanceof Double) {
            value = getFuzzyDescription((Double) value);
        }
        Color bg = getBackgroundColor((Double) table.getValueAt(row, 1));
        Color fg = getForegroundColor((Double) table.getValueAt(row, 1));
        Component r = super.getTableCellRendererComponent(table, value,
                isSelected, hasCellFocus, row, column);
        r.setBackground(bg);
        r.setForeground(fg);
        return r;
    }

}
