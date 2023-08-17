/**
 *
 */
package org.mmarini.fredy2.swing;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;

/**
 * Shows a list of predicate with values
 */
public class PredicateTable extends JTable {
    private static final Logger logger = LoggerFactory.getLogger(PredicateTable.class);

    /**
     * Creates the table
     *
     * @param tableModel the table model
     */
    public PredicateTable(PredicateTableModel tableModel) {
        super(tableModel);
        setDefaultRenderer(String.class, new FuzzyTableCellRenderer());
        setDefaultRenderer(Double.class, new FuzzyTableCellRenderer());
        JComboBox<Double> editor = new JComboBox<>(new Double[]{
                FuzzyTableCellRenderer.FALSE_VALUE,
                FuzzyTableCellRenderer.QUITE_FALSE_VALUE,
                FuzzyTableCellRenderer.UNKNOWN_VALUE,
                FuzzyTableCellRenderer.QUITE_TRUE_VALUE,
                FuzzyTableCellRenderer.TRUE_VALUE});
        editor.setRenderer(new FuzzyListRenderer());
        setDefaultEditor(Double.class, new DefaultCellEditor(editor));
        logger.atDebug().log("Created");
    }
}
