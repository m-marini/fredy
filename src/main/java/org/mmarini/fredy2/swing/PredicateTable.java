/**
 *
 */
package org.mmarini.fredy2.swing;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import java.awt.*;

import static org.mmarini.fredy2.swing.FuzzyTableCellRenderer.FUZZY_TABLE_CELL_RENDERER;

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
        setDefaultRenderer(String.class, FUZZY_TABLE_CELL_RENDERER);
        setDefaultRenderer(Double.class, FUZZY_TABLE_CELL_RENDERER);
        setRowHeight(Messages.getInt("PredicateTable.rowHeight", getRowHeight()));
        JTableHeader tableHeader1 = getTableHeader();
        tableHeader1.setPreferredSize(new Dimension(tableHeader1.getPreferredSize().width,
                Messages.getInt("PredicateTable.headerHeight", tableHeader1.getPreferredSize().height)));
        TableColumnModel columnModel = getColumnModel();

        TableColumn column0 = columnModel.getColumn(0);
        column0
                .setPreferredWidth(Messages.getInt("PredicateTable.predicate.width",
                        column0.getPreferredWidth()));
        TableColumn column = columnModel.getColumn(1);
        int truthWidth = Messages.getInt("PredicateTable.truth.width", column.getPreferredWidth());
        column.setPreferredWidth(truthWidth);

        logger.atDebug().log("Created");
    }
}