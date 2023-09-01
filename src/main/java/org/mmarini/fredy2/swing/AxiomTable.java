/**
 *
 */
package org.mmarini.fredy2.swing;

import javax.swing.*;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import java.awt.*;

import static org.mmarini.fredy2.swing.AxiomTableCellRenderer.AXIOM_TABLE_CELL_RENDERER;
import static org.mmarini.fredy2.swing.AxiomTableModel.FALSE_COLUMN;
import static org.mmarini.fredy2.swing.AxiomTableModel.TRUE_COLUMN;

/**
 * Shows a list of predicate with values
 */
public class AxiomTable extends JTable {
    /**
     * Creates the table
     *
     * @param tableModel the table model
     */
    public AxiomTable(AxiomTableModel tableModel) {
        super(tableModel);
        //setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        setDefaultRenderer(Boolean.class, AXIOM_TABLE_CELL_RENDERER);
        setDefaultRenderer(String.class, AXIOM_TABLE_CELL_RENDERER);
        setDefaultEditor(Boolean.class, new AxiomTableCellEditor());
        setRowHeight(Messages.getInt("AxiomTable.rowHeight", getRowHeight()));

        JTableHeader tableHeader1 = getTableHeader();
        tableHeader1.setPreferredSize(new Dimension(tableHeader1.getPreferredSize().width,
                Messages.getInt("AxiomTable.headerHeight", tableHeader1.getPreferredSize().height)));

        TableColumnModel columnModel = getColumnModel();

        TableColumn column0 = columnModel.getColumn(0);
        column0
                .setPreferredWidth(Messages.getInt("AxiomTable.predicate.width",
                        column0.getPreferredWidth()));
        for (int i = TRUE_COLUMN; i <= FALSE_COLUMN; i++) {
            TableColumn column = columnModel.getColumn(i);
            int truthWidth = Messages.getInt("AxiomTable.truth.width", column.getPreferredWidth());
            column.setPreferredWidth(truthWidth);
        }
    }
}
