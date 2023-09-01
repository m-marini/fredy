/**
 *
 */
package org.mmarini.fredy2.swing;

import org.mmarini.fredy2.model.PredicateStatus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.table.AbstractTableModel;
import java.util.List;
import java.util.Properties;

import static org.mmarini.fredy2.swing.MessagesUtils.format;

/**
 * Models the table on the predicate values
 */
public class PredicateTableModel extends AbstractTableModel {
    private static final Logger logger = LoggerFactory.getLogger(PredicateTableModel.class);
    private final String[] colName;
    private boolean editable;
    private String[] ids;
    private double[] values;
    private Properties mapper;

    /**
     * Creates the table model
     */
    public PredicateTableModel() {
        logger.atDebug().log("Created");
        colName = new String[]{
                format("PredicateTableModel.predicate"), format("PredicateTableModel.value")};
        mapper = new Properties();
    }

    @Override
    public Class<?> getColumnClass(int columnIndex) {
        if (columnIndex == 1) {
            return Double.class;
        }
        return String.class;
    }

    @Override
    public int getColumnCount() {
        return 2;
    }

    @Override
    public String getColumnName(int i) {
        return colName[i];
    }

    @Override
    public int getRowCount() {
        return ids != null ? ids.length : 0;
    }

    @Override
    public Object getValueAt(int rowIndex, int columnIndex) {
        switch (columnIndex) {
            case 0:
                return "<html>" + mapper.getOrDefault(ids[rowIndex], ids[rowIndex]) + "</html>";
            case 1:
                return values[rowIndex];
            default:
                return "???";
        }
    }

    @Override
    public boolean isCellEditable(int rowIndex, int columnIndex) {
        return columnIndex == 1 && editable;
    }

    /**
     * Returns true if values are editable
     */
    public boolean isEditable() {
        return editable;
    }

    /**
     * Sets the values editabile
     *
     * @param editable true if values are editable
     */
    public void setEditable(boolean editable) {
        this.editable = editable;
        fireTableStructureChanged();
    }

    /**
     * Set the language mapper
     *
     * @param mapper the mapper
     */
    public void setMapper(Properties mapper) {
        this.mapper = mapper;
        fireTableDataChanged();
    }

    /**
     * Sets the predicates
     *
     * @param predicates the predicates
     */
    public void setPredicates(List<? extends PredicateStatus> predicates) {
        ids = predicates.stream().map(PredicateStatus::getId).toArray(String[]::new);
        values = predicates.stream().mapToDouble(PredicateStatus::getTruth).toArray();
        fireTableDataChanged();
    }

    @Override
    public void setValueAt(Object value, int rowIndex, int columnIndex) {
        if (value instanceof Double && columnIndex == 1) {
            if (values[rowIndex] != (Double) value) {
                values[rowIndex] = (Double) value;
                fireTableCellUpdated(rowIndex, columnIndex);
            }
        }
    }
}
