/**
 *
 */
package org.mmarini.fredy2.swing;

import org.mmarini.Tuple2;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.table.AbstractTableModel;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

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

    /**
     * Creates the table model
     */
    public PredicateTableModel() {
        logger.atDebug().log("Created");
        colName = new String[]{
                format("PredicateTableModel.predicate"), format("PredicateTableModel.value")};
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

    /**
     * Returns the predicates
     */
    public List<Tuple2<String, Double>> getPredicates() {
        return ids != null
                ? IntStream.range(0, ids.length)
                .mapToObj(i -> Tuple2.of(ids[i], values[i]))
                .collect(Collectors.toList())
                : List.of();
    }

    /**
     * Sets the predicates
     *
     * @param predicates the predicates
     */
    public void setPredicates(List<Tuple2<String, Double>> predicates) {
        ids = predicates.stream().map(Tuple2::getV1).toArray(String[]::new);
        values = predicates.stream().mapToDouble(Tuple2::getV2).toArray();
        fireTableDataChanged();
    }

    @Override
    public int getRowCount() {
        return ids != null ? ids.length : 0;
    }

    @Override
    public Object getValueAt(int rowIndex, int columnIndex) {
        switch (columnIndex) {
            case 0:
                return format("Predicates." + ids[rowIndex]);
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
