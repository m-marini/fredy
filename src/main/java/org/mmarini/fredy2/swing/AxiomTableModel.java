/**
 *
 */
package org.mmarini.fredy2.swing;

import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.processors.PublishProcessor;
import org.mmarini.fredy2.model.AxiomStatus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.table.AbstractTableModel;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;

import static java.lang.Math.round;
import static org.mmarini.Utils.zipWithIndex;
import static org.mmarini.fredy2.swing.MessagesUtils.format;

/**
 * Models the table on the predicate values
 */
public class AxiomTableModel extends AbstractTableModel {
    public static final int TRUE_COLUMN = 1;
    public static final int UNKNOWN_COLUMN = 3;
    public static final int FALSE_COLUMN = 5;
    public static final double[] TRUTH_VALUES_BY_COLUMN = {
            0.5,
            1,
            0.75,
            0.5,
            0.25,
            0
    };
    private static final Logger logger = LoggerFactory.getLogger(AxiomTableModel.class);
    private final String[] colName;
    private final PublishProcessor<List<AxiomStatus>> axiomsProc;
    private String[] ids;
    private int[] selectedColumns;
    private List<AxiomStatus> axioms;
    private Properties mapper;

    /**
     * Creates the table model
     */
    public AxiomTableModel() {
        logger.atDebug().log("Created");
        colName = new String[]{
                format("AxiomTableModel.predicate.title"),
                format("AxiomTableModel.true.title"),
                format("AxiomTableModel.quiteTrue.title"),
                format("AxiomTableModel.unknown.title"),
                format("AxiomTableModel.quiteFalse.title"),
                format("AxiomTableModel.false.title")
        };
        mapper = new Properties();
        this.axiomsProc = PublishProcessor.create();
    }

    /**
     * Returns the predicates
     */
    public List<AxiomStatus> getAxioms() {
        return zipWithIndex(axioms)
                .map(t -> {
                    double truth = TRUTH_VALUES_BY_COLUMN[this.selectedColumns[t._1]];
                    return t._2.setTruth(truth);
                })
                .collect(Collectors.toList());
    }

    @Override
    public Class<?> getColumnClass(int columnIndex) {
        return columnIndex == 0 ? String.class : Boolean.class;
    }

    @Override
    public int getColumnCount() {
        return 6;
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
        return columnIndex == 0
                ? "<html>" + mapper.getOrDefault(ids[rowIndex], ids[rowIndex]) + "</html>"
                : columnIndex == selectedColumns[rowIndex];
    }

    @Override
    public boolean isCellEditable(int rowIndex, int columnIndex) {
        return columnIndex >= TRUE_COLUMN;
    }

    /**
     * Returns the flowable of axiom states
     */
    public Flowable<List<AxiomStatus>> readAxiomStates() {
        return axiomsProc;
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
     * Sets the axioms
     *
     * @param axioms the axioms
     */
    public void setPredicates(List<AxiomStatus> axioms) {
        this.axioms = axioms;
        this.ids = axioms.stream().map(AxiomStatus::getId).toArray(String[]::new);
        this.selectedColumns = axioms.stream().mapToInt(
                        axiom -> 5 - (int) round(axiom.getTruth() * 4))
                .toArray();
        this.fireTableDataChanged();
    }

    @Override
    public void setValueAt(Object value, int rowIndex, int columnIndex) {
        if (columnIndex >= TRUE_COLUMN) {
            int oldColumn = this.selectedColumns[rowIndex];
            this.selectedColumns[rowIndex] = columnIndex;
            fireTableCellUpdated(rowIndex, oldColumn);
            fireTableCellUpdated(rowIndex, columnIndex);
            axiomsProc.onNext(getAxioms());
        }
    }
}
