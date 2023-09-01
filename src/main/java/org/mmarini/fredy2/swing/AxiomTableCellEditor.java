/*
 * Copyright (c) 2023 Marco Marini, marco.marini@mmarini.org
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 *    END OF TERMS AND CONDITIONS
 */

package org.mmarini.fredy2.swing;

import javax.swing.*;
import java.awt.*;

import static org.mmarini.fredy2.swing.AxiomTableModel.*;

/**
 * The axiom cell editor
 */
public class AxiomTableCellEditor extends DefaultCellEditor {

    /**
     * Creates the fuzzy cell editor
     */
    public AxiomTableCellEditor() {
        super(new JCheckBox());
    }

    @Override
    public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
        int selectedCol = UNKNOWN_COLUMN;
        for (int i = TRUE_COLUMN; i <= FALSE_COLUMN; i++) {
            if ((Boolean) table.getValueAt(row, i)) {
                selectedCol = i;
                break;
            }
        }
        Color c = FuzzyTableCellRenderer.getColor(AxiomTableModel.TRUTH_VALUES_BY_COLUMN[selectedCol]);
        Component r = super.getTableCellEditorComponent(table, value,
                isSelected, row, column);
        r.setBackground(c);
        r.setForeground(c);
        return r;
    }
}
