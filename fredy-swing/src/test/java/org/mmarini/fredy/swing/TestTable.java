/**
 * 
 */
package org.mmarini.fredy.swing;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.util.ArrayList;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JScrollPane;

import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.PredicateValue;

/**
 * @author us00852
 * 
 */
public class TestTable {

	public static void main(String[] args) throws Throwable {
		new TestTable().run();
	}

	private JFrame frame;
	private AbstractAction testAction;

	private PredicateTable table;

	/**
	 * 
	 */
	public TestTable() {
		frame = new JFrame();
		table = new PredicateTable();
		testAction = new AbstractAction() {
			private static final long serialVersionUID = 3068753504491589681L;

			@Override
			public void actionPerformed(ActionEvent arg0) {
				test();
			}
		};

		testAction.putValue(Action.NAME, "Test");
		Container cp = frame.getContentPane();
		cp.setLayout(new BorderLayout());
		cp.add(new JScrollPane(table), BorderLayout.CENTER);

		cp.add(new JButton(testAction), BorderLayout.SOUTH);
		frame.setSize(300, 200);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}

	private void run() {
		frame.setVisible(true);

		ArrayList<PredicateValue> list = new ArrayList<PredicateValue>();
		list.add(new PredicateValue("Falso", FuzzyBoolean.FALSE));
		list.add(new PredicateValue("Vero", FuzzyBoolean.TRUE));
		((PredicateTableModel) table.getModel()).setPredicates(list);
	}

	protected void test() {
		// combo.setSelectedItem(new FuzzyBoolean(0.1));
	}
}
