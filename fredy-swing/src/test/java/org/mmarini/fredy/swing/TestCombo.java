/**
 * 
 */
package org.mmarini.fredy.swing;

import java.awt.Container;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;

import org.mmarini.fuzzy.FuzzyBoolean;

/**
 * @author us00852
 * 
 */
public class TestCombo {

	public static void main(String[] args) throws Throwable {
		new TestCombo().run();
	}

	private JFrame frame;
	private JComboBox combo;

	private AbstractAction testAction;

	/**
	 * 
	 */
	public TestCombo() {
		frame = new JFrame();
		combo = new JComboBox(new FuzzyBoolean[] { FuzzyBoolean.FALSE,
				FuzzyBoolean.TRUE });
		combo.setRenderer(new FuzzyBooleanListRenderer());
		testAction = new AbstractAction() {
			private static final long serialVersionUID = 3068753504491589681L;

			@Override
			public void actionPerformed(ActionEvent arg0) {
				test();
			}
		};

		testAction.putValue(Action.NAME, "Test");
		Container cp = frame.getContentPane();
		cp.setLayout(new FlowLayout());
		cp.add(combo);
		cp.add(new JButton(testAction));
		frame.setSize(300, 200);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}

	private void run() {
		frame.setVisible(true);
		// combo.setEditor(new BasicComboBoxEditor());
	}

	protected void test() {
		System.out.println(combo.getSelectedItem());
		// combo.setSelectedItem(new FuzzyBoolean(0.1));
	}
}
