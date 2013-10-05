/**
 * 
 */
package org.mmarini.fredy.swing;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JToolBar;

import org.mmarini.fuzzy.PredicateValue;

/**
 * @author us00852
 * 
 */
public class Main {

	private JFrame frame;
	private FredyHandler handler;
	private PredicateTableModel hypothesisTableModel;
	private PredicateTableModel inferencesTableModel;
	private PredicateTableModel axiomsTableModel;
	private AbstractAction analyzeAction;

	/**
	 * 
	 */
	public Main() {
		frame = new JFrame(Messages.getString("Main.title")); //$NON-NLS-1$
		handler = new FredyHandler();
		axiomsTableModel = new PredicateTableModel();
		inferencesTableModel = new PredicateTableModel();
		hypothesisTableModel = new PredicateTableModel();

		analyzeAction = new AbstractAction() {
			private static final long serialVersionUID = -4093560512652496020L;

			@Override
			public void actionPerformed(ActionEvent arg0) {
				analyze();
			}
		};
		frame.setSize(480, 640);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setupActions();
		createFrameContent();
	}

	/**
	 * 
	 */
	private void setupActions() {
		analyzeAction.putValue(Action.NAME, Messages.getString("Main.analyze.button")); //$NON-NLS-1$
	}

	/**
	 * 
	 */
	private void createFrameContent() {
		Container cp = frame.getContentPane();
		cp.setLayout(new BorderLayout());
		JTabbedPane tp = new JTabbedPane();
		tp.addTab(Messages.getString("Main.axiom.label"), createAxiomsPane()); //$NON-NLS-1$
		tp.addTab(Messages.getString("Main.hypothesis.label"), createHypothesysPane()); //$NON-NLS-1$
		tp.addTab(Messages.getString("Main.inferences.label"), createInferencesPane()); //$NON-NLS-1$
		cp.add(tp, BorderLayout.CENTER);
		cp.add(createToolBar(), BorderLayout.NORTH);
	}

	/**
	 * 
	 * @return
	 */
	private Component createToolBar() {
		JToolBar tb = new JToolBar();
		tb.add(analyzeAction);
		return tb;
	}

	/**
	 * 
	 * @return
	 */
	private Component createInferencesPane() {
		return new JScrollPane(new JTable(inferencesTableModel));
	}

	/**
	 * 
	 * @return
	 */
	private Component createAxiomsPane() {
		return new JScrollPane(new JTable(axiomsTableModel));
	}

	/**
	 * 
	 * @return
	 */
	private Component createHypothesysPane() {
		return new JScrollPane(new JTable(hypothesisTableModel));
	}

	/**
	 * @param args
	 * @throws MalformedURLException
	 */
	public static void main(String[] args) throws MalformedURLException {
		new Main().run(args);
	}

	/**
	 * 
	 * @param args
	 * @throws MalformedURLException
	 */
	private void run(String[] args) throws MalformedURLException {
		URL resource;
		if (args.length > 0) {
			resource = new URL(args[0]);
		} else {
			resource = getClass().getResource("/animals-rules.xml"); //$NON-NLS-1$
		}

		handler.loadRules(resource);
		analyze();
		frame.setVisible(true);
	}

	private void analyze() {
		handler.analyze();
		hypothesisTableModel.setPredicates(handler
				.addHypothesisTo(new ArrayList<PredicateValue>()));
		axiomsTableModel.setPredicates(handler
				.addAxiomsTo(new ArrayList<PredicateValue>()));
		inferencesTableModel.setPredicates(handler
				.addInferencesTo(new ArrayList<PredicateValue>()));
	}

}
