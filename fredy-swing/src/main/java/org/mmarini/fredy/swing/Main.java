/**
 * 
 */
package org.mmarini.fredy.swing;

import static org.mmarini.fredy.swing.MessagesUtils.format;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JToolBar;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.mmarini.fuzzy.PredicateValue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author us00852
 * 
 */
public class Main {
	private static final String MAIN_INIT_ERROR = "Main.init.error"; //$NON-NLS-1$
	private static final String DEFAULT_RULES_XML = "/animals-rules.xml"; //$NON-NLS-1$
	private static final String MAIN_INFERENCES_LABEL = "Main.inferences.label"; //$NON-NLS-1$
	private static final String MAIN_HYPOTHESIS_LABEL = "Main.hypothesis.label"; //$NON-NLS-1$
	private static final String MAIN_AXIOMS_LABEL = "Main.axioms.label"; //$NON-NLS-1$
	private static final String MAIN_TITLE = "Main.title"; //$NON-NLS-1$
	private static final String MAIN_FILE_CANNOT_BE_READ_ERROR = "Main.fileCannotBeRead.error"; //$NON-NLS-1$
	private static final String MAIN_OPENING_ERROR = "Main.opening.error"; //$NON-NLS-1$
	private static final String MAIN_OPEN_ACTION = "Main.open.action"; //$NON-NLS-1$
	private static final String MAIN_ALERT_TITLE = "Main.alert.title"; //$NON-NLS-1$

	private static Logger logger = LoggerFactory.getLogger(Main.class);

	/**
	 * @param args
	 * @throws MalformedURLException
	 */
	public static void main(String[] args) throws MalformedURLException {
		new Main().run(args);
	}

	private JFrame frame;
	private FredyHandler handler;
	private PredicateTableModel hypothesisTableModel;
	private PredicateTableModel inferencesTableModel;
	private PredicateTableModel axiomsTableModel;
	private AbstractAction openAction;
	private boolean analyzing;

	private JFileChooser fileChooser;
	private JSplitPane verticalPane;
	private JSplitPane hsp;

	/**
	 * 
	 */
	public Main() {
		frame = new JFrame(Messages.getString(MAIN_TITLE));
		fileChooser = new JFileChooser();
		handler = new FredyHandler();
		axiomsTableModel = new PredicateTableModel();
		inferencesTableModel = new PredicateTableModel();
		hypothesisTableModel = new PredicateTableModel();
		hsp = new JSplitPane();
		verticalPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
		openAction = new AbstractAction() {
			private static final long serialVersionUID = -4093560512652496020L;

			@Override
			public void actionPerformed(ActionEvent ev) {
				open();
			}
		};
		axiomsTableModel.addTableModelListener(new TableModelListener() {

			@Override
			public void tableChanged(TableModelEvent ev) {
				analyze();
			}
		});

		verticalPane.setOneTouchExpandable(true);
		verticalPane.setResizeWeight(0.5);
		hsp.setOneTouchExpandable(true);
		hsp.setResizeWeight(0.5);
		frame.setSize(480, 640);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setupActions();
		createFrameContent();
		axiomsTableModel.setEditable(true);
		fileChooser.setFileFilter(new FileNameExtensionFilter(Messages
				.getString("Main.fileType.label"), "xml")); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/**
	 * 
	 * @param messageKey
	 * @param file
	 */
	private void alert(String messageKey, Object... parms) {
		String message = format(messageKey, parms);
		logger.error(message);
		JOptionPane
				.showMessageDialog(frame, message,
						Messages.getString(MAIN_ALERT_TITLE),
						JOptionPane.ERROR_MESSAGE);
	}

	/**
	 * 
	 * @param messageKey
	 * @param ex
	 * @param parms
	 */
	private void alert(String messageKey, Throwable ex) {
		String message = format(messageKey, ex.getLocalizedMessage());
		logger.error(message, ex);
		JOptionPane
				.showMessageDialog(frame, message,
						Messages.getString(MAIN_ALERT_TITLE),
						JOptionPane.ERROR_MESSAGE);
	}

	/**
	 * 
	 */
	private void analyze() {
		if (!analyzing) {
			analyzing = true;
			handler.setAxioms(axiomsTableModel.getPredicates());
			handler.analyze();
			hypothesisTableModel.setPredicates(handler
					.addHypothesisTo(new ArrayList<PredicateValue>()));
			axiomsTableModel.setPredicates(handler
					.addAxiomsTo(new ArrayList<PredicateValue>()));
			inferencesTableModel.setPredicates(handler
					.addInferencesTo(new ArrayList<PredicateValue>()));
			analyzing = false;
		}
	}

	/**
	 * 
	 * @return
	 */
	private Component createAxiomsPane() {
		JScrollPane c = new JScrollPane(new PredicateTable(axiomsTableModel));
		c.setBorder(BorderFactory.createTitledBorder(Messages
				.getString(MAIN_AXIOMS_LABEL)));
		return c;
	}

	/**
	 * 
	 */
	private void createFrameContent() {
		verticalPane.setTopComponent(createHypothesysPane());
		verticalPane.setBottomComponent(createInferencesPane());

		hsp.setLeftComponent(createAxiomsPane());
		hsp.setRightComponent(verticalPane);

		Container cp = frame.getContentPane();
		cp.setLayout(new BorderLayout());
		cp.add(hsp, BorderLayout.CENTER);
		cp.add(createToolBar(), BorderLayout.NORTH);
	}

	/**
	 * 
	 * @return
	 */
	private Component createHypothesysPane() {
		JScrollPane c = new JScrollPane(
				new PredicateTable(hypothesisTableModel));
		c.setBorder(BorderFactory.createTitledBorder(Messages
				.getString(MAIN_HYPOTHESIS_LABEL)));
		return c;
	}

	/**
	 * 
	 * @return
	 */
	private Component createInferencesPane() {
		JScrollPane c = new JScrollPane(
				new PredicateTable(inferencesTableModel));
		c.setBorder(BorderFactory.createTitledBorder(Messages
				.getString(MAIN_INFERENCES_LABEL)));
		return c;
	}

	/**
	 * 
	 * @return
	 */
	private Component createToolBar() {
		JToolBar tb = new JToolBar();
		tb.add(openAction);
		return tb;
	}

	/**
	 * 
	 */
	private void open() {
		if (fileChooser.showOpenDialog(frame) == JFileChooser.APPROVE_OPTION) {
			File file = fileChooser.getSelectedFile();
			if (!file.canRead()) {
				alert(MAIN_FILE_CANNOT_BE_READ_ERROR, file);
			} else {
				try {
					handler.loadRules(file.toURI().toURL());
					analyze();
				} catch (Exception e) {
					alert(MAIN_OPENING_ERROR, e);
				}
			}
		}
	}

	/**
	 * 
	 * @param args
	 */
	private void run(String[] args) {
		logger.debug("Main started"); //$NON-NLS-1$
		URL resource;
		frame.setVisible(true);
		try {
			if (args.length > 0) {
				resource = new URL(args[0]);
			} else {
				resource = getClass().getResource(DEFAULT_RULES_XML);
			}

			handler.loadRules(resource);
			analyze();
		} catch (Exception e) {
			alert(MAIN_INIT_ERROR, e, e.getMessage());
		}
		verticalPane.setDividerLocation(0.5);
		hsp.setDividerLocation(0.5);
	}

	/**
	 * 
	 */
	private void setupActions() {
		openAction.putValue(Action.NAME, Messages.getString(MAIN_OPEN_ACTION));
	}
}
