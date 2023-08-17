/**
 *
 */
package org.mmarini.fredy2.swing;

import hu.akarnokd.rxjava3.swing.SwingObservable;
import io.reactivex.rxjava3.core.BackpressureStrategy;
import org.mmarini.fredy2.model.Evidences;
import org.mmarini.fredy2.model.Model;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.event.TableModelEvent;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.File;

import static org.mmarini.fredy2.swing.MessagesUtils.format;

/**
 * The window app frame management
 */
public class Main {
    private static final String MAIN_INIT_ERROR = "Main.init.error"; //$NON-NLS-1$
    private static final String DEFAULT_RULES_YAML = "/animals.yml"; //$NON-NLS-1$
    private static final String MAIN_INFERENCES_LABEL = "Main.inferences.label"; //$NON-NLS-1$
    private static final String MAIN_HYPOTHESIS_LABEL = "Main.hypothesis.label"; //$NON-NLS-1$
    private static final String MAIN_AXIOMS_LABEL = "Main.axioms.label"; //$NON-NLS-1$
    private static final String MAIN_TITLE = "Main.title"; //$NON-NLS-1$
    private static final String MAIN_FILE_CANNOT_BE_READ_ERROR = "Main.fileCannotBeRead.error"; //$NON-NLS-1$
    private static final String MAIN_OPENING_ERROR = "Main.opening.error"; //$NON-NLS-1$
    private static final String MAIN_OPEN_ACTION = "Main.open.action"; //$NON-NLS-1$
    private static final String MAIN_CLEAR_ACTION = "Main.clear.action"; //$NON-NLS-1$
    private static final String MAIN_ALERT_TITLE = "Main.alert.title"; //$NON-NLS-1$

    private static final Logger logger = LoggerFactory.getLogger(Main.class);

    /**
     * Starts the application
     *
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        new Main().run(args);
    }

    private final JFrame frame;
    private final PredicateTableModel hypothesisTableModel;
    private final PredicateTableModel inferencesTableModel;
    private final PredicateTableModel axiomsTableModel;

    private final JFileChooser fileChooser;
    private final JSplitPane verticalPane;
    private final JSplitPane hsp;

    private final JButton openButton;
    private final JButton clearButton;
    private Model model;
    private Evidences evidences;

    /**
     *
     */
    public Main() {
        frame = new JFrame(Messages.getString(MAIN_TITLE));
        fileChooser = new JFileChooser();
        axiomsTableModel = new PredicateTableModel();
        inferencesTableModel = new PredicateTableModel();
        hypothesisTableModel = new PredicateTableModel();
        hsp = new JSplitPane();
        verticalPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        openButton = new JButton();
        clearButton = new JButton();
        init();
        createFlow();
    }

    /**
     * Shows message dialog
     *
     * @param messageKey the message key
     * @param params     the parameters
     */
    private void alert(String messageKey, Object... params) {
        String message = format(messageKey, params);
        logger.atError().log(message);
        JOptionPane
                .showMessageDialog(frame, message,
                        Messages.getString(MAIN_ALERT_TITLE),
                        JOptionPane.ERROR_MESSAGE);
    }

    /**
     * Shows a dialog message for error
     *
     * @param messageKey the message key
     * @param ex         the exception
     */
    private void alertError(String messageKey, Throwable ex) {
        String message = format(messageKey, ex.getLocalizedMessage());
        logger.atError().setCause(ex).log();
        JOptionPane
                .showMessageDialog(frame, message,
                        Messages.getString(MAIN_ALERT_TITLE),
                        JOptionPane.ERROR_MESSAGE);
    }

    /**
     *
     */
    private void analyze() {
        this.evidences = model.apply(evidences);
        hypothesisTableModel.setPredicates(evidences.getHypothesisList());
        inferencesTableModel.setPredicates(evidences.getInferencesList());
    }

    /**
     * Returns the axioms panel
     */
    private Component createAxiomsPane() {
        JScrollPane c = new JScrollPane(new PredicateTable(axiomsTableModel));
        c.setBorder(BorderFactory.createTitledBorder(Messages
                .getString(MAIN_AXIOMS_LABEL)));
        return c;
    }

    /**
     * Creates the flows
     */
    private void createFlow() {
        SwingObservable.tableModel(axiomsTableModel)
                .toFlowable(BackpressureStrategy.BUFFER)
                .doOnNext(this::handleAxiomsChanged)
                .subscribe();
        //axiomsTableModel.addTableModelListener(ev -> analyze());
        SwingObservable.actions(openButton)
                .toFlowable(BackpressureStrategy.BUFFER)
                .doOnNext(this::handleOpen)
                .subscribe();
        SwingObservable.actions(clearButton)
                .toFlowable(BackpressureStrategy.BUFFER)
                .doOnNext(this::handleClear)
                .subscribe();
    }

    /**
     * Creates the frame content
     */
    private void createFrameContent() {
        verticalPane.setTopComponent(createHypothesisPane());
        verticalPane.setBottomComponent(createInferencesPane());

        hsp.setLeftComponent(createAxiomsPane());
        hsp.setRightComponent(verticalPane);

        Container cp = frame.getContentPane();
        cp.setLayout(new BorderLayout());
        cp.add(hsp, BorderLayout.CENTER);
        cp.add(createToolBar(), BorderLayout.NORTH);
    }

    /**
     * Returns the hypothesis panel
     */
    private Component createHypothesisPane() {
        JScrollPane c = new JScrollPane(
                new PredicateTable(hypothesisTableModel));
        c.setBorder(BorderFactory.createTitledBorder(Messages
                .getString(MAIN_HYPOTHESIS_LABEL)));
        return c;
    }

    /**
     * Returns the inferences panel
     */
    private Component createInferencesPane() {
        JScrollPane c = new JScrollPane(
                new PredicateTable(inferencesTableModel));
        c.setBorder(BorderFactory.createTitledBorder(Messages
                .getString(MAIN_INFERENCES_LABEL)));
        return c;
    }

    /**
     * Returns the toolbar
     */
    private Component createToolBar() {
        JToolBar tb = new JToolBar();
        tb.add(openButton);
        tb.add(new JSeparator(JSeparator.VERTICAL));
        tb.add(clearButton);
        return tb;
    }

    /**
     * Handles axioms changed
     *
     * @param tableModelEvent the event
     */
    private void handleAxiomsChanged(TableModelEvent tableModelEvent) {
        axiomsTableModel.getPredicates().forEach(t -> evidences.put(t._1, t._2));
        analyze();
    }

    /**
     * Handles the clear events
     *
     * @param actionEvent the event
     */
    private void handleClear(ActionEvent actionEvent) {
        evidences = model.createUnknownAxioms();
        analyze();
    }

    /**
     * Handles the open events
     *
     * @param actionEvent the event
     */
    private void handleOpen(ActionEvent actionEvent) {
        if (fileChooser.showOpenDialog(frame) == JFileChooser.APPROVE_OPTION) {
            File file = fileChooser.getSelectedFile();
            if (!file.canRead()) {
                alert(MAIN_FILE_CANNOT_BE_READ_ERROR, file);
            } else {
                try {
                    setModel(Model.fromFile(file));
                } catch (Exception e) {
                    alertError(MAIN_OPENING_ERROR, e);
                }
            }
        }
    }

    /**
     * Initializes the component
     */
    private void init() {
        verticalPane.setOneTouchExpandable(true);
        verticalPane.setResizeWeight(0.5);
        hsp.setOneTouchExpandable(true);
        hsp.setResizeWeight(0.5);

        openButton.setText(Messages.getString(MAIN_OPEN_ACTION));
        clearButton.setText(Messages.getString(MAIN_CLEAR_ACTION));

        createFrameContent();

        axiomsTableModel.setEditable(true);

        fileChooser.setFileFilter(new FileNameExtensionFilter(Messages
                .getString("Main.fileType.label"), "yml", "yaml"));

        frame.setSize(640, 480);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    }

    /**
     * Starts the application
     *
     * @param args the command line argument
     */
    private void run(String[] args) {
        logger.atDebug().log("Main started");
        frame.setVisible(true);
        try {
            Model model = args.length > 0
                    ? Model.fromUrl(args[0])
                    : Model.fromResource(DEFAULT_RULES_YAML);
            setModel(model);
        } catch (Exception e) {
            alertError(MAIN_INIT_ERROR, e);
        }
        verticalPane.setDividerLocation(0.5);
        hsp.setDividerLocation(0.5);
    }

    /**
     * Sets the model
     *
     * @param model the model
     */
    private void setModel(Model model) {
        this.model = model;
        this.evidences = model.createUnknownAxioms();
        hypothesisTableModel.setPredicates(evidences.getHypothesisList());
        inferencesTableModel.setPredicates(evidences.getInferencesList());
        axiomsTableModel.setPredicates(evidences.getAxiomsList());
    }
}
