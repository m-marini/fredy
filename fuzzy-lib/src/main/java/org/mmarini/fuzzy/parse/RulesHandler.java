package org.mmarini.fuzzy.parse;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Queue;

import org.mmarini.fuzzy.AssignCmd;
import org.mmarini.fuzzy.AssignListCmd;
import org.mmarini.fuzzy.Expression;
import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.Rule;
import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * @author US00852
 * @version $Id: RulesHandler.java,v 1.2 2005/02/10 22:32:37 marco Exp $
 */
public class RulesHandler extends DefaultHandler {

	private Locator documentLocator;
	private List<Rule> rules;
	private StringBuilder name;
	private Map<String, ParseState> stateMap;
	private Expression condition;
	private AssignListCmd thenConsequences;
	private AssignListCmd elseConsequences;
	private Queue<Expression> expStack;
	private Queue<Queue<Expression>> listStack;
	private List<AssignCmd> cmdStack;

	/**
	 * 
	 */
	public RulesHandler() {
		cmdStack = new ArrayList<AssignCmd>();
		rules = new ArrayList<Rule>();
		name = new StringBuilder();
		listStack = Collections
				.asLifoQueue(new ArrayDeque<Queue<Expression>>());
		stateMap = new HashMap<String, ParseState>();
		stateMap.put("rule", new RuleState(this));
		stateMap.put("if", new IfState(this));
		stateMap.put("and", new AndState(this));
		stateMap.put("predicate", new PredicateState(this));
		stateMap.put("or", new OrState(this));
		stateMap.put("ifonlyif", new IfOnlyIfState(this));
		stateMap.put("implies", new ImpliesState(this));
		stateMap.put("somewhat", new SomewhatState(this));
		stateMap.put("known", new KnownState(this));
		stateMap.put("very", new VeryState(this));
		stateMap.put("not", new NotState(this));
		stateMap.put("then", new ThenState(this));
		stateMap.put("assign-not", new AssignNotState(this));
		stateMap.put("assign", new AssignState(this));
		stateMap.put("else", new ElseState(this));
		stateMap.put("false", new ConstantState(FuzzyBoolean.FALSE, this));
		stateMap.put("quite-false", new ConstantState(FuzzyBoolean.QUITE_FALSE,
				this));
		stateMap.put("unknown", new ConstantState(FuzzyBoolean.UNKNOWN, this));
		stateMap.put("quite-true", new ConstantState(FuzzyBoolean.QUITE_TRUE,
				this));
		stateMap.put("true", new ConstantState(FuzzyBoolean.TRUE, this));
	}

	/**
	 * 
	 * @param rule
	 */
	public void addRule(Rule rule) {
		rules.add(rule);
	}

	/**
	 * @see org.xml.sax.ContentHandler#characters(char[], int, int)
	 */
	@Override
	public void characters(char[] buffer, int startIndex, int lenght)
			throws SAXException {
		this.getName().append(buffer, startIndex, lenght);
	}

	/**
	 * @see org.xml.sax.ContentHandler#endElement(java.lang.String,
	 *      java.lang.String, java.lang.String)
	 */
	@Override
	public void endElement(String uri, String localName, String qName)
			throws SAXException {
		ParseState parseState = stateMap.get(qName);
		if (parseState != null)
			parseState.end();
	}

	/**
	 * @see org.xml.sax.ErrorHandler#error(org.xml.sax.SAXParseException)
	 */
	@Override
	public void error(SAXParseException error) throws SAXException {
		error.printStackTrace();
		super.error(error);
	}

	/**
	 * @return the condition
	 */
	public Expression getCondition() {
		return condition;
	}

	/**
	 * @return Returns the documentLocator.
	 */
	public Locator getDocumentLocator() {
		return documentLocator;
	}

	/**
	 * @return the elseConsequences
	 */
	public AssignListCmd getElseConsequences() {
		return elseConsequences;
	}

	/**
	 * @return Returns the name.
	 */
	public StringBuilder getName() {
		return name;
	}

	/**
	 * @return the rules
	 */
	public List<Rule> getRules() {
		return rules;
	}

	/**
	 * @return the thenConsequences
	 */
	public AssignListCmd getThenConsequences() {
		return thenConsequences;
	}

	/**
	 * @return the expression
	 */
	public Expression pop() {
		return expStack.poll();
	}

	/**
	 * 
	 * @return
	 */
	public AssignListCmd popAssignList() {
		AssignListCmd assList = new AssignListCmd(
				cmdStack.toArray(new AssignCmd[0]));
		cmdStack.clear();
		return assList;
	}

	/**
	 * 
	 * @return
	 */
	public Expression[] popExpList() {
		List<Expression> list = new ArrayList<Expression>(listStack.poll());
		Collections.reverse(list);
		expStack = listStack.peek();
		return list.toArray(new Expression[0]);
	}

	/**
	 * 
	 * @param assignCmd
	 */
	public void pushAssign(AssignCmd assignCmd) {
		cmdStack.add(assignCmd);
	}

	/**
	 * 
	 */
	public void pushAssignList() {
		cmdStack.clear();
	}

	/**
	 * 
	 * @param expression
	 */
	public void pushExp(Expression expression) {
		expStack.offer(expression);
	}

	/**
	 */
	public void pushExpList() {
		expStack = Collections.asLifoQueue(new ArrayDeque<Expression>());
		listStack.offer(expStack);
	}

	/**
	 * 
	 * @param expression
	 */
	public void setCondition(Expression expression) {
		condition = expression;
	}

	/**
	 * @param documentLocator
	 *            The documentLocator to set.
	 */
	@Override
	public void setDocumentLocator(Locator documentLocator) {
		this.documentLocator = documentLocator;
	}

	/**
	 * @param elseConsequences
	 *            the elseConsequences to set
	 */
	public void setElseConsequences(AssignListCmd elseConsequences) {
		this.elseConsequences = elseConsequences;
	}

	/**
	 * @param thenConsequences
	 *            the thenConsequences to set
	 */
	public void setThenConsequences(AssignListCmd thenConsequences) {
		this.thenConsequences = thenConsequences;
	}

	/**
	 * @see org.xml.sax.ContentHandler#startElement(java.lang.String,
	 *      java.lang.String, java.lang.String, org.xml.sax.Attributes)
	 */
	@Override
	public void startElement(String uri, String localName, String qName,
			Attributes attributes) throws SAXException {
		name.setLength(0);
		ParseState parseState = stateMap.get(qName);
		if (parseState != null)
			parseState.start(attributes);
	}

	/**
	 * @see org.xml.sax.ErrorHandler#warning(org.xml.sax.SAXParseException)
	 */
	@Override
	public void warning(SAXParseException warning) throws SAXException {
		warning.printStackTrace();
		super.warning(warning);
	}
}