package org.mmarini.fuzzy.parse;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.mmarini.fuzzy.Analisys;
import org.mmarini.fuzzy.IPredicate;
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

	public static final String ASSIGN_NOT = "assign-not";
	public static final String ASSIGN = "assign";
	public static final String RULES = "rules";
	public static final String THEN = "then";
	public static final String ELSE = "else";
	public static final String IMPLIES = "implies";
	public static final String IFONLYIF = "ifonlyif";
	public static final String OR = "or";
	public static final String AND = "and";
	public static final String KNOWN = "known";
	public static final String SOMEWHAT = "somewhat";
	public static final String VERY = "very";
	public static final String NOT = "not";
	public static final String PREDICATE = "predicate";
	public static final String IF = "if";
	public static final String RULE = "rule";

	private Locator documentLocator;
	private Analisys analisys;
	private Rule rule;
	private IStack state = new Stack();
	private IPredicate predicate;
	private List expression = new ArrayList(1);
	private List assignExpression = new ArrayList(1);
	private StringBuffer name = new StringBuffer();
	private Map stateMap = new HashMap();

	/**
	 * 
	 */
	public RulesHandler() {
		Map map = this.getStateMap();
		map.put(RULES, RulesState.class);
		map.put(RULE, RuleState.class);
		map.put(IF, IfState.class);
		map.put(AND, AndState.class);
		map.put(PREDICATE, PredicateState.class);
		map.put(OR, OrState.class);
		map.put(IFONLYIF, IfOnlyIfState.class);
		map.put(IMPLIES, ImpliesState.class);
		map.put(SOMEWHAT, SomewhatState.class);
		map.put(KNOWN, KnownState.class);
		map.put(VERY, VeryState.class);
		map.put(NOT, NotState.class);
		map.put(THEN, ThenState.class);
		map.put(ASSIGN_NOT, AssignNotState.class);
		map.put(ASSIGN, AssignState.class);
		map.put(ELSE, ElseState.class);
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
		this.pop().end(this);
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
	 * @return Returns the analisys.
	 */
	public Analisys getAnalisys() {
		return analisys;
	}

	/**
	 * @return Returns the assignExpression.
	 */
	public List getAssignExpression() {
		return assignExpression;
	}

	/**
	 * @return Returns the documentLocator.
	 */
	public Locator getDocumentLocator() {
		return documentLocator;
	}

	/**
	 * @return Returns the expression.
	 */
	public List getExpression() {
		return expression;
	}

	/**
	 * @return Returns the name.
	 */
	protected StringBuffer getName() {
		return name;
	}

	/**
	 * @return Returns the assignExpression.
	 */
	protected IPredicate getPredicate() {
		return predicate;
	}

	/**
	 * @return Returns the rule.
	 */
	protected Rule getRule() {
		return rule;
	}

	/**
	 * @return Returns the expression.
	 */
	protected List getState() {
		return state;
	}

	/**
	 * @return Returns the stateMap.
	 */
	protected Map getStateMap() {
		return stateMap;
	}

	protected IParseState pop() {
		List stack = this.getState();
		int index = stack.size() - 1;
		IParseState state = (IParseState) stack.get(index);
		stack.remove(index);
		return state;
	}

	protected void push(IParseState exp) {
		this.getState().add(exp);
	}

	/**
	 * @param analisys
	 *            The analisys to set.
	 */
	protected void setAnalisys(Analisys analisys) {
		this.analisys = analisys;
	}

	/**
	 * @param assignExpression
	 *            The assignExpression to set.
	 */
	public void setAssignExpression(IStack assignExpression) {
		this.assignExpression = assignExpression;
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
	 * @param expression
	 *            The expression to set.
	 */
	public void setExpression(IStack expression) {
		this.expression = expression;
	}

	/**
	 * @param assignExpression
	 *            The assignExpression to set.
	 */
	protected void setPredicate(IPredicate predicate) {
		this.predicate = predicate;
	}

	/**
	 * @param rule
	 *            The rule to set.
	 */
	protected void setRule(Rule rule) {
		this.rule = rule;
	}

	/**
	 * @see org.xml.sax.ContentHandler#startElement(java.lang.String,
	 *      java.lang.String, java.lang.String, org.xml.sax.Attributes)
	 */
	@Override
	public void startElement(String uri, String localName, String qName,
			Attributes attributes) throws SAXException {

		Class stateClass = (Class) this.getStateMap().get(qName);
		if (stateClass == null)
			throw new SAXParseException("Unknown element \"" + qName + "\"",
					this.getDocumentLocator());
		IParseState state;
		try {
			state = (IParseState) stateClass.newInstance();
		} catch (InstantiationException e) {
			throw new SAXException(e);
		} catch (IllegalAccessException e) {
			throw new SAXException(e);
		}
		this.push(state);
		state.start(this);
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