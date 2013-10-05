package org.mmarini.fredy.handlers;

import java.io.IOException;
import java.io.Serializable;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;

import org.mmarini.fuzzy.Analisys;
import org.mmarini.fuzzy.EvidenceComparator;
import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.PostulateComparator;
import org.mmarini.fuzzy.parse.RulesParser;
import org.xml.sax.SAXException;

/**
 * @author US00852
 * @version $Id: SessionHandler.java,v 1.2 2005/02/10 22:32:37 marco Exp $
 */
public class SessionHandler implements Serializable {
	private Analisys analisys;
	private String resource;
	private EvidenceListHandler evidenceHandler = new EvidenceListHandler(this);
	private PostulateListHandler postulateHandler = new PostulateListHandler(
			this);
	private HypotesysListHandler hypotesysHandler = new HypotesysListHandler(
			this);

	/**
	 * 
	 */
	public SessionHandler() {
	}

	public void analize() throws FactoryConfigurationError,
			ParserConfigurationException, SAXException, IOException {
		this.getAnalisys().analize();
		this.getPostulateHandler().selectElements();
		this.getHypotesysHandler().selectElements();
		this.getEvidenceHandler().selectElements();
	}

	protected Analisys createAnalisys(String resource)
			throws FactoryConfigurationError, ParserConfigurationException,
			SAXException, IOException {
		Analisys analisys;
		URL url = this.getClass().getResource(resource);
		analisys = RulesParser.getInstance().parse(url.toExternalForm());
		return analisys;
	}

	public Analisys getAnalisys() throws FactoryConfigurationError,
			ParserConfigurationException, SAXException, IOException {
		if (analisys == null) {
			analisys = createAnalisys(this.getResource());
		}
		return analisys;
	}

	public List getEvidence() {
		List evidence = new ArrayList(analisys.getEvidence());
		Collections.sort(evidence, EvidenceComparator.getInstance());
		return evidence;
	}

	/**
	 * @return Returns the evidenceHandler.
	 */
	public EvidenceListHandler getEvidenceHandler() {
		return evidenceHandler;
	}

	public List getHypotesys() {
		List hypotesis = analisys.getHypotesys();
		Collections.sort(hypotesis, EvidenceComparator.getInstance());
		return hypotesis;
	}

	/**
	 * @return Returns the hypotesysHandler.
	 */
	public HypotesysListHandler getHypotesysHandler() {
		return hypotesysHandler;
	}

	public List getPostulate() {
		List postulate = analisys.getPostulate();
		Collections.sort(postulate, PostulateComparator.getInstance());
		return postulate;
	}

	/**
	 * @return Returns the postulateHandler.
	 */
	public PostulateListHandler getPostulateHandler() {
		return postulateHandler;
	}

	public String getResource() {
		return resource;
	}

	public void gotoFirstEvidencePage() {
		this.gotoFirstPage(this.getEvidenceHandler());
	}

	public void gotoFirstHypotesysPage() {
		this.gotoFirstPage(this.getHypotesysHandler());
	}

	/**
	 * Goes to the first page
	 * 
	 * @param handler
	 *            the ValueList
	 */
	protected void gotoFirstPage(IValueList handler) {
		handler.setIndex(0);
	}

	public void gotoFirstPostulatePage() {
		this.gotoFirstPage(this.getPostulateHandler());
	}

	public void gotoLastEvidencePage() {
		this.gotoLastPage(this.getEvidenceHandler());
	}

	public void gotoLastHypotesysPage() {
		this.gotoLastPage(this.getHypotesysHandler());
	}

	/**
	 * Goes to the last page
	 * 
	 * @param handler
	 *            the ValueList
	 */
	protected void gotoLastPage(IValueList handler) {
		handler.setIndex((handler.getPageCount() - 1)
				* handler.getSubListSize());
	}

	public void gotoLastPostulatePage() {
		this.gotoLastPage(this.getPostulateHandler());
	}

	public void gotoNextEvidencePage() {
		this.gotoNextPage(this.getEvidenceHandler());
	}

	public void gotoNextHypotesysPage() {
		this.gotoNextPage(this.getHypotesysHandler());
	}

	/**
	 * Goes to the next page
	 * 
	 * @param handler
	 *            the ValueList
	 */
	protected void gotoNextPage(IValueList handler) {
		handler.skip(handler.getSubListSize());
	}

	/**
	 * 
	 */
	public void gotoNextPostulatePage() {
		this.gotoNextPage(this.getPostulateHandler());
	}

	public void gotoPreviousEvidencePage() {
		this.gotoPrevPage(this.getEvidenceHandler());
	}

	public void gotoPreviousHypotesysPage() {
		this.gotoPrevPage(this.getHypotesysHandler());
	}

	public void gotoPreviousPostulatePage() {
		this.gotoPrevPage(this.getPostulateHandler());
	}

	/**
	 * Goes to the previous page
	 * 
	 * @param handler
	 *            the ValueList
	 */
	protected void gotoPrevPage(IValueList handler) {
		handler.skip(-handler.getSubListSize());
	}

	/**
	 * @param analisys
	 *            The analisys to set.
	 */
	public void setAnalisys(Analisys analisys) {
		this.analisys = analisys;
	}

	public void setResource(String resource) {
		this.resource = resource;
	}

	/**
	 * @param name
	 * @param d
	 * @throws IOException
	 * @throws SAXException
	 * @throws ParserConfigurationException
	 * @throws FactoryConfigurationError
	 */
	public void setValue(String name, double value)
			throws FactoryConfigurationError, ParserConfigurationException,
			SAXException, IOException {
		this.getAnalisys().getPredicate(name).setValue(new FuzzyBoolean(value));
	}
}