/*
 * Created on 1-feb-2005
 */
package org.mmarini.fuzzy.handlers;

import java.util.List;

import org.mmarini.fuzzy.IPredicate;
import org.mmarini.fuzzy.handlers.EvidenceListHandler;
import org.mmarini.fuzzy.handlers.SessionHandler;

import junit.framework.TestCase;

/**
 * @author US00852
 * @version $Id: EvidenceListHandlerTest.java,v 1.2 2005/02/10 22:32:36 marco
 *          Exp $
 */
public class EvidenceListHandlerTest extends TestCase {
	private static final int EVIDENCE_COUNT = 11;
	static final String RESOURCE_NAME = "/animals-rules.xml";

	SessionHandler session;
	EvidenceListHandler handler;

	/*
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		session = new SessionHandler();
		session.setResource(RESOURCE_NAME);
		assertSame(RESOURCE_NAME, session.getResource());
		session.analize();
		handler = new EvidenceListHandler(session);
	}

	public void testGetElements() {
		handler.selectElements();
		List list = handler.getElements();
		assertEquals(handler.getSubListSize(), list.size());
		assertEquals("albatro", ((IPredicate) list.get(0)).getName());
	}

	public void testGetSize() {
		handler.selectElements();
		assertEquals(EVIDENCE_COUNT, handler.getSize());
	}

	public void testSelectElements() {
		handler.selectElements();
		this.testGetSize();
	}

	public void testSetIndex() {
		handler.selectElements();
		List list = handler.getList();
		int idx = 1;
		handler.setIndex(idx);
		assertEquals(idx, handler.getIndex());
		List subList = handler.getElements();
		assertEquals(list.get(idx), subList.get(0));

		idx = 5;
		handler.setIndex(idx);
		assertEquals(idx, handler.getIndex());
		subList = handler.getElements();
		assertEquals(list.get(idx), subList.get(0));

		handler.setIndex(-1);
		assertEquals(0, handler.getIndex());

		handler.setIndex(1000);
		assertEquals(EVIDENCE_COUNT, handler.getIndex());
		assertEquals(0, handler.getElements().size());
	}

	public void testSetSubListSize() {
		handler.selectElements();

		int n = 0;
		handler.setSubListSize(n);
		assertEquals(n, handler.getSubListSize());
		assertEquals(n, handler.getElements().size());

		n = 1;
		handler.setSubListSize(n);
		assertEquals(n, handler.getSubListSize());
		assertEquals(n, handler.getElements().size());

		n = 10;
		handler.setSubListSize(n);
		assertEquals(n, handler.getSubListSize());
		assertEquals(n, handler.getElements().size());
	}

	public void testSkip() {
		handler.selectElements();
		handler.skip(1);
		assertEquals(1, handler.getIndex());

		handler.skip(5);
		assertEquals(6, handler.getIndex());

		handler.skip(-4);
		assertEquals(2, handler.getIndex());

		handler.skip(100);
		assertEquals(EVIDENCE_COUNT, handler.getIndex());

		handler.skip(-100);
		assertEquals(0, handler.getIndex());
	}
}