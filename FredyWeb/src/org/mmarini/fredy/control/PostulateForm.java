package org.mmarini.fredy.control;

import java.util.HashMap;
import java.util.Map;

import org.apache.struts.action.ActionForm;

/**
 * @author US00852
 * @version $Id: PostulateForm.java,v 1.2 2005/02/10 22:32:35 marco Exp $
 */
public class PostulateForm extends ActionForm {
	private Map postulateMap = new HashMap();

	/**
	 * Gets the postulate
	 * 
	 * @param key
	 *            the index
	 * @return the value
	 */
	public Object getPostulate(Object key) {
		return this.getPostulateMap().get(key);
	}

	/**
	 * @return Returns the pustulateMap.
	 */
	public Map getPostulateMap() {
		return postulateMap;
	}

	/**
	 * Sets the value
	 * 
	 * @param key
	 *            the index
	 * @param value
	 *            the value
	 */
	public void setPostulate(Object key, Object value) {
		this.getPostulateMap().put(key, value);
	}
}