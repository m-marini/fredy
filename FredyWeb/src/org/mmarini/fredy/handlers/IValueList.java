package org.mmarini.fredy.handlers;

import java.util.List;

/**
 * @author US00852
 * @version $Id: IValueList.java,v 1.2 2005/02/10 22:32:37 marco Exp $
 */
public interface IValueList {

	/**
	 * Gets the elements
	 * 
	 * @return the elements list
	 */
	public abstract List getElements();

	/**
	 * Return the current index
	 * 
	 * @return the current index
	 */
	public abstract int getIndex();

	/**
	 * Gets the page count
	 * 
	 * @return the page count
	 */
	public abstract int getPageCount();

	/**
	 * Gets the page number
	 * 
	 * @return the page number
	 */
	public abstract int getPageNumber();

	/**
	 * Gets the number of elements
	 * 
	 * @return the number of elements
	 */
	public abstract int getSize();

	/**
	 * Gets the sublist size
	 * 
	 * @return the sublist size
	 */
	public abstract int getSubListSize();

	/**
	 * Sets the current index
	 * 
	 * @param index
	 *            the index
	 */
	public abstract void setIndex(int index);

	/**
	 * Sets the sublist size
	 * 
	 * @param size
	 *            the sublist size
	 */
	public abstract void setSubListSize(int size);

	/**
	 * Skip elements
	 * 
	 * @param count
	 *            the number of element to skip &lt; 0 if backword
	 */
	public abstract void skip(int count);
}