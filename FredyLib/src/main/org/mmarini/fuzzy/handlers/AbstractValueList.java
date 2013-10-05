package org.mmarini.fuzzy.handlers;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * @author US00852
 * @version $Id: AbstractValueList.java,v 1.2 2005/02/10 22:32:37 marco Exp $
 */
public abstract class AbstractValueList implements IValueList, Serializable {
	private int index;
	private int subListSize = 10;
	private List list = new ArrayList(0);

	/**
	 * @see org.mmarini.fuzzy.handlers.IValueList#getElements()
	 */
	@Override
	public List getElements() {
		int from = this.getIndex();
		int to = from + this.getSubListSize();
		int n = this.getSize();
		if (to > n)
			to = n;
		return this.getList().subList(from, to);
	}

	/**
	 * @see org.mmarini.fuzzy.handlers.IValueList#getIndex()
	 */
	@Override
	public int getIndex() {
		return index;
	}

	/**
	 * Gets the list
	 * 
	 * @return the list.
	 */
	public List getList() {
		return list;
	}

	/**
	 * @see org.mmarini.fuzzy.handlers.IValueList#getPageCount()
	 */
	@Override
	public int getPageCount() {
		return (this.getSize() + this.getSubListSize() - 1)
				/ this.getSubListSize();
	}

	/**
	 * @see org.mmarini.fuzzy.handlers.IValueList#getPageNumber()
	 */
	@Override
	public int getPageNumber() {
		return this.getIndex() / this.getSubListSize() + 1;
	}

	/**
	 * @see org.mmarini.fuzzy.handlers.IValueList#getSize()
	 */
	@Override
	public int getSize() {
		return this.getList().size();
	}

	/**
	 * @see org.mmarini.fuzzy.handlers.IValueList#getSubListSize()
	 */
	@Override
	public int getSubListSize() {
		return subListSize;
	}

	/**
	 * Selects the elements of list
	 */
	public abstract void selectElements();

	/**
	 * @see org.mmarini.fuzzy.handlers.IValueList#setIndex(int)
	 */
	@Override
	public void setIndex(int index) {
		if (index < 0)
			index = 0;
		int n = this.getSize();
		if (index > n)
			index = n;
		this.index = index;
	}

	/**
	 * Sets the list
	 * 
	 * @param list
	 *            the list to set.
	 */
	protected void setList(List list) {
		this.list = list;
	}

	/**
	 * @see org.mmarini.fuzzy.handlers.IValueList#setSubListSize(int)
	 */
	@Override
	public void setSubListSize(int size) {
		subListSize = size;
	}

	/**
	 * @see org.mmarini.fuzzy.handlers.IValueList#skip(int)
	 */
	@Override
	public void skip(int count) {
		this.setIndex(this.getIndex() + count);
	}
}