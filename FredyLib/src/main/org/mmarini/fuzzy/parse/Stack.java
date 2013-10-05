package org.mmarini.fuzzy.parse;

import java.util.ArrayList;

/**
 * @author US00852
 * @version $Id: Stack.java,v 1.2 2005/02/10 22:32:38 marco Exp $
 */
public class Stack extends ArrayList implements IStack {
	@Override
	public Object pop() {
		int index = this.size();
		Object obj = this.get(index);
		remove(index);
		return obj;
	}

	@Override
	public void push(Object obj) {
		this.add(obj);
	}
}