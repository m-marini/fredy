package org.mmarini.fuzzy.parse;

import java.util.List;

/**
 * @author US00852
 * @version $Id: IStack.java,v 1.2 2005/02/10 22:32:38 marco Exp $
 */
public interface IStack extends List {
	public abstract Object pop();

	public abstract void push(Object obj);
}
