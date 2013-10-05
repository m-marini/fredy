package org.mmarini.fuzzy;

/**
 * @author US00852
 * @version $Id: IExpressionVisitor.java,v 1.2 2005/02/10 22:32:36 marco Exp $
 */
public interface IExpressionVisitor {

	public void visitPredicate(IPredicate predicate);
}