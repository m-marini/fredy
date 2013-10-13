package org.mmarini.fuzzy;

public class MockExecutionContext implements ExecutionContext {
	private FuzzyBoolean value;
	private String predicate;
	private int size;

	public MockExecutionContext() {
	}

	@Override
	public void assertFalse(String predicate) {
		this.predicate = predicate;
		value = value.not();
		--size;
	}

	@Override
	public void assertTrue(String predicate) {
		this.predicate = predicate;
		--size;
	}

	public String getPredicate() {
		return predicate;
	}

	/**
	 * @return the size
	 */
	public int getSize() {
		return size;
	}

	public FuzzyBoolean getValue() {
		return value;
	}

	@Override
	public FuzzyBoolean pop() {
		--size;
		return value;
	}

	@Override
	public void push(FuzzyBoolean value) {
		this.value = value;
		++size;
	}

	@Override
	public void push(String predicate) {
		this.predicate = predicate;
		value = FuzzyBoolean.UNKNOWN;
		++size;
	}
}
