/*
 * Copyright (c) 2023 Marco Marini, marco.marini@mmarini.org
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 *    END OF TERMS AND CONDITIONS
 */

package org.mmarini.fredy2.model;

import java.util.Collection;
import java.util.Objects;
import java.util.StringJoiner;

import static java.util.Objects.requireNonNull;

/**
 * Gets the value, id, max hypothesis truth and change, dependant unknown hypothesis of axiom
 */
public class AxiomStatus extends PredicateStatus {
    private final double maxHypothesis;
    private final Collection<String> hypothesis;

    /**
     * Creates the predicate status
     *
     * @param id            the identifier
     * @param truth         the value
     * @param maxHypothesis the maximum hypothesis truth on changes
     * @param hypothesis    the dependant unknown hypothesis
     */
    public AxiomStatus(String id, double truth, double maxHypothesis, Collection<String> hypothesis) {
        super(id, truth);
        this.maxHypothesis = maxHypothesis;
        this.hypothesis = requireNonNull(hypothesis);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        AxiomStatus that = (AxiomStatus) o;
        return Double.compare(that.maxHypothesis, maxHypothesis) == 0 && hypothesis.equals(that.hypothesis);
    }

    /**
     * Returns the dependant unknown hypothesis
     */
    public Collection<String> getHypothesis() {
        return hypothesis;
    }

    /**
     * Returns the maximum hypothesis truth on changes
     */
    public double getMaxHypothesis() {
        return maxHypothesis;
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), maxHypothesis, hypothesis);
    }

    /**
     * Returns the axiom with truth changed
     *
     * @param truth the truth
     */
    public AxiomStatus setTruth(double truth) {
        return new AxiomStatus(id, truth, maxHypothesis, hypothesis);
    }

    @Override
    public String toString() {
        return new StringJoiner(", ", AxiomStatus.class.getSimpleName() + "[", "]")
                .add("id=" + getId())
                .add("truth=" + getTruth())
                .add("maxHypothesis=" + maxHypothesis)
                .add("hypothesis=" + hypothesis)
                .toString();
    }
}
