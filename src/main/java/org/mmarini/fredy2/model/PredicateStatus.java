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

import java.util.Objects;
import java.util.StringJoiner;

import static java.util.Objects.requireNonNull;

/**
 * Gets the value and id of predicate
 */
public class PredicateStatus {
    protected final String id;
    protected final double truth;

    /**
     * Creates the predicate status
     *
     * @param id    the identifier
     * @param truth the value
     */
    public PredicateStatus(String id, double truth) {
        this.id = requireNonNull(id);
        this.truth = truth;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        PredicateStatus that = (PredicateStatus) o;
        return Double.compare(that.truth, truth) == 0 && id.equals(that.id);
    }

    /**
     * Returns the identifier
     */
    public String getId() {
        return id;
    }

    /**
     * Returns the truth of predicate
     */
    public double getTruth() {
        return truth;
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, truth);
    }

    @Override
    public String toString() {
        return new StringJoiner(", ", PredicateStatus.class.getSimpleName() + "[", "]")
                .add("id='" + id + "'")
                .add("truth=" + truth)
                .toString();
    }
}
