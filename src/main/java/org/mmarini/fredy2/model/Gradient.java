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

import org.mmarini.Tuple2;

import java.util.HashMap;
import java.util.Map;

/**
 * Manages the gradient vector components
 */
public class Gradient {
    /**
     * Returns the empty gradient
     */
    public static Gradient empty() {
        return new Gradient(new HashMap<>());
    }

    private final Map<Tuple2<String, String>, Double> vector;

    /**
     * Creates the gradients
     *
     * @param vector the vector components
     */
    protected Gradient(Map<Tuple2<String, String>, Double> vector) {
        this.vector = vector;
    }

    /**
     * Returns the gradient component
     *
     * @param up   the upper identifier
     * @param down the down identifier
     */
    public double get(String up, String down) {
        return vector.getOrDefault(Tuple2.of(up, down), 0d);
    }

    /**
     * Returns the gradient with set vector component
     *
     * @param up    the upper identifier
     * @param down  the down identifier
     * @param value the value
     */
    public Gradient put(String up, String down, double value) {
        vector.put(Tuple2.of(up, down), value);
        return this;
    }
}
