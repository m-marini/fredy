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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.stream.Collectors;

import static java.lang.Math.abs;
import static java.util.Objects.requireNonNull;

/**
 * Gets the predicate values
 */
public class Evidences {

    public static final double UNKNOWN_VALUE = 0.5;
    private static final Logger logger = LoggerFactory.getLogger(Evidences.class);

    /**
     * Returns the empty evidences
     */
    public static Evidences empty() {
        return new Evidences(new HashMap<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>());
    }

    private final Map<String, Double> evidences;
    private final List<String> hypothesis;
    private final List<String> inferences;
    private final List<String> axioms;

    /**
     * Creates the evidences
     *
     * @param evidences  the evidences values
     * @param hypothesis the hypothesis
     * @param inferences the inferences
     * @param axioms     the axioms
     */
    protected Evidences(Map<String, Double> evidences, List<String> hypothesis, List<String> inferences, List<String> axioms) {
        this.evidences = requireNonNull(evidences);
        this.hypothesis = requireNonNull(hypothesis);
        this.inferences = requireNonNull(inferences);
        this.axioms = requireNonNull(axioms);
    }

    /**
     * Creates the evidences by copy
     *
     * @param evidences the evidences
     */
    protected Evidences(Evidences evidences) {
        this.evidences = new HashMap<>(evidences.evidences);
        this.hypothesis = new ArrayList<>(evidences.hypothesis);
        this.inferences = new ArrayList<>(evidences.inferences);
        this.axioms = new ArrayList<>(evidences.axioms);
    }

    /**
     * Clears the assertions value
     */
    public Evidences clearAssertions() {
        hypothesis.forEach(evidences::remove);
        inferences.forEach(evidences::remove);
        return this;
    }

    /**
     * Returns true if evidences contains the predicate value
     *
     * @param id the evidence identifier
     */
    public boolean contains(String id) {
        return evidences.containsKey(id);
    }

    /**
     * Returns a copy of evidences
     */
    public Evidences copy() {
        return new Evidences(this);
    }

    /**
     * Returns the value of a predicate
     *
     * @param id the predicate identifier
     */
    public double get(String id) {
        Double value = evidences.get(id);
        return value != null ? value : UNKNOWN_VALUE;
    }

    /**
     * Returns the axioms
     */
    public List<String> getAxioms() {
        return axioms;
    }

    /**
     * Returns the evidences with the axioms
     *
     * @param axioms the axioms
     */
    public Evidences setAxioms(List<String> axioms) {
        this.axioms.clear();
        this.axioms.addAll(axioms);
        return this;
    }

    /**
     * Returns the list of axioms values order by certainty
     */
    public List<Tuple2<String, Double>> getAxiomsList() {
        return axioms.stream().map(id -> Tuple2.of(id, get(id)))
                .sorted(Comparator.comparingDouble(Tuple2::getV2))
                .collect(Collectors.toList());
    }

    /**
     * Returns the hypothesis
     */
    public List<String> getHypothesis() {
        return hypothesis;
    }

    /**
     * Returns the evidences with the hypothesis
     *
     * @param hypothesis the hypothesis
     */
    public Evidences setHypothesis(Set<String> hypothesis) {
        this.hypothesis.clear();
        this.hypothesis.addAll(hypothesis);
        return this;
    }

    /**
     * Returns the list of hypothesis values order by certainty
     */
    public List<Tuple2<String, Double>> getHypothesisList() {
        Comparator<Tuple2<String, Double>> comparator = Comparator.<Tuple2<String, Double>>comparingDouble(t -> abs(t._2 - 0.5))
                .reversed()
                .thenComparing(Comparator.<Tuple2<String, Double>>comparingDouble(Tuple2::getV2).reversed())
                .thenComparing(Tuple2::getV1);
        return hypothesis.stream()
                .map(id -> Tuple2.of(id, evidences.get(id)))
                .sorted(comparator)
                .collect(Collectors.toList());
    }

    /**
     * Returns the inferences
     */
    public List<String> getInferences() {
        return inferences;
    }

    /**
     * Returns the evidences with the inferences
     *
     * @param inferences the inferences
     */
    public Evidences setInferences(Set<String> inferences) {
        this.inferences.clear();
        this.inferences.addAll(inferences);
        return this;
    }

    /**
     * Returns the list of inferences values order by certainty
     */
    public List<Tuple2<String, Double>> getInferencesList() {
        Comparator<Tuple2<String, Double>> comparator = Comparator.<Tuple2<String, Double>>comparingDouble(t -> abs(t._2 - 0.5))
                .reversed()
                .thenComparing(Comparator.<Tuple2<String, Double>>comparingDouble(Tuple2::getV2).reversed())
                .thenComparing(Tuple2::getV1);
        return inferences.stream()
                .map(id -> Tuple2.of(id, evidences.get(id)))
                .sorted(comparator)
                .collect(Collectors.toList());
    }

    /**
     * Returns the evidences with the set value of the evidence
     *
     * @param id    the evidence identifier
     * @param value the value
     */
    public Evidences put(String id, double value) {
        evidences.put(id, value);
        return this;
    }
}
