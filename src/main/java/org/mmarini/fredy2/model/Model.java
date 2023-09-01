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

import com.fasterxml.jackson.databind.JsonNode;
import org.mmarini.Tuple2;
import org.mmarini.yaml.schema.Locator;
import org.mmarini.yaml.schema.Validator;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.lang.Math.max;
import static java.lang.String.format;
import static java.util.Objects.requireNonNull;
import static org.mmarini.Utils.zipWithIndex;
import static org.mmarini.yaml.schema.Validator.objectAdditionalProperties;

/**
 * Computes the model status from hypothesis
 */
public class Model {
    public static final Validator JSON_SPEC = objectAdditionalProperties(InferenceNode.JSON_SPEC);
    public static final double UNKNOWN_VALUE = 0.5;

    /**
     * Returns the comparator of two predicate
     */
    static final Comparator<PredicateStatus> PREDICATE_STATUS_COMPARATOR = Comparator.<PredicateStatus>comparingDouble(x -> {
                double truth = x.getTruth();
                return truth != UNKNOWN_VALUE ? truth : -2;
            })
            .reversed()
            .thenComparing(PredicateStatus::getId);

    /**
     * Returns the comparator of two axioms
     */
    static final Comparator<AxiomStatus> AXIOM_STATUS_COMPARATOR =
            Comparator.<AxiomStatus>comparingDouble(x -> {
                        double truth = x.getTruth();
                        return truth != UNKNOWN_VALUE ? truth : 2;
                    })
                    .thenComparingDouble(a ->
                            a.getTruth() == UNKNOWN_VALUE ? a.getMaxHypothesis() : 0d
                    )
                    .thenComparingInt(a ->
                            a.getTruth() == UNKNOWN_VALUE
                                    ? a.getHypothesis().size()
                                    : 0)
                    .reversed()
                    .thenComparing(AxiomStatus::getId);

    /**
     * Returns the model from assertions
     *
     * @param assertions the assignment
     */
    public static Model create(Map<String, InferenceNode> assertions) {
        Map<String, Collection<String>> dependencies = createDependencies(assertions);
        Map<String, Collection<String>> closure = extractReverseClosure(dependencies);
        // check for cycle tree
        validate(closure);
        Collection<String> hypothesis = findHypothesis(dependencies);
        Collection<String> inferences = dependencies.keySet().stream()
                .filter(id -> !hypothesis.contains(id))
                .collect(Collectors.toSet());
        Collection<String> axioms = dependencies.values().stream()
                .flatMap(Collection::stream)
                .filter(id -> !(hypothesis.contains(id) || inferences.contains(id)))
                .collect(Collectors.toSet());
        Map<String, Collection<String>> hypothesisByAxiom = extractHypothesisByAxiom(closure, axioms, hypothesis);
        return new Model(assertions, hypothesis, inferences, axioms, hypothesisByAxiom);
    }

    /**
     * Returns the closure matrix of nodes
     *
     * @param dependencies the dependencies
     * @param nodes        the nodes
     */
    private static boolean[][] createClosure(Map<String, Collection<String>> dependencies, List<String> nodes) {
        int n = nodes.size();
        boolean[][] matrix = new boolean[n][n];
        // Generate adjacent matrix
        zipWithIndex(nodes)
                .flatMap(ti -> zipWithIndex(nodes).map(tj -> Tuple2.of(ti, tj)))
                .forEach(t -> {
                    int i = t._1._1;
                    int j = t._2._1;
                    String ni = t._1._2;
                    String nj = t._2._2;
                    if (dependencies.containsKey(ni) && dependencies.get(ni).contains(nj)) {
                        matrix[j][i] = true;
                    }
                });
        // Warshall Floyd
        for (int k = 0; k < n; k++) {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < n; j++) {
                    matrix[i][j] = matrix[i][j]
                            || matrix[i][k] && matrix[k][j];
                }
            }
        }
        return matrix;
    }

    /**
     * Creates dependencies
     *
     * @param assertions the assertions
     */
    static Map<String, Collection<String>> createDependencies(Map<String, InferenceNode> assertions) {
        return Tuple2.stream(assertions)
                .map(t -> t.setV2((Collection<String>) t._2.getDependencies()
                        .collect(Collectors.toSet())))
                .collect(Tuple2.toMap());
    }

    /**
     * Returns the hypothesis by axiom
     *
     * @param closure the reverse closure
     */

    static Map<String, Collection<String>> extractHypothesisByAxiom(Map<String, Collection<String>> closure,
                                                                    Collection<String> axioms, Collection<String> hypothesis) {
        return axioms.stream().map(axiom -> Tuple2.of(axiom,
                (Collection<String>) closure.get(axiom).stream()
                        .filter(hypothesis::contains)
                        .collect(Collectors.toList())
        )).collect(Tuple2.toMap());
    }

    /**
     * Returns the dependent assertions by predicate
     *
     * @param dependencies the dependencies
     */
    static Map<String, Collection<String>> extractReverseClosure(Map<String, Collection<String>> dependencies) {
        List<String> nodes = Stream.concat(
                        dependencies.keySet().stream(),
                        dependencies.values().stream()
                                .flatMap(Collection::stream))
                .distinct()
                .sorted()
                .collect(Collectors.toList());
        // Creates closure matrix
        boolean[][] closure = createClosure(dependencies, nodes);
        // Extract reverse closure dependencies (antecedent -> consequent)
        return zipWithIndex(nodes)
                .map(antecedent -> Tuple2.of(antecedent._2,
                        (Collection<String>) zipWithIndex(nodes)
                                .filter(consequent -> closure[antecedent._1][consequent._1])
                                .map(Tuple2::getV2)
                                .collect(Collectors.toList()))
                )
                .collect(Tuple2.toMap());
    }

    /**
     * Returns the hypothesis identifiers from dependencies
     *
     * @param dependencies the dependencies
     */
    static Collection<String> findHypothesis(Map<String, Collection<String>> dependencies) {
        Set<String> nodes = dependencies.values().stream()
                .flatMap(Collection::stream)
                .collect(Collectors.toSet());
        return dependencies.keySet().stream()
                .filter(id -> !nodes.contains(id))
                .collect(Collectors.toSet());
    }

    /**
     * Returns the model by json spec
     *
     * @param root    the document root
     * @param locator the locator
     */
    static Model fromJson(JsonNode root, Locator locator) {
        JSON_SPEC.apply(locator).accept(root);
        Map<String, InferenceNode> assertions =
                locator.propertyNames(root)
                        .map(t -> {
                            String id = t._1;
                            InferenceNode expression = InferenceNode.fromJson(root, t._2);
                            return Tuple2.of(id, expression);
                        })
                        .collect(Tuple2.toMap());
        return create(assertions);
    }

    /**
     * Validates the reverse closure
     *
     * @param closure the reverse closure
     * @throws IllegalArgumentException if dependencies are not valid
     */
    static void validate(Map<String, Collection<String>> closure) {
        List<String> cycleNodes = Tuple2.stream(closure).filter(t ->
                        t._2.contains(t._1)
                ).map(Tuple2::getV1)
                .collect(Collectors.toList());
        if (!cycleNodes.isEmpty()) {
            String cyclesStr = cycleNodes.stream()
                    .sorted()
                    .collect(Collectors.joining(", "));
            throw new IllegalArgumentException(format("Cycles on nodes [%s]",
                    cyclesStr));
        }
    }

    private final Map<String, InferenceNode> assertions;
    private final Collection<String> hypothesis;
    private final Collection<String> inferences;
    private final Collection<String> axioms;
    private final Map<String, Collection<String>> hypothesisByAxiom;

    /**
     * Creates the model
     *
     * @param assertions        the assertions
     * @param hypothesis        the hypothesis
     * @param inferences        the inferences identifiers
     * @param axioms            the axiom identifiers
     * @param hypothesisByAxiom yhe map of hypothesis by axiom
     */
    protected Model(Map<String, InferenceNode> assertions, Collection<String> hypothesis, Collection<String> inferences,
                    Collection<String> axioms, Map<String, Collection<String>> hypothesisByAxiom) {
        this.hypothesis = requireNonNull(hypothesis);
        this.assertions = requireNonNull(assertions);
        this.inferences = requireNonNull(inferences);
        this.axioms = requireNonNull(axioms);
        this.hypothesisByAxiom = requireNonNull(hypothesisByAxiom);
    }

    /**
     * Returns the axioms only values
     *
     * @param evidences the evidences
     */
    public Map<String, Double> createOnlyAxioms(Map<String, Double> evidences) {
        return new HashMap<>(axioms.stream().map(id ->
                Tuple2.of(id, evidences.getOrDefault(id, UNKNOWN_VALUE))
        ).collect(Tuple2.toMap()));
    }

    /**
     * Returns the axioms evidences in unknown state
     */
    public Map<String, Double> createUnknownAxioms() {
        return axioms.stream().map(id -> Tuple2.of(id, UNKNOWN_VALUE))
                .collect(Tuple2.toMap());
    }

    /**
     * Returns the evaluations of a predicate and stores it in evidences
     *
     * @param id        the predicate identifier
     * @param evidences the evidences
     */
    public double evaluate(String id, Map<String, Double> evidences) {
        Double value = evidences.get(id);
        if (value != null) {
            return value;
        }
        InferenceNode expression = assertions.get(id);
        double expValue = expression == null
                ? UNKNOWN_VALUE
                : expression.evaluate(this, evidences);
        evidences.put(id, expValue);
        return expValue;
    }

    /**
     * Returns the assertions
     */
    public Map<String, InferenceNode> getAssertions() {
        return assertions;
    }

    /**
     * Returns the axiom identifiers
     */
    public Collection<String> getAxioms() {
        return axioms;
    }

    /**
     * Returns the evidences sorted by gradient
     *
     * @param evidences the evidences
     */
    public List<AxiomStatus> getAxioms(Map<String, Double> evidences) {
        Map<String, Double> maxTruths = getMaxTruthByAxiomsExtremes(evidences);
        Map<String, Collection<String>> unknownHypothesis = getUnknownHypothesis(evidences);
        return axioms.stream().map(id -> new AxiomStatus(
                        id,
                        evidences.getOrDefault(id, UNKNOWN_VALUE),
                        maxTruths.getOrDefault(id, 0d),
                        unknownHypothesis.getOrDefault(id, List.of())
                ))
                .sorted(AXIOM_STATUS_COMPARATOR)
                .collect(Collectors.toList());
    }

    /**
     * Returns the hypothesis identifiers
     */
    public Collection<String> getHypothesis() {
        return hypothesis;
    }

    /**
     * Returns the sorted hypothesis status
     *
     * @param evidences the evidences
     */
    public List<PredicateStatus> getHypothesis(Map<String, Double> evidences) {
        return hypothesis.stream().map(id -> new PredicateStatus(
                        id, evidences.getOrDefault(id, UNKNOWN_VALUE)))
                .sorted(PREDICATE_STATUS_COMPARATOR)
                .collect(Collectors.toList());
    }

    /**
     * Returns the sorted inferences status
     *
     * @param evidences the evidences
     */
    public List<PredicateStatus> getInferences(Map<String, Double> evidences) {
        return inferences.stream().map(id -> new PredicateStatus(
                        id, evidences.getOrDefault(id, UNKNOWN_VALUE)))
                .sorted(PREDICATE_STATUS_COMPARATOR)
                .collect(Collectors.toList());
    }

    /**
     * Returns the inference identifiers
     */
    public Collection<String> getInferences() {
        return inferences;
    }

    /**
     * Returns the max level of hypothesis truth by axiom variation
     *
     * @param evidences the evidences
     */
    public Map<String, Double> getMaxTruthByAxiomsExtremes(Map<String, Double> evidences) {
        return axioms.stream().map(axiom -> {
                    // Computes the negated evidences
                    Map<String, Double> negatedAxioms = createOnlyAxioms(evidences);
                    negatedAxioms.put(axiom, 0d);
                    Map<String, Double> negationEvidences = infer(negatedAxioms);

                    // Computes the asserted evidences
                    Map<String, Double> assertedAxioms = createOnlyAxioms(evidences);
                    assertedAxioms.put(axiom, 1d);
                    Map<String, Double> assertionEvidences = infer(assertedAxioms);

                    // Extracts the maximum value between sum of negated hypothesis and asserted hypothesis
                    double value = hypothesis.stream().map(hyp -> {
                                // Extracts the negation hypotheses value
                                double negationValue = negationEvidences.getOrDefault(hyp, 0d);
                                // Extracts the assertion hypotheses value
                                double assertionValue = assertionEvidences.getOrDefault(hyp, 0d);
                                return new double[]{negationValue, assertionValue};
                            }).reduce((a, b) ->
                                    // Sum among all hypothesis
                                    new double[]{a[0] + b[0], a[1] + b[1]})
                            .map(a ->
                                    // Extract the maximum
                                    max(a[0], a[1]))
                            .orElse(0d);
                    return Tuple2.of(axiom, value);
                })
                .collect(Tuple2.toMap());
    }

    /**
     * Returns the dependant unknown hypothesis by axiom
     *
     * @param evidences the evidences
     */
    Map<String, Collection<String>> getUnknownHypothesis(Map<String, Double> evidences) {
        return Tuple2.stream(hypothesisByAxiom).map(t -> {
                    Collection<String> hypothesis = t._2.stream()
                            .filter(hyp -> evidences.getOrDefault(hyp, UNKNOWN_VALUE) == UNKNOWN_VALUE)
                            .collect(Collectors.toList());
                    return Tuple2.of(t._1, hypothesis);
                })
                .collect(Tuple2.toMap());
    }

    /**
     * Returns the sorted results of inference model
     *
     * @param facts the facts
     */
    public Map<String, Double> infer(Map<String, Double> facts) {
        assertions.keySet().forEach(id -> evaluate(id, facts));
        return facts;
    }
}
