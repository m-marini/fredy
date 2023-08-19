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
import org.mmarini.yaml.Utils;
import org.mmarini.yaml.schema.Locator;
import org.mmarini.yaml.schema.Validator;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.lang.Math.abs;
import static java.lang.String.format;
import static java.util.Objects.requireNonNull;
import static org.mmarini.Utils.zipWithIndex;
import static org.mmarini.yaml.schema.Validator.objectAdditionalProperties;

/**
 * Computes the model status from hypothesis
 */
public class Model {
    public static final Validator JSON_SPEC = objectAdditionalProperties(InferenceNode.JSON_SPEC);
    private static final double DX = 1e-6;

    /**
     * Returns the model from assertions
     *
     * @param assertions the assignment
     */
    public static Model create(Assertion... assertions) {
        return create(List.of(assertions));
    }

    /**
     * Returns the model from assertions
     *
     * @param assertions the assignment
     */
    public static Model create(List<Assertion> assertions) {
        Map<String, Set<String>> dependencies = createDependencies(assertions);
        validate(dependencies);
        Set<String> hypothesis = findHypothesis(dependencies);
        Map<String, Assertion> assignmentById = assertions.stream()
                .collect(Collectors.toMap(
                        Assertion::getId,
                        Function.identity()
                ));
        Set<String> inferences = dependencies.keySet().stream()
                .filter(id -> !hypothesis.contains(id))
                .collect(Collectors.toSet());
        Set<String> axioms = dependencies.values().stream()
                .flatMap(Set::stream)
                .filter(id -> !(hypothesis.contains(id) || inferences.contains(id)))
                .collect(Collectors.toSet());
        return new Model(assignmentById, hypothesis, inferences, axioms);
    }

    /**
     * Returns the closure matrix of nodes
     *
     * @param dependencies the dependencies
     * @param nodes        the nodes
     */
    private static boolean[][] createClosure(Map<String, Set<String>> dependencies, List<String> nodes) {
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
    static Map<String, Set<String>> createDependencies(List<Assertion> assertions) {
        Map<String, Set<String>> dependenciesMap = new HashMap<>();
        for (Assertion assertion : assertions) {
            Set<String> dependencies = assertion.createDependencies();
            dependenciesMap.put(assertion.getId(), dependencies);
        }
        return dependenciesMap;
    }

    /**
     * Returns the hypothesis identifiers from dependencies
     *
     * @param dependencies the dependencies
     */
    static Set<String> findHypothesis(Map<String, Set<String>> dependencies) {
        Set<String> nodes = dependencies.values().stream()
                .flatMap(Set::stream)
                .collect(Collectors.toSet());
        return dependencies.keySet().stream()
                .filter(id -> !nodes.contains(id))
                .collect(Collectors.toSet());
    }

    /**
     * Returns the model from file
     *
     * @param file the file
     * @throws IOException in case of error
     */
    public static Model fromFile(File file) throws IOException {
        return Model.fromJson(Utils.fromFile(file), Locator.root());
    }

    /**
     * Returns the model by json spec
     *
     * @param root    the document root
     * @param locator the locator
     */
    static Model fromJson(JsonNode root, Locator locator) {
        JSON_SPEC.apply(locator).accept(root);
        List<Assertion> assertions =
                locator.propertyNames(root)
                        .map(t -> {
                            String id = t._1;
                            InferenceNode expression = InferenceNode.fromJson(root, t._2);
                            return new Assertion(id, expression);
                        })
                        .collect(Collectors.toList());
        return create(assertions);
    }

    /**
     * Returns the model from a resource
     *
     * @param resource the resource
     * @throws IOException in case of error
     */
    public static Model fromResource(String resource) throws IOException {
        return Model.fromJson(Utils.fromResource(resource), Locator.root());
    }

    /**
     * Returns the model from a resource
     *
     * @param url the resource
     * @throws IOException in case of error
     */
    public static Model fromUrl(String url) throws IOException {
        return Model.fromJson(Utils.fromUrl(new URL(url)), Locator.root());
    }

    /**
     * Validates the dependencies
     *
     * @param dependencies the dependencies
     * @throws IllegalArgumentException if dependencies are not valid
     */
    static void validate(Map<String, Set<String>> dependencies) {
        // check for cycle tree
        List<String> nodes = Stream.concat(
                        dependencies.keySet().stream(),
                        dependencies.values().stream()
                                .flatMap(Set::stream))
                .distinct()
                .sorted()
                .collect(Collectors.toList());
        boolean[][] closure = createClosure(dependencies, nodes);
        Set<String> cycleNodes = zipWithIndex(nodes)
                .filter(t -> closure[t._1][t._1])
                .map(Tuple2::getV2)
                .collect(Collectors.toSet());
        if (!cycleNodes.isEmpty()) {
            String cyclesStr = cycleNodes.stream()
                    .sorted()
                    .collect(Collectors.joining(", "));
            throw new IllegalArgumentException(format("Cycles on nodes [%s]",
                    cyclesStr));
        }
    }

    private final Map<String, Assertion> assertions;
    private final Set<String> hypothesis;
    private final Set<String> inferences;
    private final Set<String> axioms;

    /**
     * Creates the model
     *
     * @param assertions the assertions
     * @param hypothesis the hypothesis
     * @param inferences the inferences identifiers
     * @param axioms     the axiom identifiers
     */
    protected Model(Map<String, Assertion> assertions, Set<String> hypothesis, Set<String> inferences, Set<String> axioms) {
        this.hypothesis = requireNonNull(hypothesis);
        this.assertions = requireNonNull(assertions);
        this.inferences = requireNonNull(inferences);
        this.axioms = requireNonNull(axioms);
    }

    /**
     * Returns the results of inference model
     */
    Evidences apply(Evidences facts) {
        Evidences result = facts.copy().clearAssertions();
        getHypothesisStream().forEach(assertion -> assertion.apply(this, result));
        return result;
    }

    /**
     * Returns the axioms evidences in unknown state
     */
    public Evidences createUnknownAxioms() {
        Evidences result = Evidences.empty()
                .setAxioms(axioms.stream().sorted().collect(Collectors.toList()))
                .setHypothesis(hypothesis)
                .setInferences(inferences);
        axioms.forEach(id -> result.put(id, Evidences.UNKNOWN_VALUE));
        return result;
    }

    /**
     * Returns the evaluations of a predicate and stores it in evidences
     *
     * @param id        the predicate identifier
     * @param evidences the evidences
     */
    public double evaluate(String id, Evidences evidences) {
        if (!evidences.contains(id)) {
            Assertion assertion = assertions.get(id);
            if (assertion != null) {
                assertion.apply(this, evidences);
            } else {
                // Unknown predicate
                evidences.put(id, Evidences.UNKNOWN_VALUE);
            }
        }
        return evidences.get(id);
    }

    /**
     * Returns the assertion by identifier
     *
     * @param id the identifier
     */
    public Optional<Assertion> getAssertion(String id) {
        return Optional.ofNullable(assertions.get(id));
    }

    /**
     * Returns the assertions
     */
    public Map<String, Assertion> getAssertions() {
        return assertions;
    }

    /**
     * Returns the axiom identifiers
     */
    public Set<String> getAxioms() {
        return axioms;
    }

    /**
     * Returns the hypothesis identifiers
     */
    public Set<String> getHypothesis() {
        return hypothesis;
    }

    /**
     * Returns the stream of hypothesis
     */
    Stream<Assertion> getHypothesisStream() {
        return hypothesis.stream().map(assertions::get);
    }

    /**
     * Returns the inference identifiers
     */
    public Set<String> getInferences() {
        return inferences;
    }

    /**
     * Returns the evidences sorted by gradient
     *
     * @param evidences the evidences
     */
    public Stream<String> getSortedAxioms(Evidences evidences) {
        Gradient grad = gradient(evidences);
        return axioms.stream()
                .map(axiom -> {
                    double maxAbsGrad = hypothesis.stream()
                            .mapToDouble(hyp -> abs(grad.get(hyp, axiom)))
                            .max()
                            .orElse(0);
                    return Tuple2.of(axiom, maxAbsGrad);
                })
                .sorted(Comparator.comparing(Tuple2::getV1))
                .sorted(Comparator.comparingDouble(Tuple2<String, Double>::getV2).reversed())
                .map(Tuple2::getV1);
    }

    /**
     * Returns the gradient of model for the evidences
     *
     * @param evidences evidences
     */
    Gradient gradient(Evidences evidences) {
        Gradient result = Gradient.empty();
        for (String axiom : axioms) {
            double x = evidences.get(axiom);
            double dx = x >= 0.5 ? -DX : DX;
            Evidences axioms1 = evidences.copy().clearAssertions();
            axioms1.put(axiom, x + dx);
            Evidences evidences1 = apply(axioms1);
            for (String assertion : assertions.keySet()) {
                double y1 = evidences1.get(assertion);
                double y0 = evidences.get(assertion);
                double grad = (y1 - y0) / dx;
                result.put(assertion, axiom, grad);
            }
        }
        return result;
    }

    /**
     * Returns the sorted results of inference model
     */
    public Evidences infer(Evidences facts) {
        Evidences result = facts.copy().clearAssertions();
        getHypothesisStream().forEach(assertion -> assertion.apply(this, result));
        Stream<String> sorted = getSortedAxioms(result);
        result.setAxioms(sorted.collect(Collectors.toList()));
        return result;
    }
}
