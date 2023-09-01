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
import org.hamcrest.Matcher;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mmarini.yaml.schema.Locator;

import java.io.IOException;
import java.util.*;
import java.util.stream.Stream;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.mmarini.TestFunctions.text;
import static org.mmarini.fredy2.model.Model.UNKNOWN_VALUE;
import static org.mmarini.yaml.Utils.fromText;

class ModelTest implements TestUtils {
    @ParameterizedTest
    @CsvSource({
            "0.5, 0.5, 0, 0, a b, a b, a, a, 0",
            "0  , 0  , 0, 1, a b, a  , a, a, 0",
            "1  , 1  , 0, 1, a b, a  , a, a, 0",

            "0.5, 0.5, 0, 0, a b, a b, a, b, -1",
            "0.5, 0.5, 0, 0, a b, a  , a, a, -1",
            "0.5, 0.5, 1, 0, a b, a b, a, a, -1",
            "0.5, 0  , 0, 0, a b, a b, a, a, -1",
            "0.5, 1  , 0, 0, a b, a b, a, a, -1",
            "1  , 0  , 0, 0, a b, a b, a, a, -1",

            "0.5, 0.5, 0, 0, a b, a b, b, a, 1",
            "0.5, 0.5, 0, 0, a  , a b, a, a, 1",
            "0.5, 0.5, 0, 1, a b, a b, a, a, 1",
            "0  , 0.5, 0, 1, a b, a b, a, a, 1",
            "1  , 0.5, 0, 1, a b, a b, a, a, 1",
            "0  , 1  , 0, 1, a b, a b, a, a, 1",
    })
    void compareAxioms(double aTruth, double bTruth,
                       double aMax, double bMax,
                       String aHyps, String bHyps,
                       String a, String b,
                       int expected
    ) {
        // Given ...
        AxiomStatus aAxiom = new AxiomStatus(a, aTruth, aMax, List.of(aHyps.split(" ")));
        AxiomStatus bAxiom = new AxiomStatus(b, bTruth, bMax, List.of(bHyps.split(" ")));

        // When ...
        int result = Model.AXIOM_STATUS_COMPARATOR.compare(aAxiom, bAxiom);

        // Then ...
        assertEquals(expected, result);
    }

    @ParameterizedTest
    @CsvSource({
            "0.5, 0.5, a, a, 0",
            "0, 0, a, a, 0",
            "1, 1, a, a, 0",

            "0.5, 0.5, a, b, -1",
            "0, 0.5, a, a, -1",
            "1, 0.5, a, a, -1",
            "0, 0, a, b, -1",
            "1, 1, a, b, -1",
            "1, 0, a, a, -1",

            "0.5, 0.5, b, a, 1",
            "0, 0, b, a, 1",
            "1, 1, b, a, 1",
            "0.5, 0, a, a, 1",
            "0.5, 1, a, a, 1",
            "0, 1, a, a, 1",
    })
    void comparePredicate(double aTruth, double bTruth,
                          String a, String b,
                          int expected
    ) {
        // Given ...
        PredicateStatus aAxiom = new PredicateStatus(a, aTruth);
        PredicateStatus bAxiom = new PredicateStatus(b, bTruth);

        // When ...
        int result = Model.PREDICATE_STATUS_COMPARATOR.compare(aAxiom, bAxiom);

        // Then ...
        assertEquals(expected, result);
    }

    @Test
    void createDependencies() {
        // Given ...
        Map<String, InferenceNode> assertions = Map.of(
                "a", new Predicate("b"),
                "b", new Predicate("c")
        );

        // When ...
        Map<String, Collection<String>> dependencies = Model.createDependencies(assertions);

        // Then ...
        assertThat(dependencies, hasEntry(equalTo("a"),
                containsInAnyOrder("b")));
        assertThat(dependencies, hasEntry(equalTo("b"),
                containsInAnyOrder("c")));
    }

    @Test
    void createUnknownAxioms() {
        // Given ...
        Map<String, InferenceNode> assertions = Map.of(
                "a", new Predicate("b"),
                "b", new Predicate("c")
        );
        Model model = Model.create(assertions);

        // When ...
        Map<String, Double> evidences = model.createUnknownAxioms();

        assertThat(evidences, hasEntry("c", UNKNOWN_VALUE));
        assertEquals(1, evidences.size());
    }

    @Test
    void evaluateExists() {
        // Given ...
        Map<String, InferenceNode> assertions = Map.of(
                "a", new Predicate("b"),
                "b", new Predicate("c")
        );
        Model model = Model.create(assertions);

        Map<String, Double> evidences = Map.of("b", 0.3, "c", 0.3);

        // When ...
        double value = model.evaluate("b", evidences);

        // Then ...
        assertEquals(0.3, value);
    }

    @Test
    void evaluateNoExists() {
        // Given ...
        Map<String, InferenceNode> assertions = Map.of(
                "a", new Predicate("b"),
                "b", new Predicate("c")
        );
        Model model = Model.create(assertions);

        Map<String, Double> evidences = new HashMap<>();

        // When ...
        double value = model.evaluate("c", evidences);

        // Then ...
        assertEquals(UNKNOWN_VALUE, value);
        assertThat(evidences, hasEntry("c", UNKNOWN_VALUE));
        assertEquals(1, evidences.size());
    }

    @Test
    void extractClosure() {
        // Given ...
        /*
        e  a
        |/ \
        b   c
        |
        d
         */
        Map<String, Collection<String>> dependencies = Map.of(
                "a", List.of("b", "c"),
                "b", List.of("d"),
                "e", List.of("b")
        );

        // When ...
        Map<String, Collection<String>> result = Model.extractReverseClosure(dependencies);

        // Then ...
        assertThat(result, hasEntry(equalTo("a"), empty()));
        assertThat(result, hasEntry(equalTo("b"), containsInAnyOrder("a", "e")));
        assertThat(result, hasEntry(equalTo("c"), containsInAnyOrder("a")));
        assertThat(result, hasEntry(equalTo("d"), containsInAnyOrder("a", "b", "e")));
        assertThat(result, hasEntry(equalTo("e"), empty()));
        assertEquals(5, result.size());
    }

    @Test
    void extractHypothesisByAxiom() {
        // Given ...
        /*
        e  a
        |/ \
        b   c
        |
        d
         */
        Map<String, Collection<String>> closure = Map.of(
                "a", List.of(),
                "b", List.of("a", "e"),
                "c", List.of("a"),
                "d", List.of("a", "b", "c", "e"),
                "e", List.of()
        );
        Collection<String> axioms = List.of("c", "d");
        Collection<String> hypothesis = List.of("a", "e");

        // When ...
        Map<String, Collection<String>> result = Model.extractHypothesisByAxiom(closure, axioms, hypothesis);

        // Then ...
        assertThat(result, hasEntry(equalTo("c"), containsInAnyOrder("a")));
        assertThat(result, hasEntry(equalTo("d"), containsInAnyOrder("a", "e")));
    }

    @Test
    void findHypothesis() {
        // Given ...
        Map<String, InferenceNode> assertions = Map.of(
                "a", new Predicate("b"),
                "b", new Predicate("c")
        );
        Map<String, Collection<String>> dependencies = Model.createDependencies(assertions);

        // When ...
        Collection<String> hypothesis = Model.findHypothesis(dependencies);

        // Then ...
        assertThat(hypothesis, containsInAnyOrder("a"));
    }

    @Test
    void fromJson() throws IOException {
        // Given ...
        JsonNode node = fromText(text(
                "---",
                "a:",
                "  type: predicate",
                "  id: b",
                "b:",
                "  type: predicate",
                "  id: c"
        ));

        // When ...
        Model p = Model.fromJson(node, Locator.root());

        // Then ...
        assertThat(p.getHypothesis(), contains("a"));
        assertThat(p.getInferences(), contains("b"));
        assertThat(p.getAxioms(), contains("c"));
    }

    @ParameterizedTest
    @CsvSource({
            "0  , 0  , a, b",
            "0  , 0.5, a, b",
            "0  , 1  , b, a",
            "0.5, 0  , b, a",
            "0.5, 0.5, a, b",
            "0.5, 1  , b, a",
            "1  , 0  , a, b",
            "1  , 0.5, a, b",
            "1  , 1  , a, b",
    })
    void getHypothesis(double a, double b, String h0, String h1) {
        Map<String, InferenceNode> assertions = Map.of(
                "a", new Predicate("c"),
                "b", new Predicate("d")
        );
        Model model = Model.create(assertions);

        Map<String, Double> evidences = Map.of("a", a, "b", b);

        // When ...
        List<PredicateStatus> result = model.getHypothesis(evidences);

        // Then ...
        assertThat(result, contains(
                hasProperty("id", equalTo(h0)),
                hasProperty("id", equalTo(h1))
        ));
    }

    @ParameterizedTest
    @CsvSource({
            "0  , 0  , a, b",
            "0  , 0.5, a, b",
            "0  , 1  , b, a",
            "0.5, 0  , b, a",
            "0.5, 0.5, a, b",
            "0.5, 1  , b, a",
            "1  , 0  , a, b",
            "1  , 0.5, a, b",
            "1  , 1  , a, b",
    })
    void getInferences(double a, double b, String h0, String h1) {
        Map<String, InferenceNode> assertions = Map.of(
                "root", And.create(new Predicate("a"), new Predicate("b")),
                "a", new Predicate("c"),
                "b", new Predicate("d")
        );
        Model model = Model.create(assertions);

        Map<String, Double> evidences = Map.of("a", a, "b", b);

        // When ...
        List<PredicateStatus> result = model.getInferences(evidences);

        // Then ...
        assertThat(result, contains(
                hasProperty("id", equalTo(h0)),
                hasProperty("id", equalTo(h1))
        ));
    }

    @ParameterizedTest
    @CsvSource({
            "0  , 0  , ,   ",
            "0  , 0.5, ,b  ",
            "0  , 1  , ,   ",
            "0.5, 0  ,a,a  ",
            "0.5, 0.5,a,a b",
            "0.5, 1  ,a,a  ",
            "1  , 0  , ,   ",
            "1  , 0.5, ,b  ",
            "1  , 1  , ,   ",
    })
    void getUnknownHypothesis(double a, double b, String expC, String expD) {
        // Given ...
        Map<String, InferenceNode> assertions = Map.of(
                "a", And.create(
                        new Predicate("c"),
                        new Predicate(("d"))
                ),
                "b", new Predicate(("d")
                )
        );
        Model model = Model.create(assertions);
        Map<String, Double> evidences = Map.of("a", a, "b", b);

        // When ...
        Map<String, Collection<String>> result = model.getUnknownHypothesis(evidences);

        // Then ...
        Matcher<Iterable<? extends String>> matcherC = containsInAnyOrder(
                (expC != null ? Arrays.stream(expC.split(" ")) : Stream.empty())
                        .map(Matchers::equalTo)
                        .toArray(Matcher[]::new));
        Matcher<Iterable<? extends String>> matcherD = containsInAnyOrder(
                (expD != null ? Arrays.stream(expD.split(" ")) : Stream.empty())
                        .map(Matchers::equalTo)
                        .toArray(Matcher[]::new));
        assertThat(result, hasEntry(equalTo("c"), matcherC));
        assertThat(result, hasEntry(equalTo("d"), matcherD));
    }

    @Test
    void inferEmpty() {
        // Given ...
        Map<String, InferenceNode> assertions = Map.of(
                "a", new Predicate("b"),
                "b", new Predicate("c")
        );
        Model model = Model.create(assertions);

        Map<String, Double> evidences = new HashMap<>();

        // When ...
        Map<String, Double> result = model.infer(evidences);

        // Then ...
        assertThat(result, hasEntry("a", UNKNOWN_VALUE));
        assertThat(result, hasEntry("b", UNKNOWN_VALUE));
        assertThat(result, hasEntry("c", UNKNOWN_VALUE));
    }

    @Test
    void inferNotEmpty() {
        // Given ...
        Map<String, InferenceNode> assertions = Map.of(
                "a", new Not(new Predicate("b")),
                "b", new Predicate("c")
        );
        Model model = Model.create(assertions);

        Map<String, Double> evidences = new HashMap<>(Map.of("c", 0.3));

        // When ...
        Map<String, Double> result = model.infer(evidences);

        // Then ...
        assertThat(result, hasEntry("a", 0.7));
        assertThat(result, hasEntry("b", 0.3));
        assertThat(result, hasEntry("c", 0.3));
    }

    @ParameterizedTest
    @CsvSource({
            "0  , 0  , 1  , 1",
            "0  , 0.5, 1.5, 1",
            "0  , 1  , 2  , 1",
            "0.5, 0  , 1  , 1.5",
            "0.5, 0.5, 1.5, 1.5",
            "0.5, 1  , 2  , 1.5",
            "1  , 0  , 1  , 2",
            "1  , 0.5, 1.5, 2",
            "1  , 1  , 2  , 2",
    })
    void maxAndOr(double c, double d, double xc, double xd) {
        // Given ...
        Map<String, InferenceNode> assertions = Map.of(
                "a", And.create(
                        new Predicate("c"),
                        new Predicate(("d"))
                ),
                "b", Or.create(
                        new Predicate("c"),
                        new Predicate(("d"))
                )
        );
        Model model = Model.create(assertions);
        Map<String, Double> axioms = new HashMap<>(Map.of("d", d, "c", c));
        Map<String, Double> evidences = model.infer(axioms);

        // When ...
        Map<String, Double> result = model.getMaxTruthByAxiomsExtremes(evidences);

        // Then ...
        assertThat(result, hasEntry("c", xc));
        assertThat(result, hasEntry("d", xd));
    }

    @ParameterizedTest
    @CsvSource({
            "0  , 2",
            "0.5, 2",
            "1  , 2",
    })
    void maxNot(double c, double xc) {
        // Given ...
        Map<String, InferenceNode> assertions = Map.of(
                "a", new Not(new Predicate("c")),
                "b", new Not(new Predicate("c"))
        );
        Model model = Model.create(assertions);
        Map<String, Double> axioms = new HashMap<>(Map.of("c", c));
        Map<String, Double> evidences = model.infer(axioms);

        // When ...
        Map<String, Double> result = model.getMaxTruthByAxiomsExtremes(evidences);
        assertThat(result, hasEntry("c", xc));
    }

    @ParameterizedTest
    @CsvSource({
            "0  , 2",
            "0.5, 2",
            "1  , 2",
    })
    void maxPredicate(double c, double xc) {
        // Given ...
        Map<String, InferenceNode> assertions = Map.of(
                "a", new Predicate("c"),
                "b", new Predicate("c")
        );
        Model model = Model.create(assertions);
        Map<String, Double> axioms = new HashMap<>(Map.of("c", c));
        Map<String, Double> result = model.infer(axioms);

        // When ...
        Map<String, Double> grad = model.getMaxTruthByAxiomsExtremes(result);
        assertThat(grad, hasEntry("c", xc));
    }

    @ParameterizedTest
    @CsvSource({
            "0  , 2",
            "0.5, 2",
            "1  , 2",
    })
    void maxSomewhat(double c, double xc) {
        // Given ...
        Map<String, InferenceNode> assertions = Map.of(
                "a", new Somewhat(new Predicate("c")),
                "b", new Somewhat(new Predicate("c"))
        );
        Model model = Model.create(assertions);
        Map<String, Double> axioms = new HashMap<>(Map.of("c", c));
        Map<String, Double> evidences = model.infer(axioms);

        // When ...
        Map<String, Double> result = model.getMaxTruthByAxiomsExtremes(evidences);
        assertThat(result, hasEntry("c", xc));
    }

    @ParameterizedTest
    @CsvSource({
            "0  , 2",
            "0.5, 2",
            "1  , 2",
    })
    void maxVery(double c, double xc) {
        // Given ...
        Map<String, InferenceNode> assertions = Map.of(
                "a", new Very(new Predicate("c")),
                "b", new Very(new Predicate("c"))
        );
        Model model = Model.create(assertions);
        Map<String, Double> axioms = new HashMap<>(Map.of("c", c));
        Map<String, Double> evidences = model.infer(axioms);

        // When ...
        Map<String, Double> result = model.getMaxTruthByAxiomsExtremes(evidences);
        assertThat(result, hasEntry("c", xc));
    }

    @Test
    void validateAcycled() {
        Map<String, Collection<String>> dependencies = Map.of(
                "a", Set.of("b", "c"),
                "b", Set.of("d", "e"),
                "c", Set.of("e")
        );
        assertDoesNotThrow(() -> Model.validate(dependencies));
    }

    @Test
    void validateCycled() {
        Map<String, Collection<String>> dependencies = Map.of(
                "a", Set.of("b", "c"),
                "b", Set.of("d", "e", "b"),
                "e", Set.of("b", "e")
        );
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Model.validate(dependencies));
        assertThat(ex.getMessage(), matchesPattern("Cycles on nodes \\[b, e]"));
    }
}