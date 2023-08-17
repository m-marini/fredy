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
import org.junit.jupiter.api.Test;
import org.mmarini.yaml.schema.Locator;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.mmarini.TestFunctions.text;
import static org.mmarini.fredy2.model.Evidences.UNKNOWN_VALUE;
import static org.mmarini.yaml.Utils.fromText;
import static org.mockito.Mockito.*;

class ModelTest {

    @Test
    void applyEmpty() {
        // Given ...
        List<Assertion> assertions = List.of(
                new Assertion("a", new Predicate("b")),
                new Assertion("b", new Predicate("c"))
        );
        Model model = Model.create(assertions);

        Evidences evidences = Evidences.empty();

        // When ...
        Evidences result = model.apply(evidences);

        // Then ...
        assertTrue(result.contains("a"));
        assertTrue(result.contains("b"));
        assertTrue(result.contains("c"));
        assertEquals(UNKNOWN_VALUE, result.get("a"));
        assertEquals(UNKNOWN_VALUE, result.get("b"));
        assertEquals(UNKNOWN_VALUE, result.get("c"));
    }

    @Test
    void applyNotEmpty() {
        // Given ...
        List<Assertion> assertions = List.of(
                new Assertion("a", new Not(new Predicate("b"))),
                new Assertion("b", new Predicate("c"))
        );
        Model model = Model.create(assertions);

        Evidences evidences = Evidences.empty().put("c", 0.3);

        // When ...
        Evidences result = model.apply(evidences);

        // Then ...
        assertTrue(result.contains("a"));
        assertTrue(result.contains("b"));
        assertTrue(result.contains("c"));
        assertEquals(0.7, result.get("a"));
        assertEquals(0.3, result.get("b"));
        assertEquals(0.3, result.get("c"));
    }

    @Test
    void createDependencies() {
        // Given ...
        List<Assertion> assertions = List.of(
                new Assertion("a", new Predicate("b")),
                new Assertion("b", new Predicate("c"))
        );

        // When ...
        Map<String, Set<String>> dependencies = Model.createDependencies(assertions);

        // Then ...
        assertThat(dependencies, hasEntry(equalTo("a"),
                containsInAnyOrder("b")));
        assertThat(dependencies, hasEntry(equalTo("b"),
                containsInAnyOrder("c")));
    }

    @Test
    void createUnknownAxioms() {
        // Given ...
        List<Assertion> assertions = List.of(
                new Assertion("a", new Predicate("b")),
                new Assertion("b", new Predicate("c"))
        );
        Model model = Model.create(assertions);

        // When ...
        Evidences evidences = model.createUnknownAxioms();

        assertThat(evidences.getHypothesis(), containsInAnyOrder("a"));
        assertThat(evidences.getInferences(), containsInAnyOrder("b"));
        assertThat(evidences.getAxioms(), containsInAnyOrder("c"));

        assertFalse(evidences.contains("a"));
        assertFalse(evidences.contains("b"));
        assertTrue(evidences.contains("c"));
        assertEquals(UNKNOWN_VALUE, evidences.get("c"));
    }

    @Test
    void evaluateExists() {
        // Given ...
        List<Assertion> assertions = List.of(
                new Assertion("a", new Predicate("b")),
                new Assertion("b", new Predicate("c"))
        );
        Model model = Model.create(assertions);

        Evidences evidences = mock();
        when(evidences.contains("b")).thenReturn(false);
        when(evidences.contains("c")).thenReturn(true);
        when(evidences.get("c")).thenReturn(0.3);
        when(evidences.get("b")).thenReturn(0.3);

        // When ...
        double value = model.evaluate("b", evidences);

        // Then ...
        assertEquals(0.3, value);
        verify(evidences, times(2)).contains("b");
        verify(evidences, times(1)).contains("c");
        verify(evidences).get("c");
        verify(evidences).put("b", 0.3);
        verify(evidences).get("b");
        verifyNoMoreInteractions(evidences);
    }

    @Test
    void evaluateNoExists() {
        // Given ...
        List<Assertion> assertions = List.of(
                new Assertion("a", new Predicate("b")),
                new Assertion("b", new Predicate("c"))
        );
        Model model = Model.create(assertions);

        Evidences evidences = mock();
        when(evidences.contains("c")).thenReturn(false);
        when(evidences.get("c")).thenReturn(UNKNOWN_VALUE);

        // When ...
        double value = model.evaluate("c", evidences);

        // Then ...
        assertEquals(UNKNOWN_VALUE, value);
        verify(evidences).contains("c");
        verify(evidences).put("c", UNKNOWN_VALUE);
        verify(evidences).get("c");
        verifyNoMoreInteractions(evidences);
    }

    @Test
    void findHypothesis() {
        // Given ...
        List<Assertion> assertions = List.of(
                new Assertion("a", new Predicate("b")),
                new Assertion("b", new Predicate("c"))
        );
        Map<String, Set<String>> dependencies = Model.createDependencies(assertions);

        // When ...
        Set<String> hypothesis = Model.findHypothesis(dependencies);

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

    @Test
    void validateAcycled() {
        Map<String, Set<String>> dependencies = Map.of(
                "a", Set.of("b", "c"),
                "b", Set.of("d", "e"),
                "c", Set.of("e")
        );
        assertDoesNotThrow(() -> Model.validate(dependencies));
    }

    @Test
    void validateCycled() {
        Map<String, Set<String>> dependencies = Map.of(
                "a", Set.of("b", "c"),
                "b", Set.of("d", "e"),
                "e", Set.of("b")
        );
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Model.validate(dependencies));
        assertThat(ex.getMessage(), matchesPattern("Cycles on nodes \\[b, e\\]"));
    }
}