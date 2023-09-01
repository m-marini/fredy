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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasToString;
import static org.hamcrest.Matchers.matchesPattern;
import static org.mmarini.TestFunctions.text;
import static org.mmarini.yaml.Utils.fromText;

class InferenceNodeTest {
    @Test
    void fromJsonAnd() throws IOException {
        // Given ...
        JsonNode node = fromText(text(
                "---",
                "type: and",
                "expressions:",
                "- type: predicate",
                "  id: a",
                "- type: predicate",
                "  id: b",
                "- type: predicate",
                "  id: c"
        ));

        // When ...
        InferenceNode p = InferenceNode.fromJson(node, Locator.root());

        // Then ...
        assertThat(p, hasToString(
                matchesPattern("and\\('a', 'b', 'c'\\)")
        ));
    }

    @Test
    void fromJsonCertainty() throws IOException {
        // Given ...
        JsonNode node = fromText(text(
                "---",
                "type: certainty",
                "assertion:",
                "  type: predicate",
                "  id: a",
                "negation:",
                "  type: predicate",
                "  id: b"
        ));

        // When ...
        InferenceNode p = InferenceNode.fromJson(node, Locator.root());

        // Then ...
        assertThat(p, hasToString(
                matchesPattern("certainty\\('a', 'b'\\)")
        ));
    }

    @Test
    void fromJsonFalsity() throws IOException {
        // Given ...
        JsonNode node = fromText(text(
                "---",
                "type: falsity",
                "assertion:",
                "  type: predicate",
                "  id: a",
                "negation:",
                "  type: predicate",
                "  id: b"
        ));

        // When ...
        InferenceNode p = InferenceNode.fromJson(node, Locator.root());

        // Then ...
        assertThat(p, hasToString(
                matchesPattern("falsity\\('a', 'b'\\)")
        ));
    }

    @Test
    void fromJsonIff() throws IOException {
        // Given ...
        JsonNode node = fromText(text(
                "---",
                "type: iff",
                "antecedent:",
                "  type: predicate",
                "  id: a",
                "consequent:",
                "  type: predicate",
                "  id: b"
        ));

        // When ...
        InferenceNode p = InferenceNode.fromJson(node, Locator.root());

        // Then ...
        assertThat(p, hasToString(
                matchesPattern("iff\\('a', 'b'\\)")
        ));
    }

    @Test
    void fromJsonImplies() throws IOException {
        // Given ...
        JsonNode node = fromText(text(
                "---",
                "type: implies",
                "antecedent:",
                "  type: predicate",
                "  id: a",
                "consequent:",
                "  type: predicate",
                "  id: b"
        ));

        // When ...
        InferenceNode p = InferenceNode.fromJson(node, Locator.root());

        // Then ...
        assertThat(p, hasToString(
                matchesPattern("implies\\('a', 'b'\\)")
        ));
    }

    @Test
    void fromJsonIsAntinomy() throws IOException {
        // Given ...
        JsonNode node = fromText(text(
                "---",
                "type: isAntinomy",
                "assertion:",
                "  type: predicate",
                "  id: a",
                "negation:",
                "  type: predicate",
                "  id: b"
        ));

        // When ...
        InferenceNode p = InferenceNode.fromJson(node, Locator.root());

        // Then ...
        assertThat(p, hasToString(
                matchesPattern("isAntinomy\\('a', 'b'\\)")
        ));
    }

    @Test
    void fromJsonIsCertain() throws IOException {
        // Given ...
        JsonNode node = fromText(text(
                "---",
                "type: isCertain",
                "expression:",
                "  type: predicate",
                "  id: a"
        ));

        // When ...
        InferenceNode p = InferenceNode.fromJson(node, Locator.root());

        // Then ...
        assertThat(p, hasToString(
                matchesPattern("isCertain\\('a'\\)")
        ));
    }

    @Test
    void fromJsonIsCertainFalse() throws IOException {
        // Given ...
        JsonNode node = fromText(text(
                "---",
                "type: isCertainFalse",
                "expression:",
                "  type: predicate",
                "  id: a"
        ));

        // When ...
        InferenceNode p = InferenceNode.fromJson(node, Locator.root());

        // Then ...
        assertThat(p, hasToString(
                matchesPattern("isCertainFalse\\('a'\\)")
        ));
    }

    @Test
    void fromJsonIsCertainTrue() throws IOException {
        // Given ...
        JsonNode node = fromText(text(
                "---",
                "type: isCertainTrue",
                "expression:",
                "  type: predicate",
                "  id: a"
        ));

        // When ...
        InferenceNode p = InferenceNode.fromJson(node, Locator.root());

        // Then ...
        assertThat(p, hasToString(
                matchesPattern("isCertainTrue\\('a'\\)")
        ));
    }

    @Test
    void fromJsonNot() throws IOException {
        // Given ...
        JsonNode node = fromText(text(
                "---",
                "type: not",
                "expression:",
                "  type: predicate",
                "  id: a"
        ));

        // When ...
        InferenceNode p = InferenceNode.fromJson(node, Locator.root());

        // Then ...
        assertThat(p, hasToString(
                matchesPattern("not\\('a'\\)")
        ));
    }

    @Test
    void fromJsonOr() throws IOException {
        // Given ...
        JsonNode node = fromText(text(
                "---",
                "type: or",
                "expressions:",
                "- type: predicate",
                "  id: a",
                "- type: predicate",
                "  id: b",
                "- type: predicate",
                "  id: c"
        ));

        // When ...
        InferenceNode p = InferenceNode.fromJson(node, Locator.root());

        // Then ...
        assertThat(p, hasToString(
                matchesPattern("or\\('a', 'b', 'c'\\)")
        ));
    }

    @Test
    void fromJsonPredicate() throws IOException {
        // Given ...
        JsonNode node = fromText(text(
                "---",
                "type: predicate",
                "id: a"
        ));

        // When ...
        InferenceNode p = InferenceNode.fromJson(node, Locator.root());

        // Then ...
        assertThat(p, hasToString(
                matchesPattern("'a'")
        ));
    }

    @Test
    void fromJsonSomewhat() throws IOException {
        // Given ...
        JsonNode node = fromText(text(
                "---",
                "type: somewhat",
                "expression:",
                "  type: predicate",
                "  id: a"
        ));

        // When ...
        InferenceNode p = InferenceNode.fromJson(node, Locator.root());

        // Then ...
        assertThat(p, hasToString(
                matchesPattern("somewhat\\('a'\\)")
        ));
    }

    @Test
    void fromJsonTruth() throws IOException {
        // Given ...
        JsonNode node = fromText(text(
                "---",
                "type: truth",
                "assertion:",
                "  type: predicate",
                "  id: a",
                "negation:",
                "  type: predicate",
                "  id: b"
        ));

        // When ...
        InferenceNode p = InferenceNode.fromJson(node, Locator.root());

        // Then ...
        assertThat(p, hasToString(
                matchesPattern("truth\\('a', 'b'\\)")
        ));
    }

    @Test
    void fromJsonVery() throws IOException {
        // Given ...
        JsonNode node = fromText(text(
                "---",
                "type: very",
                "expression:",
                "  type: predicate",
                "  id: a"
        ));

        // When ...
        InferenceNode p = InferenceNode.fromJson(node, Locator.root());

        // Then ...
        assertThat(p, hasToString(
                matchesPattern("very\\('a'\\)")
        ));
    }

    @Test
    void fromJsonXor() throws IOException {
        // Given ...
        JsonNode node = fromText(text(
                "---",
                "type: xor",
                "expressions:",
                "- type: predicate",
                "  id: a",
                "- type: predicate",
                "  id: b",
                "- type: predicate",
                "  id: c"
        ));

        // When ...
        InferenceNode p = InferenceNode.fromJson(node, Locator.root());

        // Then ...
        assertThat(p, hasToString(
                matchesPattern("xor\\('a', 'b', 'c'\\)")
        ));
    }
}