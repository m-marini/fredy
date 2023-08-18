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
    void fromJson() throws IOException {
        // Given ...
        JsonNode node = fromText(text(
                "---",
                "type: or",
                "expressions:",
                "  - type: and",
                "    expressions:",
                "      - type: not",
                "        expression:",
                "          type: predicate",
                "          id: a",
                "      - type: predicate",
                "        id: b",
                "  - type: somewhat",
                "    expression:",
                "      type: very",
                "      expression:",
                "        type: predicate",
                "        id: c",
                "  - type: iff",
                "    expression1:",
                "      type: predicate",
                "      id: d",
                "    expression2:",
                "      type: predicate",
                "      id: e",
                "  - type: implies",
                "    expression1:",
                "      type: predicate",
                "      id: e",
                "    expression2:",
                "      type: predicate",
                "      id: f",
                "  - type: isParadox",
                "    assertion:",
                "      type: predicate",
                "      id: g",
                "    negation:",
                "      type: predicate",
                "      id: h",
                "  - type: isCertain",
                "    assertion:",
                "      type: predicate",
                "      id: g",
                "    negation:",
                "      type: predicate",
                "      id: h",
                "  - type: isTrue",
                "    assertion:",
                "      type: predicate",
                "      id: i",
                "    negation:",
                "      type: predicate",
                "      id: j"
        ));

        // When ...
        InferenceNode p = InferenceNode.fromJson(node, Locator.root());

        // Then ...
        assertThat(p, hasToString(
                matchesPattern("or\\(and\\(not\\('a'\\), 'b'\\), " +
                        "somewhat\\(very\\('c'\\)\\), " +
                        "iff\\('d', 'e'\\), " +
                        "implies\\('e', 'f'\\), " +
                        "isParadox\\('g', 'h'\\), " +
                        "isCertain\\('g', 'h'\\), " +
                        "isTrue\\('i', 'j'\\)" +
                        "\\)")
        ));
    }
}