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
import java.util.Map;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mmarini.TestFunctions.text;
import static org.mmarini.yaml.Utils.fromText;

class DefinitionsTest {

    @Test
    void defaultEmptyLanguages() throws IOException {
        // Given ...
        JsonNode root = fromText(
                text(
                        "---",
                        "version: \"0.1\"",
                        "model: {}",
                        "languages:",
                        "  default: {}"
                )
        );

        // When ...
        Definitions definitions = Definitions.fromJson(root, Locator.root());

        // Then ...
        assertEquals(Map.of("default", new Properties()), definitions.getLanguages());
    }

    @Test
    void defaultNonEmptyLanguages() throws IOException {
        // Given ...
        JsonNode root = fromText(
                text(
                        "---",
                        "version: \"0.1\"",
                        "model: {}",
                        "languages:",
                        "  default:",
                        "    a: aText",
                        "    b: bText"
                )
        );

        // When ...
        Definitions definitions = Definitions.fromJson(root, Locator.root());

        // Then ...
        Properties v1 = new Properties();
        v1.put("a", "aText");
        v1.put("b", "bText");

        assertEquals(Map.of("default", v1), definitions.getLanguages());
    }

    @Test
    void emptyLanguages() throws IOException {
        // Given ...
        JsonNode root = fromText(
                text(
                        "---",
                        "version: \"0.1\"",
                        "model: {}",
                        "languages: {}"
                )
        );

        // When ...
        Definitions definitions = Definitions.fromJson(root, Locator.root());

        // Then ...
        assertEquals(0, definitions.getLanguages().size());

    }

    @Test
    void noLanguages() throws IOException {
        // Given ...
        JsonNode root = fromText(
                text(
                        "---",
                        "version: \"0.1\"",
                        "model: {}"
                )
        );

        // When ...
        Definitions definitions = Definitions.fromJson(root, Locator.root());

        // Then ...
        assertEquals(0, definitions.getLanguages().size());

    }

    @Test
    void twoNonEmptyLanguages() throws IOException {
        // Given ...
        JsonNode root = fromText(
                text(
                        "---",
                        "version: \"0.1\"",
                        "model: {}",
                        "languages:",
                        "  default:",
                        "    a: aText",
                        "    b: bText",
                        "  it:",
                        "    a: TestoA",
                        "    b: TestoB"
                )
        );

        // When ...
        Definitions definitions = Definitions.fromJson(root, Locator.root());

        // Then ...
        Properties def = new Properties();
        def.put("a", "aText");
        def.put("b", "bText");

        Properties it = new Properties();
        it.put("a", "TestoA");
        it.put("b", "TestoB");

        assertEquals(Map.of("default", def, "it", it), definitions.getLanguages());
    }
}