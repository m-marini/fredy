/**
 * 
 */
package org.mmarini.fredy.swing;

import java.io.IOException;
import java.net.URL;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;

import org.mmarini.functional.FList;
import org.mmarini.fuzzy.AxiomValue;
import org.mmarini.fuzzy.FuzzyBoolean;
import org.mmarini.fuzzy.InferenceEngine;
import org.mmarini.fuzzy.PredicateValue;
import org.xml.sax.SAXException;

/**
 * @author us00852
 * 
 */
public class FredyHandler {

	private InferenceEngine engine;
	private Comparator<? super PredicateValue> cmp;
	private Comparator<? super AxiomValue> axiomComparator;

	/**
	 * 
	 */
	public FredyHandler() {
		engine = new InferenceEngine();
		axiomComparator = new Comparator<AxiomValue>() {

			@Override
			public int compare(AxiomValue a0, AxiomValue a1) {
				FuzzyBoolean v0 = a0.getValue();
				FuzzyBoolean v1 = a1.getValue();
				double kd = v0.known().getValue() - v1.known().getValue();
				if (kd < -0.0)
					return -1;
				if (kd > 0.0)
					return 1;
				if (FuzzyBoolean.UNKNOWN.equals(v0)) {
					int d = a0.getAxiomsCount() - a1.getAxiomsCount();
					if (d != 0)
						return d;
					d = a0.getHypothesisCount() - a1.getHypothesisCount();
					if (d != 0)
						return -d;
				}
				double vd = v0.getValue() - v1.getValue();
				if (vd < -0.0)
					return -1;
				if (vd > 0.0)
					return 1;
				return a0.getPredicate().compareTo(a1.getPredicate());
			}
		};
		cmp = new Comparator<PredicateValue>() {

			/**
			 * 
			 * @param p0
			 * @param p1
			 * @return
			 */
			@Override
			public int compare(PredicateValue p0, PredicateValue p1) {
				FuzzyBoolean v0 = p0.getValue();
				FuzzyBoolean v1 = p1.getValue();
				double kd = v0.known().getValue() - v1.known().getValue();
				if (kd < -0.0)
					return -1;
				if (kd > 0.0)
					return 1;
				double vd = v0.getValue() - v1.getValue();
				if (vd < -0.0)
					return -1;
				if (vd > 0.0)
					return 1;
				return p0.getPredicate().compareTo(p1.getPredicate());
			}
		};
	}

	/**
	 * 
	 */
	public void analyze() {
		engine.analyze();
	}

	/**
	 * 
	 * @param url
	 * @throws IOException
	 * @throws SAXException
	 * @throws ParserConfigurationException
	 * @throws FactoryConfigurationError
	 */
	public void loadRules(URL url) throws FactoryConfigurationError,
			ParserConfigurationException, SAXException, IOException {
		engine.loadRules(url);
	}

	/**
	 * 
	 * @param values
	 */
	public List<AxiomValue> retrieveAxiom() {
		FList<AxiomValue> values = engine.retreiveAxiomValues();
		Collections.sort(values, axiomComparator);
		return values;
	}

	/**
	 * 
	 * @param values
	 */
	public List<PredicateValue> retrieveHypothesis() {
		FList<PredicateValue> values = engine.retrieveHypothesisValues();
		Collections.sort(values, Collections.reverseOrder(cmp));
		return values;
	}

	/**
	 * 
	 * @param values
	 * @return
	 */
	public List<PredicateValue> retrieveInferences() {
		FList<PredicateValue> values = engine.retrieveInferenceValues();
		Collections.sort(values, Collections.reverseOrder(cmp));
		return values;
	}

	/**
	 * 
	 * @param list
	 */
	public void setAxioms(List<? extends PredicateValue> list) {
		if (list != null)
			engine.applyPredicates(list);
	}

}
