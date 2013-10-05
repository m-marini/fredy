/**
 * 
 */
package org.mmarini.fredy.swing;

import java.text.MessageFormat;

/**
 * @author us00852
 * 
 */
public class MessagesUtils {

	/**
	 * 
	 * @param key
	 * @param values
	 * @return
	 */
	public static String format(String key, Object... values) {
		return MessageFormat.format(Messages.getString(key), values);
	}
}
