#ifndef Fuzzy_h
#define Fuzzy_h

/**
 * Returns the negated value (1 - value)
 * @param value the value
 */
extern const float fznot(const float value);

/**
 * Returns the and value of arguments
 * @param a the first value
 * @param b the second value
 */
extern const float fzand(const float a, const float b);

/**
 * Returns the or value of arguments
 * @param a the first value
 * @param b the second value
 */
extern const float fzor(const float a, const float b);

#endif