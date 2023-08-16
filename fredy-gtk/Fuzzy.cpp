
#include <algorithm>

#include "Fuzzy.h"

/**
 * Returns the negated value (1 - value)
 * @param value the value
 */
const float fznot(const float value)
{
    return 1 - value;
}

/**
 * Returns the and value of arguments
 * @param a the first value
 * @param b the second value
 */
const float fzand(const float a, const float b)
{
    return a < b ? a : b;
}

/**
 * Returns the or value of arguments
 * @param a the first value
 * @param b the second value
 */
const float fzor(const float a, const float b)
{
    return a > b ? a : b;
}
