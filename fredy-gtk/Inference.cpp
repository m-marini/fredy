#include <iostream>

#include "Inference.h"
#include "Fuzzy.h"

using namespace std;

const float fredy::Predicate::evaluate(const Context &context) const
{
    return context.count(_id) != 0
               ? context.at(_id)
               : 0.5;
}

const float fredy::And::evaluate(const Context &context) const
{
    float result = 1;
    for (auto it = _terms.begin(); it != _terms.end(); ++it)
    {
        auto x = (*it)->evaluate(context);
        result = fzand(result, x);
    }
    return result;
}

const float fredy::Or::evaluate(const Context &context) const
{
    float result = 0;
    for (auto it = _terms.begin(); it != _terms.end(); ++it)
    {
        auto x = (*it)->evaluate(context);
        result = fzor(result, x);
    }
    return result;
}

/**
 * Returns the evaluated context
 * @param facts the facts context
 */
fredy::Context fredy::Inference::evaluate(const Context &facts) const
{
    return facts;
}
