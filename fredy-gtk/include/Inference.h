#ifndef Inference_h
#define Inference_h

#include <string>
#include <map>
#include <vector>

namespace fredy
{
    typedef std::map<const std::string, float> Context;

    /**
     * Evaluates the tree node
     */
    class Expression
    {
    public:
        /**
         * Returns the value of expression
         * @param context the context
         */
        virtual const float evaluate(const Context &context) const = 0;
    };

    /**
     * Defines predicate expression
     */
    class Predicate : public Expression
    {
    public:
        /**
         * Creates the predicate
         * @param id the identifier
         */
        Predicate(const std::string &id) : _id(id) {}
        Predicate(const Predicate &other) : _id(other._id) {}

        /**
         * Returns the value of expression
         * @param context the context
         */
        virtual const float evaluate(const Context &context) const override;

        /**
         * Returns the identifier
         */
        const std::string &id(void) const { return _id; }

    private:
        std::string _id;
    };

    /**
     * Defines and expression
     */
    class And : public Expression
    {
    public:
        And(const std::vector<const Expression *> &terms) : _terms(terms) {}
        And(const And &other) : _terms(other._terms) {}

        /**
         * Returns the value of expression
         * @param context the context
         */
        virtual const float evaluate(const Context &context) const override;

    private:
        std::vector<const Expression *> _terms;
    };

    /**
     * Defines or expression
     */
    class Or : public Expression
    {
    public:
        Or(const std::vector<const Expression *> &terms) : _terms(terms) {}
        Or(const Or &other) : _terms(other._terms) {}

        /**
         * Returns the value of expression
         * @param context the context
         */
        virtual const float evaluate(const Context &context) const override;

    private:
        std::vector<const Expression *> _terms;
    };

    /**
     * Assertions map
     */
    typedef std::map<const std::string, const Expression *> Assertions;

    /**
     * Inference engine
     */
    class Inference
    {
    public:
        /**
         * Creates the inference engine
         * @param assertions the assertions
         */
        Inference(const Assertions &assertions) : _assertions(assertions) {}

        /**
         * Returns the evaluated context
         * @param facts the facts context
         */
        Context evaluate(const Context &facts) const;

    private:
        Assertions _assertions;
    };

    class ExpBuilder
    {
    public:
    };

    class PredicateBuilder : public ExpBuilder
    {
    public:
        PredicateBuilder(const std::string id) : _id(id) {}
        PredicateBuilder(const PredicateBuilder &other) : _id(other._id) {}

        PredicateBuilder &operator=(const PredicateBuilder &other)
        {
            _id = other._id;
            return *this;
        }

    private:
        std::string _id;
    };

    class NotBuilder : public ExpBuilder
    {
        NotBuilder(const ExpBuilder *exp) : _exp(exp) {}
        NotBuilder(const NotBuilder &other) : _exp(other._exp) {}

        NotBuilder &operator=(const NotBuilder &other)
        {
            _exp = other._exp;
            return *this;
        }

    private:
        const ExpBuilder *_exp;
    };
};

#endif