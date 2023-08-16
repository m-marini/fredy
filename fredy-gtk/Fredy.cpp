#include <iostream>
#include <map>
#include <vector>
#include <cstdarg>

/*
#include <yaml-cpp/depthguard.h>
#include "yaml-cpp/parser.h"
#include "yaml-cpp/exceptions.h"
*/

#include "Inference.h"
#include "Fuzzy.h"

using namespace std;

/*
static void loadyaml()
{

    YAML::Node config = YAML::LoadFile("config.yaml");

    if (config["lastLogin"])
    {
        std::cout << "Last logged in: " << config["lastLogin"].as<DateTime>() << "\n";
    }

    const std::string username = config["username"].as<std::string>();
    const std::string password = config["password"].as<std::string>();
    login(username, password);
    config["lastLogin"] = getCurrentDateTime();

    std::ofstream fout("config.yaml");
    fout << config;
}
*/
static void testFuzzy(void)
{
    for (int i = 0; i < 5; i++)
    {
        const float x = ((float)i) / 4;
        cout << "not(" << x << ") = " << fznot(x) << "\n";
    }

    cout << "\n";

    for (int i = 0; i < 5; i++)
        for (int j = 0; j < 5; j++)
        {
            const float a = ((float)i) / 4;
            const float b = ((float)j) / 4;
            cout << "and(" << a << ", " << b << ") = " << fzand(a, b) << "\n";
        }

    cout << "\n";

    for (int i = 0; i < 5; i++)
        for (int j = 0; j < 5; j++)
        {
            const float a = ((float)i) / 4;
            const float b = ((float)j) / 4;
            cout << "or(" << a << ", " << b << ") = " << fzor(a, b) << "\n";
        }
}

static void testMap()
{
    map<const string, float> values;
    values["ciao"] = 0;
    values["ciao1"] = 1;

    for (auto it = values.begin(); it != values.end(); ++it)
    {
        cout << it->first << " -> " << it->second << "\n";
    }
}

static void dump(const fredy::Context &context)
{
    if (context.empty())
    {
        cout << "empty\n";
    }
    else
    {
        for (auto it = context.begin(); it != context.end(); ++it)
        {
            cout << it->first << " -> " << it->second << "\n";
        }
    }
}

static void testInference()
{
    fredy::Predicate a("a");
    fredy::Predicate b("b");

    cout << "a=" << &a << "\n";
    cout << "b=" << &b << "\n";

    vector<const fredy::Expression *> terms;

    terms.push_back(&a);
    terms.push_back(&b);

    auto exp = fredy::And(terms);

    for (int i = 0; i < 5; i++)
        for (int j = 0; j < 5; j++)
        {
            const float av = ((float)i) / 4;
            const float bv = ((float)j) / 4;
            map<const string, float> values;
            values["a"] = av;
            values["b"] = bv;
            cout << "and(" << av << ", " << bv << ") = " << exp.evaluate(values) << "\n";
        }

    auto exp1 = fredy::Or(terms);

    for (int i = 0; i < 5; i++)
        for (int j = 0; j < 5; j++)
        {
            const float av = ((float)i) / 4;
            const float bv = ((float)j) / 4;
            map<const string, float> values;
            values["a"] = av;
            values["b"] = bv;
            cout << "or(" << av << ", " << bv << ") = " << exp1.evaluate(values) << "\n";
        }

    fredy::Assertions assertions;

    assertions["c"] = &exp1;

    fredy::Inference inference(assertions);

    map<const string, float> facts;
    auto results = inference.evaluate(facts);

    dump(results);
}

static void testVar(const string &data...)
{
    va_list args;
    va_start(args, data);

    for (;;)
    {
        auto arg = va_arg(args, const char *);
        if (arg == NULL)
        {
            break;
        }
        cout << arg << "\n";
    }
}

int main(const int argc, const char *argv[])
{
    //    testFuzzy();
    // testMap();
    //    testInference();
    fredy::ExpBuilder builder = fredy::NotBuilder(
        &fredy::PredicateBuilder("a"));
}
