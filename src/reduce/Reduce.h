/*
 * File name: Reduce.h
 * Date:      2015/01/09 19:08
 * Author:    
 */

#ifndef __LFL__REDUCE__REDUCE_H__
#define __LFL__REDUCE__REDUCE_H__


#include <common.h>

#include "../common/Data.h"
#include "../common/ChainCombiner.h"
#include "ReduceConfig.h"
#include "RuleQueue.h"


namespace lfl { namespace reduce {


typedef std::vector<Rule*> RuleVector;
typedef std::map<IdType, RuleVector> ConsequentMap;


class Reduce {
protected:
    ReduceConfig& m_config;

    lfl::Data m_data;

    RuleVector m_input;

    RuleVector m_output;

    ConsequentMap m_consequentMap;

    size_t count;


    float computeCoverage(RuleVector& rules) {
        lfl::ChainCombiner* conj = m_config.getConjunction();
        lfl::ChainCombiner* disj = m_config.getDisjunction();
        lfl::Chain chain(m_config.getRowCount(), 0);

        for (size_t row = 0; row < m_config.getRowCount(); row++) {
            for (RuleVector::iterator i = rules.begin(); i != rules.end(); i++) {
                Rule* rule = *i;
                float fire = 1;
                for (IdSet::iterator j=rule->getAntecedent().begin(); j != rule->getAntecedent().end(); j++) {
                    fire = conj->combine(fire, m_data.getValue(row, *j));
                }
                chain.set(row, disj->combine(fire, chain.get(row)));
                if (chain.get(row) >= 1) {
                    break;
                }
            }
        }

        return chain.sum() / m_config.getRowCount();
    }


    lfl::Chain* createUpdatedChain(lfl::Chain* chain, Rule* rule) {
        lfl::Chain* result = NULL;

        count++;

        for (IdSet::iterator j=rule->getAntecedent().begin(); j != rule->getAntecedent().end(); j++) {
            if (!result) {
                result = m_data.getChain(*j)->copy();
            } else {
                result->combineWith(m_data.getChain(*j), m_config.getConjunction());
            }
        }

        if (!result) {
            result = new lfl::Chain(m_config.getRowCount(), 1);
        } else {
            result->combineWith(chain, m_config.getDisjunction());
        }
        return result;
    }


    void initializeConsequentMap() {
        for (RuleVector::iterator i = m_input.begin(); i != m_input.end(); i++) {
            Rule* rule = *i;
            m_consequentMap[rule->getConsequent()].push_back(rule);
        }
    }


    void reduceSubBases() {
#ifdef _OPENMP
        int nthreads = m_config.getNumThreads();
        #pragma omp parallel num_threads(nthreads) default(shared)
#endif
        for (ConsequentMap::iterator cit=m_consequentMap.begin(); cit != m_consequentMap.end(); cit++) {
            #pragma omp single nowait
            {
                RuleVector& rules = cit->second;
                RuleQueue queue;
                for (RuleVector::iterator rit = rules.begin(); rit != rules.end(); rit++) {
                    queue.push(*rit);
                }
                float coverage = computeCoverage(rules);
                //std::cout << "coverage is " << coverage << std::endl;
                reduceLoop(queue, coverage * m_config.getRatio());
            }
        }
    }


    void reduceLoop(RuleQueue& queue, float threshold) {
        lfl::Chain* rbChain = new lfl::Chain(m_config.getRowCount(), 0);
        float rbCoverage = 0;

        Rule* bestRule = NULL;
        lfl::Chain* bestChain = NULL;
        float bestCoverage = 0;

        IdType timestamp = 0;

        while (rbCoverage < threshold) {
            while (queue.top() != bestRule) {
                Rule* rule = queue.top();
                queue.pop();
                lfl::Chain* ruleChain = createUpdatedChain(rbChain, rule);
                float ruleCoverage = ruleChain->sum() / m_config.getRowCount();
                rule->setPotential(ruleCoverage - rbCoverage, ++timestamp);
                queue.push(rule);

                //std::cout << "Top rule " << rule->getId() << " coverage = " << ruleCoverage << 
                    //" potential = " << (ruleCoverage - rbCoverage) << std::endl;

                if (ruleCoverage > bestCoverage) {
                    //std::cout << "(was best)" << std::endl;
                    if (bestChain) {
                        delete bestChain;
                    }
                    bestChain = ruleChain;
                    bestCoverage = ruleCoverage;
                    bestRule = rule;
                } else {
                    delete ruleChain;
                }
            }

            //std::cout << "Overall best rule " << bestRule->getId() << " coverage = " << bestCoverage << 
                    //" potential = " << (bestCoverage - rbCoverage) << std::endl;

            queue.pop();

            #pragma omp critical(PUSH_REDUCED)
            {
                m_output.push_back(bestRule);
            }
            delete rbChain;
            rbChain = bestChain;
            rbCoverage = bestCoverage;

            bestRule = NULL;
            bestChain = NULL;
            bestCoverage = 0;
        }

        delete rbChain;
    }


public:
    Reduce(ReduceConfig& config) :
        m_config(config),
        m_data(config.getRowCount(), config.getColCount()),
        count(0)
    { }


    virtual ~Reduce() {
        for (RuleVector::iterator i = m_input.begin(); i != m_input.end(); i++) {
            delete *i;
        }
    }


    lfl::Data& getData()
    { return m_data; }


    RuleVector& getInputRules()
    { return m_input; }


    RuleVector& getOutputRules()
    { return m_output; }


    virtual Rule* createRule(IdType id) {
        Rule* rule = new Rule();
        rule->setId(id);
        return(rule);
    }


    virtual void run() {
        initializeConsequentMap();
        reduceSubBases();
        //std::cout << "***** Count: " << count << std::endl;
    }
};


}}
#endif
