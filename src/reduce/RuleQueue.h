/*
 * File name: RuleQueue.h
 * Date:      2015/01/13 10:06
 * Author:    
 */

#ifndef __LFL__REDUCE__RULEQUEUE_H__
#define __LFL__REDUCE__RULEQUEUE_H__


#include <common.h>
#include "Rule.h"

#include <queue>


namespace lfl { namespace reduce {


class RuleComparison {
public:
    RuleComparison() {
    }

    bool operator()(Rule* lhs, Rule* rhs) {
        if (lhs->getPotential() == rhs->getPotential()) {
            return (lhs->getPotentialTimestamp() > rhs->getPotentialTimestamp());
        }
        return (lhs->getPotential() < rhs->getPotential());
    }
};


class RuleQueue : public std::priority_queue<Rule*, std::vector<Rule*>, RuleComparison> {
public:
    RuleQueue() : priority_queue(RuleComparison())
    { }
};

}}
#endif
