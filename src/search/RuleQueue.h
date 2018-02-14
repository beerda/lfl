/*
 * File name: RuleQueue.h
 * Date:      2014/01/28 11:30
 * Author:    Michal Burda
 */

#ifndef __LFL__SEARCH__RULEQUEUE_H__
#define __LFL__SEARCH__RULEQUEUE_H__


#include <common.h>
#include "Rule.h"

#include <queue>


namespace lfl { namespace search {


class RuleComparison {
private:
    int m_statIndex;
    bool m_reverse;

public:
    RuleComparison(const bool& reverse, int statisticIndex) {
        m_reverse = reverse;
        m_statIndex = statisticIndex;
    }

    bool operator()(Rule* lhs, Rule* rhs) {
        double ll = lhs->getStatistics().get(m_statIndex);
        double rr = rhs->getStatistics().get(m_statIndex);
        if (m_reverse)
            return (ll > rr);
        else
            return (ll < rr);
    }
};


//typedef priority_queue<Rule*, vector<Rule*>, RuleComparison> RuleQueue;
class RuleQueue : public std::priority_queue<Rule*, std::vector<Rule*>, RuleComparison> {
public:
    RuleQueue(bool bestIsMax, size_t statisticIndex) :
        priority_queue(RuleComparison(bestIsMax, statisticIndex))
    { }
};

}}
#endif
