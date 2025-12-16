/**********************************************************************
 * lfl: Linguistic Fuzzy Logic
 * Copyright (C) 2025 Michal Burda
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 **********************************************************************/


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
