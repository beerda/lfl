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
