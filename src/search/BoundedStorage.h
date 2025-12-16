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
 * File name: BoundedStorage.h
 * Date:      2014/01/28 18:01
 * Author:    Michal Burda
 */

#ifndef __LFL__SEARCH__BOUNDEDSTORAGE_H__
#define __LFL__SEARCH__BOUNDEDSTORAGE_H__


#include <common.h>
#include "UnlimitedStorage.h"


namespace lfl { namespace search {


class BoundedStorage : public UnlimitedStorage {
protected:
    size_t m_max;


public:
    BoundedStorage(bool bestIsMax, size_t statisticIndex, size_t max) :
        UnlimitedStorage(bestIsMax, statisticIndex), m_max(max)
    { }


    virtual void storeCandidate(Rule *rule) {

        if (m_queue.size() < m_max) {
            m_queue.push(rule);
        }
        else {
            double ruleStat = rule->getStatistics().get(m_statistic);
            double topStat = m_queue.top()->getStatistics().get(m_statistic);

            if ((m_bestIsMax && (ruleStat > topStat))
                        || (!m_bestIsMax && (ruleStat < topStat))) {
                Rule* r = m_queue.top();
                m_queue.pop();
                delete r;
                m_queue.push(rule);
            }
            else {
                delete rule;
            }
        }
    }
};


}}
#endif
