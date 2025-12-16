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
 * File name: UnlimitedStorage.h
 * Date:      2014/01/28 17:31
 * Author:    Michal Burda
 */

#ifndef __LFL__SEARCH__UNLIMITEDSTORAGE_H__
#define __LFL__SEARCH__UNLIMITEDSTORAGE_H__


#include <common.h>
#include "AbstractStorage.h"
#include "RuleQueue.h"


namespace lfl { namespace search {


class UnlimitedStorage : public AbstractStorage {
protected:
    size_t m_statistic;
    size_t m_max;
    bool m_bestIsMax;
    RuleQueue m_queue;


public:
    UnlimitedStorage(bool bestIsMax, int statisticIndex) :
        m_queue(bestIsMax, statisticIndex) {
        m_statistic = statisticIndex;
        m_bestIsMax = bestIsMax;
    }


    virtual ~UnlimitedStorage() {
        while (!m_queue.empty()) {
            delete m_queue.top();
            m_queue.pop();
        }
    }


    virtual Rule* popRule() {
        Rule* r = m_queue.top();
        m_queue.pop();
        return r;
    }


    virtual bool empty() {
        return m_queue.empty();
    }


    virtual size_t size() {
      return m_queue.size();
    }


    virtual void printStatus() {
    }


    virtual void storeCandidate(Rule *rule) {
        m_queue.push(rule);
    }


    virtual void print() {
        RuleQueue backup(m_bestIsMax, m_statistic);
        while (!m_queue.empty()) {
            Rule* r = m_queue.top();
            m_queue.pop();
            backup.push(r);
            //r->print();
            //std::cout << std::endl;
        }
        m_queue = backup;
    }
};

}}
#endif
