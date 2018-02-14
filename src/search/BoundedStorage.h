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
