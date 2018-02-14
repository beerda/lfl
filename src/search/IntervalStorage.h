/*
 * File name: IntervalStorage.h
 * Date:      2013/09/11 06:53
 * Author:    Michal Burda
 */

#ifndef __LFL__SEARCH__INTERVALSTORAGE_H__
#define __LFL__SEARCH__INTERVALSTORAGE_H__


#include <common.h>
#include "RuleQueue.h"
#include "AbstractStorage.h"

#include <queue>


namespace lfl { namespace search {


class IntervalStorage : public AbstractStorage {
protected:
    size_t m_loStatistic;
    size_t m_hiStatistic;
    size_t m_max;
    size_t m_rawCount;
    bool m_bestIsMax;
    RuleQueue m_hardQueue;
    RuleQueue m_softQueue;


public:
    IntervalStorage(bool bestIsMax, int loStatisticIndex, int hiStatisticIndex, int max) :
        m_loStatistic(loStatisticIndex),
        m_hiStatistic(hiStatisticIndex),
        m_max(max),
        m_rawCount(0),
        m_bestIsMax(bestIsMax),
        m_hardQueue(bestIsMax, loStatisticIndex),
        m_softQueue(bestIsMax, hiStatisticIndex)
    { }


    virtual Rule* popRule() {
        Rule* r = NULL;
        if (!m_softQueue.empty()) {
          r = m_softQueue.top();
          m_softQueue.pop();
        } else {
          r = m_hardQueue.top();
          m_hardQueue.pop();
        }
        return r;
    }


    virtual bool empty() {
        return m_softQueue.empty() && m_hardQueue.empty();
    }


    virtual size_t size() {
        return m_hardQueue.size() + m_softQueue.size();
    }


    virtual void printStatus() {
        //std::cout << "raw count: " << m_rawCount << std::endl;
        //std::cout << "real size: " << size() << std::endl;
    }


    virtual void storeCandidate(Rule* rule) {
        m_rawCount++;

        if (m_hardQueue.size() < m_max) {
            m_hardQueue.push(rule);

        } else if (rule->getStatistics().get(m_loStatistic) > 
                m_hardQueue.top()->getStatistics().get(m_loStatistic)) {
            Rule* r = m_hardQueue.top();
            m_hardQueue.pop();
            m_hardQueue.push(rule);
            if (r->getStatistics().get(m_hiStatistic) >= 
                    m_hardQueue.top()->getStatistics().get(m_loStatistic)) {
                m_softQueue.push(r);
            } else {
                delete r;
            }
            while (!m_softQueue.empty()) {
                Rule* r = m_softQueue.top();
                if (r->getStatistics().get(m_hiStatistic) <
                        m_hardQueue.top()->getStatistics().get(m_loStatistic)) {
                    m_softQueue.pop();
                    delete r;
                } else {
                    break;
                }
            }

        } else if (rule->getStatistics().get(m_hiStatistic) >
                m_hardQueue.top()->getStatistics().get(m_loStatistic)) {
            m_softQueue.push(rule);

        } else {
            delete rule;
        }
    }


    virtual void print() {
        //std::cout << "HardQueue: " << std::endl;
        RuleQueue backup(m_bestIsMax, m_loStatistic);
        while (!m_hardQueue.empty()) {
            Rule* r = m_hardQueue.top();
            m_hardQueue.pop();
            backup.push(r);
            //r->print();
            //std::cout << std::endl;
        }
        m_hardQueue = backup;

        //std::cout << "SoftQueue: " << std::endl;
        RuleQueue backup2(!m_bestIsMax, m_hiStatistic);
        while (!m_softQueue.empty()) {
            Rule* r = m_softQueue.top();
            m_softQueue.pop();
            backup2.push(r);
            //r->print();
            //std::cout << std::endl;
        }
        m_softQueue = backup2;
    }
};

}}
#endif
