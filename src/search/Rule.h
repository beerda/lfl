/*
 * File name: Rule.h
 * Date:      2013/09/11 08:12
 * Author:    Michal Burda
 */

#ifndef __LFL__SEARCH__RULE_H__
#define __LFL__SEARCH__RULE_H__

#include <common.h>
#include "Attribute.h"
#include "Statistics.h"

#include <set>


namespace lfl { namespace search {


class Rule {
protected:
    IdSet m_lhs;
    int m_rhs;
    Statistics m_stats;


public:
    Rule()
    { }


    Rule(IdSet &prefix, size_t lhs, size_t rhs, Statistics &stats) :
        m_lhs(prefix), m_rhs(rhs), m_stats(stats)
    { m_lhs.insert(lhs); }


    Rule(int rhs, Statistics &stats) :
        m_rhs(rhs), m_stats(stats)
    { }


    IdSet& getLhs()
    { return m_lhs; }


    int getRhs()
    { return m_rhs; }


    Statistics& getStatistics()
    { return m_stats; }


    /*
    DblVector serialize() const {
        DblVector buff(m_lhs.size() + 2 + sizeof(Statistics) / sizeof(double));
        DblVector::iterator it = buff.begin();

        *it++ = m_lhs.size();
        std::copy(m_lhs.begin(), m_lhs.end(), it);
        it += m_lhs.size();

        *it++ = m_rhs;

        *it++ = m_stats.support;
        *it++ = m_stats.supportLhs;
        *it++ = m_stats.supportRhs;
        *it++ = m_stats.confidence;
        *it++ = m_stats.lift;

        return buff;
    }


    static Rule *deSerialize(const DblVector &buffer) {
        DblVector::const_iterator it = buffer.begin();

        IdSet lhs;
        int rhs;
        Statistics stats;

        size_t lhsSize = *it++;
        std::copy(it, it + lhsSize, inserter(lhs, lhs.begin()));
        it += lhsSize;

        rhs = *it++;

        stats.support = *it++;
        stats.supportLhs = *it++;
        stats.supportRhs = *it++;
        stats.confidence = *it++;
        stats.lift = *it++;

        return new Rule(lhs, nullId, rhs, stats);
    }
*/

    friend std::ostream& operator<< (std::ostream& stream, const Rule& obj) {
        //stream << obj.m_lhs << " => " << obj.m_rhs << " (" << obj.m_stats << ")";

        return stream;
    }


    friend std::ostream& operator<< (std::ostream& stream, const Rule* obj) {
        //stream << *obj;
        return stream;
    }
};

}}
#endif
