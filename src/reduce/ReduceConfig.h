/*
 * File name: ReduceConfig.h
 * Date:      2015/01/09 19:13
 * Author:    Michal Burda
 */

#ifndef __LFL__REDUCE__REDUCECONFIG_H__
#define __LFL__REDUCE__REDUCECONFIG_H__


#include <common.h>
#include "../common/ChainCombiner.h"


namespace lfl { namespace reduce {


class ReduceConfig {
private:
    unsigned int m_threads;

    /**
     * Number of records (rows)
     */
    size_t m_nrow;

    /**
     * Number of fuzzy attributes (columns)
     */
    size_t m_ncol;

    /**
     * T-norm used as conjunctions: 
     * 'p' = product, 'l' = Lukasiewicz, 'm' = minimum
     */
    char m_tNorm;


    /**
     * T-conorm used as disjunction: 
     * 'p' = product, 'l' = Lukasiewicz, 'm' = max
     */
    char m_tConorm;


    lfl::ChainCombiner* m_conjunction;


    lfl::ChainCombiner* m_disjunction;


    double m_ratio;


public:
    ReduceConfig() :
        m_threads(1),
        m_nrow(0),
        m_ncol(0),
        m_tNorm('m'),
        m_tConorm('m'),
        m_conjunction(NULL),
        m_disjunction(NULL),
        m_ratio(0.9)
    { }


    virtual ~ReduceConfig() {
        if (m_conjunction) {
            delete m_conjunction;
        }
        if (m_disjunction) {
            delete m_disjunction;
        }
    }


    unsigned int getNumThreads() const
    { return m_threads; }


    size_t getRowCount() const
    { return m_nrow; }


    size_t getColCount() const
    { return m_ncol; }


    char getTNorm() const
    { return m_tNorm; }


    char getTConorm() const
    { return m_tConorm; }


    double getRatio() const
    { return m_ratio; }


    lfl::ChainCombiner* getConjunction()
    { return m_conjunction; }


    lfl::ChainCombiner* getDisjunction()
    { return m_disjunction; }


    void setNumThreads(unsigned int threads)
    { m_threads = (threads < 1) ? 1 : threads; }


    void setRowCount(size_t rowCount)
    { m_nrow = rowCount; }


    void setColCount(size_t colCount)
    { m_ncol = colCount; }


    void setTNorm(char tNorm) { 
        m_tNorm = tNorm;
        if (m_conjunction) {
            delete m_conjunction;
        }
        m_conjunction = createConjunctionCombiner(tNorm);
    }


    void setTConorm(char tConorm) { 
        m_tConorm = tConorm;
        if (m_disjunction) {
            delete m_disjunction;
        }
        m_disjunction = createDisjunctionCombiner(tConorm);
    }


    void setRatio(double ratio)
    { m_ratio = ratio; }
};

}}
#endif
