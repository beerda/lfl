/*
 * File name: SearchConfig.h
 * Date:      2014/02/04 09:17
 */

#ifndef __LFL__SEARCH__SEARCHCONFIG_H__
#define __LFL__SEARCH__SEARCHCONFIG_H__


#include <common.h>

#include <stdlib.h>
#include <iostream>
#include "../common/ChainCombiner.h"


namespace lfl { namespace search {


/**
 * Class for initialization parameters of the search algorithm.
 */
class SearchConfig {
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

    lfl::ChainCombiner* m_conjunction;
    
    /**
     * Characteristic of which to find best rules:
     * 'c' = confidence, 'l' = lift
     */
    char m_bestBy;

    /**
     * How many rules to find
     */
    int m_ruleNumber;

    /**
     * IDs of variables that particular fuzzy attributes belong to.
     * 
     * For single numeric variable, there may exist multiple fuzzy attributes
     * that differ e.g. in linguistic expression (eg. very small vs. roughly
     * medium etc.). For this vector, index is an attribute's ID and value is an
     * ID of variable. I.e. if values on indices i and j are the same then
     * attributes i and j are fuzzy attributes that belong
     * to the same numeric variable.
     */
    IdVector m_variables;
    
    /**
     * Indices of fuzzy attributes to use in LHS part of a rule.
     */
    IdVector m_lhs;

    /**
     * Indices of fuzzy attributes to use in RHS part of a rule.
     */
    IdVector m_rhs;
    
    /** 
     * Minimum support threshold
     */
    double m_minSupport;

    /**
     * Minimum confidence threshold
     */
    double m_minConfidence;

    /**
     * Maximum confidence threshold
     */
    double m_maxConfidence;

    /**
     * Maximum length of a rule
     */
    size_t m_maxLength;

    /** 
     * Maximum of availables for recusrsive search
     */
    size_t m_recursionThreshold;


public:
    SearchConfig() :
        m_threads(1),
        m_nrow(0),
        m_ncol(0),
        m_tNorm('m'),
        m_conjunction(NULL),
        m_bestBy('c'),
        m_ruleNumber(20),
        m_minSupport(0.05),
        m_minConfidence(0.7),
        m_maxConfidence(1.0),
        m_maxLength(4),
        m_recursionThreshold(4)
    { }

    virtual ~SearchConfig() {
        if (m_conjunction) {
            delete m_conjunction;
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


    lfl::ChainCombiner* getConjunction() const
    { return m_conjunction; }

    char getBestBy() const
    { return m_bestBy; }


    size_t getRuleNumber() const
    { return m_ruleNumber; }


    //TODO: jak tu metodu udelat "const"?
    IdVector& getVariables()
    { return m_variables; }


    //TODO: jak tu metodu udelat "const"?
    IdVector& getLhs()
    { return m_lhs; }


    //TODO: jak tu metodu udelat "const"?
    IdVector& getRhs()
    { return m_rhs; }


    double getMinSupport() const
    { return m_minSupport; }


    double getMinConfidence() const
    { return m_minConfidence; }


    double getMaxConfidence() const
    { return m_maxConfidence; }


    size_t getMaxLength() const
    { return m_maxLength; }


    size_t getRecursionThreshold() const
    { return m_recursionThreshold; }

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


    void setBestBy(char bestBy)
    { m_bestBy = bestBy; }


    void setRuleNumber(size_t ruleNumber)
    { m_ruleNumber = ruleNumber; }


    void setVariables(IdVector& variables)
    { m_variables.assign(variables.begin(), variables.end()); }


    void setLhs(IdVector& lhs)
    { m_lhs.assign(lhs.begin(), lhs.end()); }


    void setRhs(IdVector& rhs)
    { m_rhs.assign(rhs.begin(), rhs.end()); }


    void setMinSupport(double minSupport)
    { m_minSupport = minSupport; }


    void setMinConfidence(double minConfidence)
    { m_minConfidence = minConfidence; }


    void setMaxConfidence(double maxConfidence)
    { m_maxConfidence = maxConfidence; }


    void setMaxLength(size_t maxLength)
    { m_maxLength = maxLength; }


    void setRecursionThreshold(size_t recursionThreshold)
    { m_recursionThreshold = recursionThreshold; }


    //TODO replace with operator<<
    void print() {
        std::cout << "variables:";
        printVector(m_variables);
        std::cout << std::endl;
        std::cout << "t-norm: " << m_tNorm << std::endl;
        std::cout << "bestBy: " << m_bestBy << std::endl;
        std::cout << "ruleNumber: " << m_ruleNumber << std::endl;
        std::cout << "LHS:";
        printVector(m_lhs);
        std::cout << std::endl;
        std::cout << "RHS:";
        printVector(m_rhs);
        std::cout << std::endl;
        std::cout << "minSupport: " << m_minSupport << std::endl;
        std::cout << "minConfidence: " << m_minConfidence << std::endl;
        std::cout << "maxConfidence: " << m_maxConfidence << std::endl;
        std::cout << "maxLength: " << m_maxLength << std::endl;
        std::cout << "recursionThreshold: " << m_recursionThreshold << std::endl;
    }
};

}}
#endif
