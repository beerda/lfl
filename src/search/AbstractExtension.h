/*
 * File name: AbstractExtension.h
 * Date:      2014/02/04 13:07
 */

#ifndef __LFL__SEARCH__ABSTRACTEXTENSION_H__
#define __LFL__SEARCH__ABSTRACTEXTENSION_H__


#include <common.h>
#include "Task.h"
#include "SearchConfig.h"
#include "Data.h"


namespace lfl { namespace search {


class AbstractExtension {
protected:
    AbstractExtension* m_prevExtension;


    /**
     * Configuration of the search algorithm
     */
    SearchConfig* m_config;

    /**
     * Data stored in the form of fuzzy chains
     */
    Data* m_data;


public:
    AbstractExtension(AbstractExtension* prevExtension = NULL) :
        m_prevExtension(prevExtension)
    { }


    virtual ~AbstractExtension()
    { }


    void setConfig(SearchConfig* config) { 
        m_config = config;
        if (m_prevExtension) {
            m_prevExtension->setConfig(config);
        }
    }


    void setData(Data* data) {
        m_data = data;
        if (m_prevExtension) {
            m_prevExtension->setData(data);
        }
    }


    virtual void initialize() {
        if (m_prevExtension) {
            m_prevExtension->initialize();
        }
    }


    /**
     * This method is for initialization of the vector of available RHS attributes.
     * It returns TRUE on success. If FALSE is returned, processing of the task
     * is postponed until TRUE is returned from this method.
     */
    virtual bool initializeRhs(Task* task) { 
        if (m_prevExtension) {
            return m_prevExtension->initializeRhs(task);
        }
        return true;
    }


    /**
     * This method is for pruning LHS before any statistics are computed
     * (TRUE to prune LHS, FALSE to not prune).
     */
    virtual bool isRedundantLhs(Task* task) {
        if (m_prevExtension) {
            return m_prevExtension->isRedundantLhs(task);
        }
        return false;
    }

    /**
     * Compute statistics related to LHS. This method is called after 
     * isRedundantLhs() but before isPrunableLhs().
     */
    virtual void computeLhsStatistics(Task* task) {
        if (m_prevExtension) {
            m_prevExtension->computeLhsStatistics(task);
        }
    }

    /**
     * This method is for pruning LHS after LHS statistics are computed
     * (TRUE to prune LHS).
     */
    virtual bool isPrunableLhs(Task* task) {
        if (m_prevExtension) {
            return m_prevExtension->isPrunableLhs(task);
        }
        return false;
    }

    /** 
     * Called after LHS statistics are computed but before RHS statistic
     * computations (TRUE to prune RHS).
     */
    virtual bool isRedundantRhs(Task* task) {
        if (m_prevExtension) {
            return m_prevExtension->isRedundantRhs(task);
        }
        return false;
    }

    /**
     * Compute statistics related to RHS. This method is called after 
     * isRedundantRhs() but before isPrunableRhs().
     */
    virtual void computeRhsStatistics(Task* task) {
        if (m_prevExtension) {
            m_prevExtension->computeRhsStatistics(task);
        }
    }

    /**
     * Called after both LHS and RHS statistics are computed
     * (TRUE to prune RHS).
     */
    virtual bool isPrunableRhs(Task* task) {
        if (m_prevExtension) {
            return m_prevExtension->isPrunableRhs(task);
        }
        return false;
    }

    /**
     * Called after all prunings, this method determines whether the rule
     * represented by the given task should be sent to the output, i.e.
     * whether it is a candidate on a interesting rule. This method only
     * affects the storage of the rule, it does not influence further
     * search tree traversal (TRUE to store rule).
     */
    virtual bool isCandidate(Task* task) {
        if (m_prevExtension) {
            return m_prevExtension->isCandidate(task);
        }
        return true;
    }

    /**
     * Called if pruning of the given ``LHS => RHS'' rule failed, e.g.
     * to send the rule to the output.
     */
    virtual void storeCandidate(Task* task) {
        if (m_prevExtension) {
            m_prevExtension->storeCandidate(task);
        }
    }

    /**
     * This method allows to prune RHS after a rule has been created,
     * i.e. after storeCandidate() has been called (FALSE to prune RHS).
     */
    virtual bool isOkToDiveRhs(Task* task) {
        if (m_prevExtension) {
            return m_prevExtension->isOkToDiveRhs(task);
        }
        return true;
    }

    /**
     * This method determines whether a child tasks have to be generated
     * and processed (FALSE to prune LHS).
     */
    virtual bool isOkToDiveLhs(Task* task) {
        if (m_prevExtension) {
            return m_prevExtension->isOkToDiveLhs(task);
        }
        return true;
    }

    /**
     * Called after isOkToDiveLhs() returns true to allow to store LHSs and RHSs
     * that may be used as a base for longer rules. 
     */
    virtual void storeDiveable(Task* task) {
        if (m_prevExtension) {
            m_prevExtension->storeDiveable(task);
        }
    }

    /**
     * Called after it is clear that the LHS can not be used in any child rule.
     */
    virtual void storeNonDiveable(Task* task) {
        if (m_prevExtension) {
            m_prevExtension->storeNonDiveable(task);
        }
    }
};

}}
#endif
