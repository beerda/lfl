/*
 * File name: DummyExtension.h
 * Date:      2014/02/04 13:07
 */

#ifndef __LFL__SEARCH__DUMMYEXTENSION_H__
#define __LFL__SEARCH__DUMMYEXTENSION_H__


#include <common.h>
#include "Task.h"
#include "SearchConfig.h"


namespace lfl { namespace search {


class DummyExtension : public AbstractExtension {
protected:
    SearchConfig m_config;


public:
    DummyExtension(SearchConfig& config) :
        m_config(config)
    { }

    virtual ~DummyExtension()
    { }

    /**
     * This method is for pruning LHS before any statistics are computed
     * (TRUE to prune LHS).
     */
    virtual bool isRedundantLhs(Task* task) 
    { return false; }

    /**
     * Compute statistics related to LHS. This method is called after 
     * isRedundantLhs() but before isPrunableLhs().
     */
    virtual void computeLhsStatistics(Task* task)
    { }

    /**
     * This method is for pruning LHS after LHS statistics are computed
     * (TRUE to prune LHS).
     */
    virtual bool isPrunableLhs(Task* task)
    { return false; }

    /** 
     * Called after LHS statistics are computed but before RHS statistic
     * computations (TRUE to prune RHS).
     */
    virtual bool isRedundantRhs(Task* task)
    { return false; }

    /**
     * Compute statistics related to RHS. This method is called after 
     * isRedundantRhs() but before isPrunableRhs().
     */
    virtual void computeRhsStatistics(Task* task)
    { }

    /**
     * Called after both LHS and RHS statistics are computed
     * (TRUE to prune RHS).
     */
    virtual bool isPrunableRhs(Task* task)
    { return false; }

    /**
     * Called after all prunings, this method determines whether the rule
     * represented by the given task should be sent to the output, i.e.
     * whether it is a candidate on a interesting rule. This method only
     * affects the storage of the rule, it does not influence further
     * search tree traversal (TRUE to store rule).
     */
    virtual bool isCandidate(Task* task)
    { return true; }

    /**
     * Called if pruning of the given ``LHS => RHS'' rule failed, e.g.
     * to send the rule to the output.
     */
    virtual void storeCandidate(Task* task)
    { }

    /**
     * This method allows to prune RHS after a rule has been created,
     * i.e. after storeCandidate() has been called (FALSE to prune RHS).
     */
    virtual bool isOkToAddTarget(Task* task)
    { return true; }

    /**
     * This method determines whether a child tasks have to be generated
     * and processed (FALSE to prune LHS).
     */
    virtual bool isOkToDive(Task* task)
    { return true; }
};

}}
#endif
