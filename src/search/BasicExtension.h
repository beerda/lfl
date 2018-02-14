/*
 * File name: BasicExtension.h
 * Date:      2014/02/04 13:31
 */

#ifndef __LFL__SEARCH__BASICEXTENSION_H__
#define __LFL__SEARCH__BASICEXTENSION_H__


#include <common.h>
#include "AbstractExtension.h"
#include "AbstractStorage.h"
#include "UnlimitedStorage.h"
#include "BoundedStorage.h"
#include "IntervalStorage.h"


namespace lfl { namespace search {


class BasicExtension : public AbstractExtension {
protected:
    /**
     * Rule storage
     */
    AbstractStorage* m_storage;


public:
    BasicExtension() :
        AbstractExtension(NULL),
        m_storage(0)
    { }


    virtual ~BasicExtension() { 
        if (m_storage)
            delete m_storage;
    }


    virtual void initialize() {
        AbstractExtension::initialize();

        bool bestIsMax;
        int by;

        if (m_config->getBestBy() == 'c') {
            by = INDEX_CONFIDENCE;
            bestIsMax = 1;
        }
        else if (m_config->getBestBy() == 'l') {
            by = INDEX_LIFT;
            bestIsMax = 1;
        }
        else throw std::runtime_error("Unknown bestBy");

        if (m_config->getRuleNumber() <= 0) {
            m_storage = new UnlimitedStorage(bestIsMax, by);
        }
        else if ((m_config->getBestBy() == 'l') && (m_config->getTNorm() != 'p')) {
            m_storage = new IntervalStorage(bestIsMax, INDEX_LO_LIFT, INDEX_HI_LIFT, 
                    m_config->getRuleNumber());
        }
        else {
            m_storage = new BoundedStorage(bestIsMax, by, m_config->getRuleNumber());
        }

    }


    virtual bool initializeRhs(Task* task) {
        if (!AbstractExtension::initializeRhs(task)) {
            return false;
        }
        task->resetRhs();
        
        return true;
    }


    /**
     * This method is for pruning LHS before any statistics are computed
     * (TRUE to prune LHS). This method returns TRUE if any predicate in prefix
     * is of the same variable as current predicate.
     */
    virtual bool isRedundantLhs(Task* task) { 
        if (AbstractExtension::isRedundantLhs(task)) {
            return true;
        }
        if (task->isEmptyLhs()) {
            return false;
        }
        if (task->getLhsLength() >= m_config->getMaxLength()) {
            return true;
        }
        if (task->lhsPrefixHasVariable(m_config->getVariables()[task->getCurrentLhs()], 
                    m_config->getVariables())) {
            return true;
        }

        return false;
    }


    /**
     * Compute statistics related to LHS. This method is called after 
     * isRedundantLhs() but before isPrunableLhs().
     */
    virtual void computeLhsStatistics(Task* task) {
        AbstractExtension::computeLhsStatistics(task);

        if (task->getLhsChain() == NULL) {
            task->getStatistics().supportLhs = 1.0;
        }
        else { 
            task->getStatistics().supportLhs = task->getLhsChain()->sum() / m_config->getRowCount();
        }
    }


    /**
     * This method is for pruning LHS after LHS statistics are computed
     * (TRUE to prune LHS). This method returns TRUE if the LHS support is lower
     * than minimum support threshold defined in m_config->
     */
    virtual bool isPrunableLhs(Task* task) {
        if (AbstractExtension::isPrunableLhs(task)) {
            return true;
        }
        return task->getStatistics().supportLhs < m_config->getMinSupport();
    }


    /** 
     * Called after LHS statistics are computed but before RHS statistic
     * computations (TRUE to prune RHS). This method returns TRUE if any
     * predicate in prefix is of the same variable as current target.
     */
    virtual bool isRedundantRhs(Task* task) {
        if (AbstractExtension::isRedundantRhs(task)) {
            return true;
        }
        if (!task->isEmptyLhs()) {
            IdVector variables = m_config->getVariables();
            // no need to test whether current lhs or lhs prefix has current rhs: it lasts
            // to test whether current lhs or prefix has variable of current rhs.
            if (variables[task->getCurrentLhs()] == variables[task->getCurrentRhs()]) {
                return true;
            }
            if (task->lhsPrefixHasVariable(variables[task->getCurrentRhs()], variables)){
                return true;
            }
        }
        return false;
    }


    /**
     * Compute statistics related to RHS. This method is called after 
     * isRedundantRhs() but before isPrunableRhs().
     */
    virtual void computeRhsStatistics(Task* task) {
        AbstractExtension::computeRhsStatistics(task);

        task->getStatistics().supportRhs = m_data->getAttribute(task->getCurrentRhs())->getSupport();
        task->getStatistics().support = task->getRhsChain()->sum() / m_config->getRowCount();
        task->getStatistics().confidence = task->getStatistics().support / task->getStatistics().supportLhs;
    }


    /**
     * Called after both LHS and RHS statistics are computed
     * (TRUE to prune RHS).
     */
    virtual bool isPrunableRhs(Task* task) {
        if (AbstractExtension::isPrunableRhs(task)) {
            return true;
        }
        return task->getStatistics().support < m_config->getMinSupport();
    }


    /**
     * Called after all prunings, this method determines whether the rule
     * represented by the given task should be sent to the output, i.e.
     * whether it is a candidate on a interesting rule. This method only
     * affects the storage of the rule, it does not influence further
     * search tree traversal (TRUE to store rule).
     */
    virtual bool isCandidate(Task* task) {
        if (!AbstractExtension::isCandidate(task)) {
            return false;
        }
        return (task->getStatistics().confidence >= m_config->getMinConfidence());
    }


    /**
     * Called if pruning of the given ``LHS => RHS'' rule failed, e.g.
     * to send the rule to the output.
     */
    virtual void storeCandidate(Task* task) {
        AbstractExtension::storeCandidate(task);

        Rule* rule;
        if (task->isEmptyLhs()) {
            assert(task->getLhsPrefix().size() <= 0);
            rule = new Rule(task->getCurrentRhs(), task->getStatistics());
        }
        else {
            rule = new Rule(task->getLhsPrefix(), task->getCurrentLhs(),
                    task->getCurrentRhs(), task->getStatistics());
        }

        #pragma omp critical(CANDIDATE_STORAGE)
        {
            m_storage->storeCandidate(rule);
        }
    }


    /**
     * This method allows to prune RHS after a rule has been created,
     * i.e. after storeCandidate() has been called (FALSE to prune RHS).
     */
    virtual bool isOkToDiveRhs(Task* task) {
        if (!AbstractExtension::isOkToDiveRhs(task)) {
            return false;
        }
        return task->getStatistics().confidence < m_config->getMaxConfidence();
    }


    /**
     * This method determines whether a child tasks have to be generated
     * and processed (FALSE to prune LHS).
     */
    virtual bool isOkToDiveLhs(Task* task) {
        if (!AbstractExtension::isOkToDiveLhs(task)) {
            return false;
        }

        if (task->getSoFarRhs().size() <= 0) {
            return false;
        }
        if (task->getLhsLength() + 1 >= m_config->getMaxLength()) {
            return false;
        }
        return true;
    }



    AbstractStorage* getStorage()
    { return m_storage; }
};

}}
#endif
