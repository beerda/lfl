/*
 * File name: TrieExtension.h
 * Date:      2014/09/04 14:30
 * Author:    
 */

#ifndef __LFL__SEARCH__TRIEEXTENSION_H__
#define __LFL__SEARCH__TRIEEXTENSION_H__


#include <common.h>
#include "AbstractExtension.h"
#include "CombinationTrie.h"


namespace lfl { namespace search {


class TrieExtension : public AbstractExtension {
protected:
    /**
     * Trie of LHS combinations that have been tested already.
     * Value is a set of available RHSs.
     */
    CombinationTrie<IdSet>* m_trie;


public:
    TrieExtension(AbstractExtension* prevExtension) :
        AbstractExtension(prevExtension),
        m_trie(0)
    { }


    virtual ~TrieExtension() { 
        if (m_trie)
            delete m_trie;
    }


    virtual void initialize() {
        AbstractExtension::initialize();
        const IdSet root = IdSet{};
        m_trie = new CombinationTrie<IdSet>(root, m_config->getColCount());
    }


    virtual bool initializeRhs(Task* task) {
        if (!AbstractExtension::initializeRhs(task)) {
            return false;
        }

        if (task->getLhsPrefix().size() > 0) {
            assert(task->hasLhs());

            IdSet check;
            check = task->getLhsPrefix();
            check.insert(task->getCurrentLhs());

            const IdSet* rhsIntersectionPtr;
            // TODO: some shared read-only lock?
            #pragma omp critical(TRIE_STORAGE)
            {
                rhsIntersectionPtr = m_trie->get(check, 0);
            }
            if (rhsIntersectionPtr == NULL) {
                return false;
            }

            IdSet rhsIntersection(*rhsIntersectionPtr);
            for (size_t i = 1; i < check.size(); i++) {
                const IdSet* obtainedRhs;
                // TODO: some shared read-only lock?
                #pragma omp critical(TRIE_STORAGE)
                {
                    obtainedRhs = m_trie->get(check, i);
                }
                if (obtainedRhs == NULL) {
                    return false;
                }

                // intersect rhsIntersection with obtainedRhs
                IdSet oldRhsIntersection = rhsIntersection;
                rhsIntersection.clear();
                std::set_intersection(
                    oldRhsIntersection.begin(), oldRhsIntersection.end(),
                    obtainedRhs->begin(), obtainedRhs->end(),
                    std::inserter(rhsIntersection, rhsIntersection.begin()));

            }
            task->resetRhs(rhsIntersection);
        }
        else {
            task->resetRhs();
        }
        
        return true;
    }


    /**
     * Called after isOkToDiveLhs() returns true to allow to store LHSs and RHSs
     * that may be used as a base for longer rules. 
     * 
     * This method stores current LHS and so-far-RHSs into CombinationTrie.
     */
    virtual void storeDiveable(Task* task) {
        AbstractExtension::storeDiveable(task);

        if (!task->isEmptyLhs()) {
            IdSet rhs(task->getSoFarRhs().begin(), task->getSoFarRhs().end());
            IdSet lhs(task->getLhsPrefix().begin(), task->getLhsPrefix().end());
            lhs.insert(task->getCurrentLhs());
            
            #pragma omp critical(TRIE_STORAGE)
            {
                m_trie->put(lhs, rhs, task->getCurrentLhs());
            }
        }
    }


    virtual void storeNonDiveable(Task* task) {
        AbstractExtension::storeNonDiveable(task);

        if (!task->isEmptyLhs()) {
            IdSet rhs;
            IdSet lhs(task->getLhsPrefix().begin(), task->getLhsPrefix().end());
            lhs.insert(task->getCurrentLhs());
            
            #pragma omp critical(TRIE_STORAGE)
            {
                m_trie->put(lhs, rhs, 0);
            }
        }
    }
};

}}
#endif
