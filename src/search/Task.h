/*
 * File name: Task.h
 * Date:      2014/02/04 08:32
 */

#ifndef __LFL__SEARCH__TASK_H__
#define __LFL__SEARCH__TASK_H__

#include <sstream>

#include <common.h>
#include "Attribute.h"
#include "Statistics.h"
#include "../common/Chain.h"


namespace lfl { namespace search {


class Task {
private:
    /**
     * Index to m_availableLhs denoting the current LHS predicate. Together
     * with m_lhsPrefix, it forms a complete LHS condition.
     */
    size_t m_currentLhsIndex;

    /**
     * Prefix of the current LHS condition. LHS is complete with m_currentLhsIndex.
     */
    IdSet m_lhsPrefix;

    /**
     * Available LHS predicates (each to try as m_currentLhsIndex).
     */
    IdVector m_availableLhs;

    /**
     * LHS predicates that have been tried already and have not been pruned,
     * so they are to be appended to LHS in the child nodes of the search
     * tree.
     */
    IdVector m_soFarLhs;


    /**
     * Index to m_availableRhs denoting the current RHS predicate.
     */
    size_t m_currentRhsIndex;

    /**
     * Available right sides of rules to be considered.
     */
    IdVector m_availableRhs;

    /**
     * Available right sides of rules to be considered, originally comming from parent
     * nodes of the search tree. m_availableRhs is created from them by calling the
     * resetRhs() method.
     */
    IdVector m_originalRhs;

    /**
     * RHS predicates that have been tried already and have not been pruned,
     * so they are to be used again in RHS of child nodes of the search tree.
     */
    IdVector m_soFarRhs;

    /**
     * Statistics of the current rule.
     */
    Statistics m_stats;

    /**
     * Timestamp of postponing -- each postponed task receives this number that
     * is incremented each time. Among postponed tasks, the one with lowest postpone
     * number is processed firstly. Tasks that were not yet postponed have 0.
     */
    unsigned long m_postpone;

    lfl::Chain* m_lhsChain;
    lfl::Chain* m_rhsChain;
    lfl::Chain* m_parentLhsChain;


public:
    Task() :
        m_currentLhsIndex(0),
        m_currentRhsIndex(0),
        m_postpone(0),
        m_lhsChain(NULL),
        m_rhsChain(NULL),
        m_parentLhsChain(NULL)
    { }


    Task(IdVector& lhs, IdVector& rhs) :
        m_currentLhsIndex(0),
        m_availableLhs(lhs),

        m_currentRhsIndex(0),
        m_originalRhs(rhs),
        m_postpone(0),
        m_lhsChain(NULL),
        m_rhsChain(NULL),
        m_parentLhsChain(NULL)
    { }


    ~Task() {
        if (m_lhsChain)
            delete m_lhsChain;
        if (m_rhsChain)
            delete m_rhsChain;
        if (m_parentLhsChain)
            delete m_parentLhsChain;
    }


    size_t getCurrentLhs() const {
        assert(m_currentLhsIndex < m_availableLhs.size());
        return m_availableLhs[m_currentLhsIndex];
    }


    IdSet& getLhsPrefix()
    { return m_lhsPrefix; }


    IdVector& getAvailableLhs()
    { return m_availableLhs; }


    IdVector& getSoFarLhs()
    { return m_soFarLhs; }


    size_t getCurrentRhs() const {
        assert(m_currentRhsIndex < m_availableRhs.size());
        return m_availableRhs[m_currentRhsIndex];
    }


    IdVector& getAvailableRhs()
    { return m_availableRhs; }


    IdVector& getOriginalRhs()
    { return m_originalRhs; }


    IdVector& getSoFarRhs()
    { return m_soFarRhs; }


    Statistics& getStatistics()
    { return m_stats; }


    lfl::Chain* getLhsChain()
    { return m_lhsChain; }


    lfl::Chain* getRhsChain()
    { return m_rhsChain; }


    lfl::Chain* getParentLhsChain()
    { return m_parentLhsChain; }


    size_t getLhsLength() const
    {
        size_t length = 0;

        if (m_currentLhsIndex < m_availableLhs.size()) {
            length++; // something is in LHS
        }
        return m_lhsPrefix.size() + length;
    }


    void setCurrentLhsIndex(size_t currentIndex) {
        assert(currentIndex < m_availableLhs.size());
        m_currentLhsIndex = currentIndex;
    }


    void setCurrentRhsIndex(size_t currentIndex) {
        assert(currentIndex < m_availableRhs.size());
        m_currentRhsIndex = currentIndex;
    }


    void setLhsChain(lfl::Chain* chain) {
        if (m_lhsChain)
            delete m_lhsChain;
        m_lhsChain = chain;
    }


    void setRhsChain(lfl::Chain* chain) {
        if (m_rhsChain)
            delete m_rhsChain;
        m_rhsChain = chain;
    }


    void setParentLhsChain(lfl::Chain* chain) {
        if (m_parentLhsChain)
            delete m_parentLhsChain;
        m_parentLhsChain = chain;
    }


    void resetLhs() {
        m_currentLhsIndex = 0;
        m_soFarLhs.clear();
    }


    bool hasLhs() const
    { return m_currentLhsIndex < m_availableLhs.size(); }


    bool isEmptyLhs() const
    { return m_lhsPrefix.size() <= 0 && m_availableLhs.size() <= 0; }


    void nextLhs() {
        m_currentLhsIndex++;
    }


    void putSoFarLhs()
    { m_soFarLhs.push_back(getCurrentLhs()); }


    void resetRhs(const IdSet& otherRhs) {
        m_currentRhsIndex = 0;
        m_soFarRhs.clear();
        m_availableRhs.clear();
        for (IdVector::const_iterator it = m_originalRhs.begin(); it != m_originalRhs.end(); it++) {
            if (otherRhs.find(*it) != otherRhs.end()) {
                m_availableRhs.push_back(*it);
            }
        }
    }


    void resetRhs() {
        m_currentRhsIndex = 0;
        m_soFarRhs.clear();
        m_availableRhs = m_originalRhs;
    }


    bool hasRhs() const
    { return m_currentRhsIndex < m_availableRhs.size(); }


    void nextRhs() {
        m_currentRhsIndex++;
    }


    void putSoFarRhs()
    { m_soFarRhs.push_back(getCurrentRhs()); }


    void initializeChildTask(Task* child) {
        child->m_availableLhs = m_soFarLhs;
        child->m_originalRhs = m_soFarRhs;
        child->m_lhsPrefix = m_lhsPrefix;

        if (!isEmptyLhs()) {
            child->m_lhsPrefix.insert(getCurrentLhs());
            child->m_parentLhsChain = m_lhsChain->copy();
        }
    }


    unsigned long getPostpone() const {
        return m_postpone;
    }


    void setPostpone(unsigned long postpone) {
        m_postpone = postpone;
    }


    //TODO: consider adding pointer to vector of variables to each Task
    // - then this function would be very simplified
    bool lhsPrefixHasVariable(IdType variable, const IdVector &variables) const {
        for (int id : m_lhsPrefix) {
            if (variables[id] == variable)
                return true;
        }
        return false;
    }


    bool lhsPrefixHasAttribute(int attr) const
    { return (m_lhsPrefix.find(attr) != m_lhsPrefix.end()); }


    friend std::ostream& operator<< (std::ostream& stream, const Task& obj) {
        //stream << "lhs: " << obj.getCurrentLhs() << " & " << obj.m_lhsPrefix;
        //stream << "prefix: " << obj.m_lhsPrefix;
        //stream << ", availLhs size: " << obj.m_availableLhs.size() << " (curr index: " << obj.m_currentLhsIndex << ") ";
        //stream << ", soFarLhs size: " << obj.m_soFarLhs.size();
        //stream << ", availRhs size: " << obj.m_availableRhs.size() << " (curr index: " << obj.m_currentRhsIndex << ") ";
        //stream << ", soFarRhs size: " << obj.m_soFarRhs.size();
        //stream << " (" << obj.m_stats << ")";

        return stream;
    }


    friend std::ostream& operator<< (std::ostream& stream, const Task* obj) {
        stream << *obj;
        return stream;
    }
};


}}
#endif
