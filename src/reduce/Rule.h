/*
 * File name: Rule.h
 * Date:      2015/01/12 06:17
 * Author:    
 */

#ifndef __LFL__REDUCE__RULE_H__
#define __LFL__REDUCE__RULE_H__


#include <common.h>
#include <set>


namespace lfl { namespace reduce {


class Rule {
protected:
    IdType m_id;

    float m_potential;

    IdType m_potentialTimestamp;

    IdType m_consequent;

    IdSet m_antecedent;


public:
    Rule()
    { }


    IdType getId() {
        return (m_id);
    }


    float getPotential() {
        return (m_potential);
    }


    IdType getPotentialTimestamp() {
        return (m_potentialTimestamp);
    }


    IdType getConsequent() {
        return (m_consequent);
    }


    IdSet& getAntecedent() {
        return (m_antecedent);
    }


    void setId(IdType id) {
        m_id = id;
    }


    void setPotential(float potential, IdType timestamp) {
        m_potential = potential;
        m_potentialTimestamp = timestamp;
    }


    void setConsequent(IdType conseq) {
        m_consequent = conseq;
    }
};

}}
#endif
