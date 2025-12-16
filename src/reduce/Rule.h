/**********************************************************************
 * lfl: Linguistic Fuzzy Logic
 * Copyright (C) 2025 Michal Burda
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 **********************************************************************/


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
