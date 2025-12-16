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
 * File name: Attribute.h
 * Date:      2013/11/05 06:22
 * Author:
 */

#ifndef __LFL__SEARCH__ATTRIBUTE_H__
#define __LFL__SEARCH__ATTRIBUTE_H__


#include <common.h>
#include "../common/Chain.h"

#include <stdexcept>


namespace lfl { namespace search {


class Attribute {
private:
    /** 
     * ID of the fuzzy attribute
     * */
    size_t m_id;

    /**
     * Identifier of the numeric attribute which the current
     * attribute was created from. Multiple (fuzzy) Attributes
     * may be created from a single numeric attribute -- these
     * attributes would have the same m_variable ID.
     */
    int m_variable;

    /**
     * Support of the fuzzy attribute (i.e. mean of membership degrees that are
     * stored in m_chain.
     */
    double m_support;

    /**
     * Chain of membership degrees where i-th value of the chain corresponds
     * to a membership degree of the i-th object to this attribute.
     */
    lfl::Chain* m_chain;

public:
    Attribute(size_t id, int variable, size_t nRow) :
        m_id(id),
        m_variable(variable)
    { m_chain = new lfl::Chain(nRow); }

    ~Attribute()
    { delete m_chain; }

    size_t getId() const
    { return m_id; }

    lfl::Chain* getChain()
    { return m_chain; }

    int getVariable() const
    { return m_variable; }

    double getSupport() const
    { return m_support; }

    void initialize() {
        m_support = m_chain->mean();
    }
};


typedef std::vector<Attribute *> AttributeVector;

}}
#endif
