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
 * File name: Data.h
 * Date:      2014/02/04 10:21
 */

#ifndef __LFL__SEARCH__DATA_H__
#define __LFL__SEARCH__DATA_H__


#include <common.h>
#include "Attribute.h"

#include <stdlib.h>


namespace lfl { namespace search {


class Data {
private:
    /**
     * Vector of fuzzy attributes
     */
    AttributeVector m_attributes;
    

public:
    Data(size_t rowCount, size_t colCount, IdVector& variables)
    {
        m_attributes.resize(colCount);
        for (size_t col = 0; col < colCount; col++) {
            Attribute *attribute =
                new Attribute(col, variables[col], rowCount);
            m_attributes[col] = attribute;
        }
    }


    ~Data() {
        for (Attribute *a : m_attributes) {
            delete a;
        }
    }


    void setValue(size_t row, size_t col, float value) {
        m_attributes[col]->getChain()->set(row, value);
    }


    float getValue(size_t row, size_t col) const {
        return m_attributes[col]->getChain()->get(row);
    }


    Attribute* getAttribute(size_t i) {
        return m_attributes[i];
    }


    void initialize() {
        for (Attribute* a : m_attributes) {
            a->initialize();
        }
    }
};

}}
#endif
