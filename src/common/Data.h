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
 * Date:      2015/01/23 08:23
 * Author:    Michal Burda
 */

#ifndef __LFL__COMMON__DATA_H__
#define __LFL__COMMON__DATA_H__


#include <common.h>
#include <vector>
#include "Chain.h"


namespace lfl {


class Data {
private:
    std::vector<Chain*> m_chains;
    

public:
    Data(size_t rowCount, size_t colCount)
    {
        m_chains.resize(colCount);
        for (size_t col = 0; col < colCount; col++) {
            m_chains[col] = new Chain(rowCount);
        }
    }


    ~Data() {
        for (Chain *a : m_chains) {
            delete a;
        }
    }


    void setValue(size_t row, size_t col, float value) {
        m_chains[col]->set(row, value);
    }


    float getValue(size_t row, size_t col) const {
        return m_chains[col]->get(row);
    }


    Chain* getChain(size_t i) {
        return m_chains[i];
    }
};

}
#endif
