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
 * File name: AbstractStorage.h
 * Date:      2014/01/28 17:00
 * Author:    
 */

#ifndef __LFL__SEARCH__ABSTRACTSTORAGE_H__
#define __LFL__SEARCH__ABSTRACTSTORAGE_H__


#include <common.h>
#include "Rule.h"


namespace lfl { namespace search {


/**
 * This is an abstract storage of mined rules. The popRule() method should 
 * return rules in the order of significance of the rule (from least 
 * significant to most significant).
 */
class AbstractStorage {
public:
    virtual ~AbstractStorage() { };
    
    virtual void storeCandidate(Rule* rule) = 0;
    virtual Rule* popRule() = 0;
    virtual bool empty() = 0;
    virtual size_t size() = 0;
    virtual void printStatus() = 0;
    virtual void print() = 0;
};

}}
#endif
