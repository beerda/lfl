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
 * File name: ChainCombiner.h
 * Date:      2015/01/23 08:07
 * Author:    Michal Burda
 */

#ifndef __LFL__COMMON__CHAINCOMBINER_H__
#define __LFL__COMMON__CHAINCOMBINER_H__


#include <common.h>
#include <algorithm>


namespace lfl {


class ChainCombiner {
    public:
    virtual float combine(float x, float y) = 0;
    virtual ~ChainCombiner() { }
};


class LukConjunction : public ChainCombiner {
    public:
    virtual float combine(float x, float y) {
        return std::max(0.0f, x + y - 1.0f);
    }
};


class MinConjunction : public ChainCombiner {
    public:
    virtual float combine(float x, float y) {
        return std::min(x, y);
    }
};


class ProdConjunction : public ChainCombiner {
    public:
    virtual float combine(float x, float y) {
        return x * y;
    }
};


class MaxDisjunction : public ChainCombiner {
    public:
    virtual float combine(float x, float y) {
        return std::max(x, y);
    }
};


class ProdDisjunction : public ChainCombiner {
    public:
    virtual float combine(float x, float y) {
        return x + y - x * y;
    }
};


class LukDisjunction : public ChainCombiner {
    public:
    virtual float combine(float x, float y) {
        return std::min(x + y, 1.0f);
    }
};


inline ChainCombiner* createConjunctionCombiner(char type) {
    if (type == 'm') {
        return new MinConjunction();
    } else if (type == 'l') {
        return new LukConjunction();
    } else if (type == 'p') {
        return new ProdConjunction();
    }
    return NULL;
}


inline ChainCombiner* createDisjunctionCombiner(char type) {
    if (type == 'm') {
        return new MaxDisjunction();
    } else if (type == 'l') {
        return new LukDisjunction();
    } else if (type == 'p') {
        return new ProdDisjunction();
    }
    return NULL;
}

}
#endif
