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
