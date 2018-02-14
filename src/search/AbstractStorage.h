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
