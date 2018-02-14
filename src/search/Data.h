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
