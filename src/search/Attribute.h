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
