/*
 * File name: Chain.h
 * Date:      2015/01/23 12:28
 * Author:    Michal Burda
 */

#ifndef __LFL__COMMON__CHAIN_H__
#define __LFL__COMMON__CHAIN_H__


#include <common.h>
#include <algorithm>
#include "ChainCombiner.h"


namespace lfl { 


class Chain {
    protected:
        size_t m_size;
        float* m_chain;


    public:
        Chain(size_t size) {
            m_size = size;
            m_chain = new float[size]();
        }

        Chain(size_t size, float value) {
            m_size = size;
            m_chain = new float[size];
            std::fill_n(m_chain, size, value);
        }

        virtual ~Chain() {
            delete[] m_chain;
        }

        virtual size_t size() {
            return m_size;
        }

        virtual void set(size_t position, float value) {
            if (value > 1.0) {
                value = 1.0;
            } else if (value < 0.0) {
                value = 0.0;
            }

            m_chain[position] = value;
        }

        virtual float get(size_t position) {
            return m_chain[position];
        }

        virtual float sum() {
            float sum = 0;
            for (size_t i = 0; i < m_size; i++) {
            sum += m_chain[i];
            }
            return sum;
        }

        virtual float mean() {
            return sum() / size();
        }

        virtual void combineWith(Chain* other, ChainCombiner* combiner) {
            for (size_t i = 0; i < m_size; i++) {
                m_chain[i] = combiner->combine(m_chain[i], other->m_chain[i]);
            }
        }

        virtual Chain* copy() {
            Chain* instance = new Chain(m_size);
            memcpy(instance->m_chain, m_chain, m_size * sizeof(*m_chain));
            return instance;
        }
};

}
#endif
