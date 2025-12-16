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
 * File name: CombinationTrie.h
 * Date:      2014/03/06 13:25
 * Author:    
 */

#ifndef __LFL__SEARCH__COMBINATIONTRIE_H__
#define __LFL__SEARCH__COMBINATIONTRIE_H__


#include <common.h>
#include <vector>


namespace lfl { namespace search {


template <typename T> class Node {
private:
    typedef Node<T> NodeClass;
    typedef std::vector<NodeClass*> NodeVector;

public:
    T m_value;
    NodeVector m_children;


    Node(T value, size_t childrenSize) :
        m_value(value),
        m_children(childrenSize, NULL)
    { }


    ~Node() {
        for (typename NodeVector::iterator i = m_children.begin(); i != m_children.end(); i++) {
            delete *i;
        }
    }

    
    template <class InputIterator>
    void put(InputIterator first, InputIterator last, T value, size_t childrenSize) {
        if (first == last) {
            // put value here
            m_value = value;
        }
        else {
            // put value to some descendant
            size_t childId = *first;
            assert(childId < m_children.size());
            Node<T>* childNode = m_children[childId];
            first++;

            if (!childNode) {
                // child node does not exist, so we assume it is the last node along the path
                // = create child and put the value into it
                assert(first == last); 
                childNode = new Node<T>(value, childrenSize);
                m_children[childId] = childNode;
            }
            else {
                // child node exists, dive into it
                childNode->put(first, last, value, childrenSize);
            }
        }
    }


    template <class InputIterator>
    const T* get(InputIterator first, InputIterator last, long skipIndex) const {
        if (first == last) {
            return &m_value;
        }
        if (skipIndex == 0) {
            first++;
            return get(first, last, skipIndex - 1);
        }

        size_t childId = *first;
        assert(childId < m_children.size());
        Node<T>* childNode = m_children[childId];
        if (!childNode) {
            return NULL;
        }

        first++;
        return childNode->get(first, last, skipIndex - 1);
    }
};



template <typename T> class CombinationTrie {
private:
    Node<T> m_root;


public:
    CombinationTrie(T rootValue, size_t count) :
        m_root(rootValue, count)
    { }


    void put(const IdSet key, T value, size_t childrenSize) {
        m_root.put(key.rbegin(), key.rend(), value, childrenSize);
    }


    void put(const IdSet key, T value) {
        m_root.put(key.rbegin(), key.rend(), value, *(key.begin()));
    }


    //const T* get(const IdSet key) const {
        //return m_root.get(key.rbegin(), key.rend(), key.size());
    //}


    /**
     * skipIndex tells which Id in key (counted in reverse order, i.e.
     * from last to first) to skip. I.e. get({10, 20, 30, 40}, 1) skips 30.
     */
    const T* get(const IdSet key, long skipIndex = -1) const {
        assert(skipIndex < (long) key.size());
        assert(skipIndex >= -1L);
        return m_root.get(key.rbegin(), key.rend(), skipIndex);
    }
};

}}
#endif
