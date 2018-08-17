/*
 * File name: common.h
 * Date:      2013/09/09 13:16
 * Author:    Michal Burda
 */


#ifndef __LFL__COMMON_H__
#define __LFL__COMMON_H__


// coment to switch off assertions:
//#undef NDEBUG

#include <assert.h>
#include <iostream>
#include <vector>
#include <set>
#include <algorithm>


#define printVector(x) { \
    for (auto v : x) { \
        std::cout << " " << v; \
    } \
}


enum { nullId = -1 };


typedef unsigned int IdType;

typedef std::vector<IdType> IdVector;
typedef std::set<IdType> IdSet;
typedef std::vector<double> DblVector;
typedef std::vector<std::vector<double> > Matrix;


inline std::ostream& operator<< (std::ostream& stream, const IdVector& obj) {
    bool first = true;
    for (IdVector::const_iterator it = obj.begin(); it != obj.end(); it++) {
        if (first) {
            first = false;
        }
        else {
            stream << " & ";
        }
        stream << *it;
    }
    return stream;
}


inline std::ostream& operator<< (std::ostream& stream, const IdSet& obj) {
    bool first = true;
    for (IdSet::const_iterator it = obj.begin(); it != obj.end(); it++) {
        if (first) {
            first = false;
        }
        else {
            stream << " & ";
        }
        stream << *it;
    }
    return stream;
}


#endif
