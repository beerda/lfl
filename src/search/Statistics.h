/*
 * File name: Statistics.h
 * Date:      2013/09/11 08:54
 * Author:
 */


#ifndef __LFL__SEARCH__STATISTICS_H__
#define __LFL__SEARCH__STATISTICS_H__


#include <common.h>

#define INDEX_SUPPORT     0
#define INDEX_SUPPORT_LHS 1
#define INDEX_SUPPORT_RHS 2
#define INDEX_CONFIDENCE  3
#define INDEX_LIFT        4
#define INDEX_LO_LIFT     5
#define INDEX_HI_LIFT     6
#define INDEX_END         7


namespace lfl { namespace search {


class Statistics {
public:
    // *** ALL STATISTICS HAVE TO BE double! (or get() will not work!)
    double support;
    double supportLhs;
    double supportRhs;
    double confidence;
    double lift;
    double loLift;
    double hiLift;

    Statistics() {
        // defaults are values for empty rule
        support = 1.0;
        supportLhs = 1.0;
        supportRhs = 1.0;
        confidence = 1.0;
        lift = 1.0;
        loLift = 1.0;
        hiLift = 1.0;
    }

    //TODO use union or only array with enumeration
    double get(int i) {
        double* a = &support;
        return a[i];
    }

    friend std::ostream& operator<< (std::ostream& stream, const Statistics& obj) {
        stream << "sup=" << obj.support << ", "
            << "supLhs=" << obj.supportLhs << ", "
            << "supRhs=" << obj.supportRhs << ", "
            << "conf=" << obj.confidence << ", "
            << "lift=" << obj.lift << ", "
            << "loLift=" << obj.loLift << ", "
            << "hiLift=" << obj.hiLift;
        return stream;
    }
};

}}
#endif
