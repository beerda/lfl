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
