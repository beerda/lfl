/*
 * File name: LiftExtension.h
 * Date:      2014/03/18 13:20
 * Author:    Michal Burda
 */

#ifndef __LFL__SEARCH__LIFTEXTENSION_H__
#define __LFL__SEARCH__LIFTEXTENSION_H__


#include <algorithm>

#include <common.h>
#include "AbstractExtension.h"


namespace lfl { namespace search {


class LiftExtension : public AbstractExtension {
private:
    /** 
     * Vector of membership degrees of RHS attributes. Index is ID of the attribute,
     * degrees are sorted in ascending order.
     */
    std::vector<std::vector<double> *> m_degrees;

    /**
     * Cummulative sum of membership degrees m_degrees.
     */
    std::vector<std::vector<double> *> m_cumSum;


    double computeLiftForMin(Task* task) {
        std::vector<double>* yDegrees = m_degrees[task->getCurrentRhs()];
        std::vector<double>* yCumSum = m_cumSum[task->getCurrentRhs()];

        size_t n = m_config->getRowCount();
        double r = 0;
        for (size_t xi = 0; xi < n; xi++) {
            double xValue = task->getLhsChain()->get(xi);
            std::vector<double>::iterator boundIter = std::lower_bound(yDegrees->begin(), yDegrees->end(), xValue);
            size_t loBound = boundIter - yDegrees->begin();
            r += xValue * (n - loBound);
            if (loBound > 0) {
                r += (*yCumSum)[loBound - 1];
            }
        }
        return n * n * task->getStatistics().support / r;
    }


    double computeLiftForLukasiewicz(Task* task) {
        std::vector<double>* yDegrees = m_degrees[task->getCurrentRhs()];
        std::vector<double>* yCumSum = m_cumSum[task->getCurrentRhs()];

        size_t n = m_config->getRowCount();
        double r = 0;
        for (size_t xi = 0; xi < n; xi++) {
            double xValue = task->getLhsChain()->get(xi);
            std::vector<double>::iterator boundIter = std::lower_bound(yDegrees->begin(),
                    yDegrees->end(), 1 - xValue);
            size_t loBound = boundIter - yDegrees->begin();
            r += xValue * (n - loBound) + (*yCumSum)[n - 1] - n + loBound;
            if (loBound > 0) {
                r -= (*yCumSum)[loBound - 1];
            }
        }
        return n * n * task->getStatistics().support / r;
    }


public:
    LiftExtension(AbstractExtension prevExtension) :
        AbstractExtension(prevExtension),
        m_degrees(0),
        m_cumSum(0)
    { }


    virtual ~LiftExtension() {
        for (std::vector<std::vector<double> *>::iterator it = m_degrees.begin(); it != m_degrees.end(); it++) {
            if (*it != NULL) {
                delete (*it);
            }
        }
        for (std::vector<std::vector<double> *>::iterator it = m_cumSum.begin(); it != m_cumSum.end(); it++) {
            if (*it != NULL) {
                delete (*it);
            }
        }
    }


    virtual void initialize() {
        AbstractExtension::initialize();

        if (m_config->getTNorm() != 'p') {
            // product t-norm does not need this stuff
            m_degrees.resize(m_config->getRhs().size(), NULL);
            m_cumSum.resize(m_config->getRhs().size(), NULL);

            for (IdVector::const_iterator it = m_config->getRhs().cbegin(); it != m_config->getRhs().cend(); it++) {
                std::vector<double>* degrees = new std::vector<double>(m_config->getRowCount());
                for (size_t i = 0; i < m_config->getRowCount(); i++) {
                    (*degrees)[i] = m_data->getValue(i, *it);
                }
                std::sort(degrees->begin(), degrees->end());
                m_degrees[*it] = degrees;

                std::vector<double>* cumSum = new std::vector<double>(m_config->getRowCount());
                double sum = 0;
                for (size_t i = 0; i < m_config->getRowCount(); i++) {
                    sum += (*degrees)[i];
                    (*cumSum)[i] = sum;
                }
                m_cumSum[*it] = cumSum;
            }
        }
    }


    virtual void computeRhsStatistics(Task* task) {
        AbstractExtension::computeRhsStatistics(task);

        if (m_config->getTNorm() == 'p') {
            // product t-norm
            task->getStatistics().lift = task->getStatistics().confidence / task->getStatistics().supportRhs;
            task->getStatistics().loLift = task->getStatistics().lift;
            task->getStatistics().hiLift = task->getStatistics().lift;
        }
        else if (m_config->getTNorm() == 'm') {
            // minimum t-norm
            task->getStatistics().loLift = task->getStatistics().support / 
                std::min(task->getStatistics().supportLhs, task->getStatistics().supportRhs);
            task->getStatistics().hiLift = task->getStatistics().support / 
                (task->getStatistics().supportLhs * task->getStatistics().supportRhs);
            task->getStatistics().lift = computeLiftForMin(task);
        }
        else {
            // lukasiewicz t-norm
            assert(m_config->getTNorm() == 'l');
            task->getStatistics().loLift = task->getStatistics().support / 
                (task->getStatistics().supportLhs * task->getStatistics().supportRhs);
            task->getStatistics().hiLift = task->getStatistics().support / 
                std::max(0.0, task->getStatistics().supportLhs + task->getStatistics().supportRhs - 1);
            task->getStatistics().lift = computeLiftForLukasiewicz(task);
        }
    }


};

}}
#endif
