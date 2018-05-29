/*
 * File name: Search.h
 * Date:      2013/09/09 12:52
 * Author:
 */


#ifndef __LFL__SEARCH__SEARCH_H__
#define __LFL__SEARCH__SEARCH_H__


// #include <iostream>

#include <common.h>
#include "SearchConfig.h"
#include "Data.h"
#include "Attribute.h"
#include "../common/Chain.h"
#include "Task.h"
#include "TaskQueue.h"
#include "AbstractExtension.h"


namespace lfl { namespace search {


class Search {
protected:
    /**
     * Configuration of the search algorithm
     */
    SearchConfig& m_config;

    /**
     * Data stored in the form of fuzzy chains
     */
    Data m_data;

    /**
     * Internal queue of tasks to be processed
     */
    TaskQueue m_taskQueue;

    /**
     * Extension that is responsible for pruning of rules and other
     * controlling of the search algorithm
     */
    AbstractExtension* m_extension;

    int m_working;


    /**
     * A factory method for allocation of a new Task
     */
    virtual Task* createTask() {
        return new Task();
    }


    /**
     * Initialize data before starting the search algorithm.
     */
    virtual void initializeData() {
        m_data.initialize();
    }


    /**
     * Initialize extensions before starting the search algorithm.
     */
    virtual void initializeExtension() {
        if (m_extension) {
            m_extension->setConfig(&m_config);
            m_extension->setData(&m_data);
            m_extension->initialize();
        }
    }


    /**
     * Initialize the search algorithm and set a starting task to the m_taskQueue.
     */
    virtual void initializeSearch() {
        Task* task = createTask();

        // empty LHS
        std::copy(m_config.getLhs().begin(), m_config.getLhs().end(),
                back_inserter(task->getSoFarLhs()));

        // RHS is available
        std::copy(m_config.getRhs().begin(), m_config.getRhs().end(),
                back_inserter(task->getOriginalRhs()));

        m_taskQueue.push(task);
    }


    virtual bool workDone() {
        bool r;

        #pragma omp critical(TASK_QUEUE)
        {
            r = m_taskQueue.empty() && m_working <= 0;
            //if (m_taskQueue.size() % 10 == 0)
                //std::cout << "queuesize: " << m_taskQueue.size() << "\n";
        }
        return r;
    }


    virtual void taskFinished() {
        #pragma omp critical(TASK_QUEUE)
        {
            m_working--;
        }
    }


    /**
     * Obtain task to be processed. If all tasks have been processed, return NULL.
     * This implementation simply gets a task from the top of the m_taskQueue.
     */
    virtual Task* receiveTask() {
        return popTask();
    }

    Task* popTask() {
        Task* task;

        #pragma omp critical(TASK_QUEUE)
        {
            if (m_taskQueue.empty()) {
                task = 0;
            }
            else {
                task = m_taskQueue.top();
                m_taskQueue.pop();
                m_working++;
            }
        }
        //std::cout << "Received task: " << task << std::endl;
        return task;
    }


    virtual void sendTask(Task* task) {
        pushTask(task);
    }

    void pushTask(Task* task) {
        //std::cout << "Sending task: " << task << std::endl;
        #pragma omp critical(TASK_QUEUE)
        {
            m_taskQueue.push(task);
        }
    }


    virtual void postponeTask(Task* task) {
        #pragma omp critical(TASK_QUEUE)
        {
            //std::cout << "Postponing task: " << task << std::endl;
            m_taskQueue.postpone(task);
        }
    }


    /**
     * Process the given task.
     */
    virtual void processTask(Task* task) {
        Task* child = NULL;
        bool diveableStored = false;

        if (!m_extension->initializeRhs(task)) {
            postponeTask(task);
        }
        else {
            //std::cout << "Processing task: " << task << std::endl;
            if (!m_extension->isRedundantLhs(task)) {
                updateLhsChain(task);
                m_extension->computeLhsStatistics(task);
                if (!m_extension->isPrunableLhs(task)) {

                    for (; task->hasRhs(); task->nextRhs()) {
                        if (!m_extension->isRedundantRhs(task)) {
                            updateRhsChain(task);
                            m_extension->computeRhsStatistics(task);
                            if (!m_extension->isPrunableRhs(task)) {
                                if (m_extension->isCandidate(task)) {
                                    m_extension->storeCandidate(task);
                                }
                                if (m_extension->isOkToDiveRhs(task)) {
                                    task->putSoFarRhs();
                                }
                            }
                        }
                    }

                    if (m_extension->isOkToDiveLhs(task)) {
                        m_extension->storeDiveable(task);
                        diveableStored = true;
                        if (task->getSoFarLhs().size() > 0) {
                            child = createTask();
                            task->initializeChildTask(child);
                        }
                        if (!task->isEmptyLhs()) {
                            task->putSoFarLhs();
                        }
                    }
                }
            }

            if (!diveableStored) {
                m_extension->storeNonDiveable(task);
            }

            task->nextLhs();
            if (task->hasLhs()) {
                sendTask(task);
            }
            else {
                delete task;
            }
            if (child) {
                sendTask(child);
            }
        }
    }

    /**
     * Perform t-norm computations in order to set m_lhsChain.
     */
    void updateLhsChain(Task* task) {
        if (task->hasLhs()) {
            // non-empty LHS
            Attribute *attr = m_data.getAttribute(task->getCurrentLhs());
            lfl::Chain* newChain = attr->getChain()->copy();
            if (task->getParentLhsChain() != NULL) {
                newChain->combineWith(task->getParentLhsChain(), m_config.getConjunction());
            }
            task->setLhsChain(newChain);
        }
        else {
            // LHS is empty, we do not have any chain
            task->setLhsChain(NULL);
        }
    }


    /**
     * Perform t-norm computations in order to set m_rhsChain.
     */
    void updateRhsChain(Task* task) {
        Attribute *attr = m_data.getAttribute(task->getCurrentRhs());
        lfl::Chain* newChain = attr->getChain()->copy();
        if (task->getLhsChain() != NULL) {
            newChain->combineWith(task->getLhsChain(), m_config.getConjunction());
        }
        task->setRhsChain(newChain);
    }


public:
    Search(SearchConfig& config, AbstractExtension* extension) :
        m_config(config),
        m_data(config.getRowCount(), config.getColCount(), config.getVariables()),
        m_extension(extension),
        m_working(0)
    { }


    virtual ~Search()
    { }


    Data& getData()
    { return m_data; }


    /**
     * Run the main search loop
     */
    virtual void run() {
        initializeData();
        initializeExtension();
        initializeSearch();

        runLoop();
    }


    virtual void runLoop() {
#ifdef _OPENMP
        int nthreads = m_config.getNumThreads();
        #pragma omp parallel num_threads(nthreads) default(shared)
#endif
        while (!workDone()) {
            Task* task = receiveTask();
            if (task) {
                processTask(task);
                taskFinished();
            }
        }
    }
};

}}
#endif
