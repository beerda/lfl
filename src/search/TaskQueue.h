/*
 * File name: TaskQueue.h
 * Date:      2014/02/04 08:49
 */

#ifndef __LFL__SEARCH__TASKQUEUE_H__
#define __LFL__SEARCH__TASKQUEUE_H__


#include <common.h>
#include "Task.h"

#include <queue>


namespace lfl { namespace search {


class TaskComparison {
public:
    TaskComparison()
    { }


    bool operator()(Task* lhs, Task* rhs) {
        if (lhs->getPostpone() == 0 && rhs->getPostpone() == 0) {
            // both are not postponed
            return (lhs->getLhsLength() < rhs->getLhsLength());
        }
        return lhs->getPostpone() > rhs->getPostpone();
    }
};


class TaskQueue : public std::priority_queue<Task*, std::vector<Task*>, TaskComparison> {
private:
    unsigned long m_lastPostpone;


public:
    TaskQueue() :
        priority_queue(TaskComparison()),
        m_lastPostpone(0)
    { }


    void postpone(Task* task) {
        m_lastPostpone++;
        task->setPostpone(m_lastPostpone);
        push(task);
    }
};

}}
#endif
