#pragma once

#include <vector>
#include <cstddef>

//---------------------------------------------------------------------------------------
class ATQPTask
{
friend class ATQPTaskFactory;

protected:
    size_t id;

public:
    ATQPTask(size_t task_id = 0) : id(task_id) {}
    virtual ~ATQPTask() {}

    size_t getID() { return id; }
};

//---------------------------------------------------------------------------------------
class ATQPTaskFactory
{
protected:
    int counter;

public:
    ATQPTaskFactory() : counter(0) {}
    virtual ~ATQPTaskFactory() {}

    virtual ATQPTask *create() = 0;
    void initialize(ATQPTask *task) { task->id = counter++; } // ToDo: can be some uuid generation
};

//---------------------------------------------------------------------------------------
class ATQPSupervisor
{
protected:
    ATQPTask **tasks;
    int n_task;
    int current_task;
    ATQPTaskFactory *factory;

public:
    ATQPSupervisor(int _n_task, ATQPTaskFactory *_factory) 
        : n_task(_n_task)
        , tasks(nullptr)
        , factory(_factory)
    { 
        if (n_task > 0)
        {
            tasks = new ATQPTask *[n_task];
            for (int i = 0; i < n_task; i++)
                tasks[i] = factory->create();
        }

        reset();
    }

    virtual ~ATQPSupervisor()
    {
        for (int i = 0; i < n_task; i++)
            delete tasks[i];
        delete[] tasks;
    }

    bool reset()
    {
        current_task = 0;
        return true;
    }

    virtual bool getTask(ATQPTask*& task)
    {
        task = nullptr;
        if (current_task >= n_task)
            return false;
        task = tasks[current_task];
        current_task++;
        return true;
    }
};

//---------------------------------------------------------------------------------------
class ATQPProcessor
{
protected:
    ATQPTask *task;
    int id;
    bool init;

public:
    ATQPProcessor(int _id) : id(_id), init(false) {}
    virtual ~ATQPProcessor() {}

    bool initialized() { return init; }
    bool reset() { init = false; return init; }
    int getID() { return id; }
    virtual bool setTask(ATQPTask *_task)
    {
        if (!_task)
            return true;
        task = _task;
        init = true;
        return false;
    }

    virtual ATQPTask* getTask() { return task; }
    virtual bool proceed() = 0;
};

//---------------------------------------------------------------------------------------
class TaskQueueProcessor
{
public:
    TaskQueueProcessor() {}
    virtual ~TaskQueueProcessor() {}

    unsigned long proceed(std::vector<ATQPProcessor *>&, ATQPSupervisor *);

    static int getProcInfo(int);

#ifdef USE_UNIT_TEST
    static void unitTest(int num_proc = 5, int num_tasks = 10);
    static void unitTest2(int num_proc = 2, int num_tasks = 3, int num_loops = 5);
#endif
};
