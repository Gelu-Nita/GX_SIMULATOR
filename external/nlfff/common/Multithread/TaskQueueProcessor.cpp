#include "TaskQueueProcessor.h"

#include <iostream>
#include <queue>
#include <condition_variable>
#include <thread>
#include <mutex>
#include "console_debug.h"

std::size_t             global_process_counter;

std::mutex              global_mutex_query;
std::condition_variable global_check_query;
std::queue<int>         global_queue_query;

std::vector<std::mutex *>              global_mutexes_task;
std::vector<std::condition_variable *> global_checks_task;
std::vector<std::queue<ATQPTask *> *>  global_queues_task;

//-----------------------------------------------------------------------------
void processorFunc(ATQPProcessor *processor)
{
    int id = processor->getID();
    console_debug("[processor " << id << "]\trunning...")

    bool finish = false;
    while (!finish)
    {
        if (processor->initialized())
        {
            console_debug("[processor " << id << "]\tproceeds task by address " << processor->getTask());
            processor->proceed();
        }

        { // supervisor request
            std::unique_lock<std::mutex> locker(global_mutex_query);
            global_queue_query.push(id);
            global_check_query.notify_one();
        }

        { // supervisor respond
            std::unique_lock<std::mutex> locker(*(global_mutexes_task[id]));
            global_checks_task[id]->wait(locker, [&]() {return !global_queues_task[id]->empty();});
            if (!global_queues_task[id]->empty())
            {
                finish = processor->setTask(global_queues_task[id]->front());
                global_queues_task[id]->pop();
            }
        }
    }

    {
        std::unique_lock<std::mutex> locker(global_mutex_query);
        global_process_counter--;
        console_debug("[processor " << id << "]\tremain(s) " << global_process_counter << " active processor(s)");
        processor->reset();
        global_check_query.notify_one();
    }
}

//-----------------------------------------------------------------------------
void supervisorFunc(ATQPSupervisor* supervisor)
{
    console_debug("[supervisor]\trunning...");

    bool done = false;
    while (!done)
    {
        std::unique_lock<std::mutex> locker(global_mutex_query);
        global_check_query.wait(locker, [&]() { return !global_queue_query.empty() || global_process_counter == 0; });
        if (global_process_counter == 0)
        {
            done = true;
            console_debug("supervisor done");
        }
        while (!global_queue_query.empty() && !done)
        {
            int id = global_queue_query.front();
            ATQPTask* task;
            {
                std::unique_lock<std::mutex> locker_m(*(global_mutexes_task[id]));
                supervisor->getTask(task);
                global_queues_task[id]->push(task);
                global_checks_task[id]->notify_one();
            }
            global_queue_query.pop();
            console_debug("[supervisor]\tquery task from processor " << id << ", task address " << task)
        }
    }

    console_debug("end of supervisor");
}

//-----------------------------------------------------------------------------
unsigned long TaskQueueProcessor::proceed(std::vector<ATQPProcessor *>& processors, ATQPSupervisor *supervisor)
{

    ATQPTask task;
    std::size_t num_proc = processors.size();
    global_process_counter = num_proc;

    for (std::size_t i = 0; i < num_proc; i++)
    {
        global_mutexes_task.push_back(new std::mutex);
        global_checks_task.push_back(new std::condition_variable);
        global_queues_task.push_back(new std::queue<ATQPTask *>);
    }

    std::thread supervisorThread(supervisorFunc, supervisor);
    std::vector<std::thread> threads;
    for (std::size_t i = 0; i < num_proc; i++)
        threads.push_back(std::thread(processorFunc, processors[i]));
    for (auto &t : threads)
        t.join();
    supervisorThread.join();

    console_debug("terminated " << num_proc << " processors")

    for (int i = 0; i < num_proc; i++)
    {
        delete global_mutexes_task[i];
        delete global_checks_task[i];
        delete global_queues_task[i];
    }

    console_debug("objects deleted")

    global_queue_query.empty();

    global_mutexes_task.clear();
    global_checks_task.clear();
    global_queues_task.clear();

    console_debug("objects clear")

    return 0;
}

//-----------------------------------------------------------------------------
int TaskQueueProcessor::getProcInfo(int nThreadsInitial)
{
    int n = (int)std::thread::hardware_concurrency();

    int outn = 1;
    if (nThreadsInitial > 100000) // debug!
        outn = nThreadsInitial - 100000;
    else if (nThreadsInitial < 0)
        outn = n - 1;
    else if (nThreadsInitial == 0)
        outn = n;
    else
    {
        if (nThreadsInitial > n)
            outn = n;
        else
            outn = nThreadsInitial;
    }

    if (outn <= 0)
        outn = 1;

    return outn;
}

#ifdef USE_UNIT_TEST

    // Call sample (Windows):
    //
    //#include "TaskQueueProcessor.h"
    //
    //AllocConsole();
    //FILE* fDummy;
    //freopen_s(&fDummy, "CONOUT$", "w", stdout);
    //TaskQueueProcessor::unitTest();

    #include <random>
    #include <chrono>

    //---------------------------------------------------------------------------------------
    class QPTask : public ATQPTask
    {
    protected:
        int data;

    public:
        void setData(int _data) { data = _data; }
        int getData() { return data; }
    };
    
    //---------------------------------------------------------------------------------------
    class QPTaskFactory : public ATQPTaskFactory
    {
    public:
        QPTaskFactory() : ATQPTaskFactory() {}
        virtual ~QPTaskFactory() {}

        virtual ATQPTask *create()
        {
            QPTask *task = new QPTask;
            initialize(task);

            return task;
        }
    };

    //---------------------------------------------------------------------------------------
    class QPSupervisor : public ATQPSupervisor
    {
    public:
        QPSupervisor(int _n_task, ATQPTaskFactory* _factory) : ATQPSupervisor(_n_task, _factory)
        {
            for (int i = 0; i < _n_task; i++)
                ((QPTask *)tasks[i])->setData(2*i);
        }

        virtual ~QPSupervisor() 
        { 
        }
    };

    //---------------------------------------------------------------------------------------
    class QPProcessor : public ATQPProcessor
    {
    protected:
        std::mt19937 *p_generator;
        QPTask *this_task;

    public:
        QPProcessor(int _id) : ATQPProcessor(_id)
        {
            p_generator = new std::mt19937((unsigned int)std::chrono::system_clock::now().time_since_epoch().count());
        }
        virtual ~QPProcessor() { delete p_generator; }

        virtual bool setTask(ATQPTask * _task)
        {
            bool finish = ATQPProcessor::setTask(_task);
            if (!finish)
            {   // cast void *_task to the real object:
                this_task = (QPTask *)_task;
                console_debug("  [child processor " << id << "]\tget task " << this_task->getID() << " (task address " << _task << "), task data " << this_task->getData());
            }
            return finish;
        }

        virtual bool proceed()
        {
            int delay = this_task->getData() /2 + (*p_generator)() % 5;
            console_debug("  [child processor " << id << "]\ttask " << this_task->getID() << " will be proceeded in " << delay << " seconds");
            std::this_thread::sleep_for(std::chrono::seconds(delay));
            return true;
        }
    };

    //---------------------------------------------------------------------------------------
    void TaskQueueProcessor::unitTest2(int num_proc, int num_tasks, int num_loops)
    {
        TaskQueueProcessor proc;

        std::vector<ATQPProcessor *> processors;
        for (int i = 0; i < num_proc; i++)
            processors.push_back(new QPProcessor(i));

        QPTaskFactory factory;
        QPSupervisor supervisor(num_tasks, &factory);
        for (int k = 0; k < num_loops; k++)
        {
            supervisor.reset();
            std::cout << "started " << k << std::endl;
            proc.proceed(processors, &supervisor);
            std::cout << "finished " << k << std::endl;
        }

        for (int i = 0; i < num_proc; i++)
            delete processors[i];
    }

    //---------------------------------------------------------------------------------------
    void TaskQueueProcessor::unitTest(int num_proc, int num_tasks)
    {
        TaskQueueProcessor proc;

        std::vector<ATQPProcessor *> processors;
        for (int i = 0; i < num_proc; i++)
            processors.push_back(new QPProcessor(i));

        QPTaskFactory factory;
        QPSupervisor supervisor(num_tasks, &factory);

        std::cout << "started" << std::endl;
        proc.proceed(processors, &supervisor);
        std::cout << "finished" << std::endl;

        for (int i = 0; i < num_proc; i++)
            delete processors[i];
    }

    //Sample console output:
    //
    //started
    //[supervisor]    running...
    //[processor 0]   running...
    //[processor 1]   running...
    //[processor 2]   running...
    //[processor 3]   running...
    //[processor 4]   running...
    //[supervisor]    query task from processor 0, task address 0000020320437280
    //  [child processor 0]   get task 0 (task address 0000020320437280), task data 0
    //[processor 0]   proceeds task by address 0000020320437280
    //  [child processor 1]   get task 1 (task address 0000020320437B80), task data 2
    //[processor 1]   proceeds task by address 0000020320437B80
    //  [child processor 0]   task 0 will be proceeded in 4 seconds
    //[supervisor]    query task from processor 1, task address 0000020320437B80
    //[supervisor]    query task from processor 3, task address 0000020320437BE0
    //  [child processor 1]   task 1 will be proceeded in 4 seconds
    //  [child processor 3]   get task 2 (task address 0000020320437BE0), task data 4
    //[processor 3]   proceeds task by address 0000020320437BE0
    //  [child processor 4]   get task 3 (task address 0000020320437400), task data 6
    //[processor 4]   proceeds task by address 0000020320437400
    //  [child processor 3]   task 2 will be proceeded in 5 seconds
    //[supervisor]    query task from processor 4, task address 0000020320437400
    //  [child processor 4]   task 3 will be proceeded in 4 seconds
    //[supervisor]    query task from processor 2, task address 00000203204378E0
    //  [child processor 2]   get task 4 (task address 00000203204378E0), task data 8
    //[processor 2]   proceeds task by address 00000203204378E0
    //  [child processor 2]   task 4 will be proceeded in 6 seconds
    //[supervisor]    query task from processor 0, task address 0000020320437880
    //  [child processor 0]   get task 5 (task address 0000020320437880), task data 10
    //[processor 0]   proceeds task by address 0000020320437880
    //  [child processor 0]   task 5 will be proceeded in 8 seconds
    //[supervisor]    query task from processor 1, task address 0000020320436F20
    //  [child processor 1]   get task 6 (task address 0000020320436F20), task data 12
    //[processor 1]   proceeds task by address 0000020320436F20
    //  [child processor 1]   task 6 will be proceeded in 6 seconds
    //[supervisor]    query task from processor 4, task address 0000020320436D40
    //  [child processor 4]   get task 7 (task address 0000020320436D40), task data 14
    //[processor 4]   proceeds task by address 0000020320436D40
    //  [child processor 4]   task 7 will be proceeded in 7 seconds
    //[supervisor]    query task from processor 3, task address 00000203204374C0
    //  [child processor 3]   get task 8 (task address 00000203204374C0), task data 16
    //[processor 3]   proceeds task by address 00000203204374C0
    //  [child processor 3]   task 8 will be proceeded in 8 seconds
    //[supervisor]    query task from processor 2, task address 00000203204376A0
    //  [child processor 2]   get task 9 (task address 00000203204376A0), task data 18
    //[processor 2]   proceeds task by address 00000203204376A0
    //  [child processor 2]   task 9 will be proceeded in 13 seconds
    //[supervisor]    query task from processor 1, task address 0000000000000000
    //[processor 1]   remain(s) 4 active processor(s)
    //[supervisor]    query task from processor 4, task address 0000000000000000
    //[processor 4]   remain(s) 3 active processor(s)
    //[supervisor]    query task from processor 0, task address 0000000000000000
    //[processor 0]   remain(s) 2 active processor(s)
    //[supervisor]    query task from processor 3, task address 0000000000000000
    //[processor 3]   remain(s) 1 active processor(s)
    //[supervisor]    query task from processor 2, task address 0000000000000000
    //[processor 2]   remain(s) 0 active processor(s)
    //terminated
    //finished

#endif // USE_UNIT_TEST
