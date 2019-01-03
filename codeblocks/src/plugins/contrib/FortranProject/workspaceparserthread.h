#ifndef WORKSPACEPARSERTHREAD_H
#define WORKSPACEPARSERTHREAD_H

#include <wx/thread.h>
#include <wx/event.h>

#include <cbthreadpool.h>

extern wxMutex s_WorkspaceParserMutex;
extern wxMutex s_NewTokensMutex;

class NativeParserF;

class WorkspaceParserThread : public cbThreadedTask
{
public:
    WorkspaceParserThread(NativeParserF* parent, int idWSPThreadEvent);
    virtual ~WorkspaceParserThread();
    int Execute();
    void ParseFiles();

private:
    NativeParserF* m_pNativeParser;
    int m_idWSPThreadEvent;
};

#endif // WORKSPACEPARSERTHREAD_H

