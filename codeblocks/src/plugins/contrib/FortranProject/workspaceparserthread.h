#ifndef WORKSPACEPARSERTHREAD_H
#define WORKSPACEPARSERTHREAD_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/thread.h>
    #include <wx/event.h>
#endif

#include <cbthreadpool.h>

extern wxMutex s_WorkspaceParserMutex;
extern wxMutex s_NewTokensMutex;
extern wxMutex s_NewSkippedLinesMutex;

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

