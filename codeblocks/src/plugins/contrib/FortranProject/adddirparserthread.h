#ifndef ADDITIONALDIRPARSERTHREAD_H
#define ADDITIONALDIRPARSERTHREAD_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/thread.h>
    #include <wx/event.h>
#endif

#include <cbthreadpool.h>

extern wxMutex s_AdditionalDirParserMutex;
extern wxMutex s_AdditionalDirNewTokensMutex;

class NativeParserF;

class ADirParserThread : public cbThreadedTask
{
public:
    ADirParserThread(NativeParserF* parent, int idADPThreadEvent);
    virtual ~ADirParserThread();
    int Execute();
    void ParseFiles();

private:
    NativeParserF* m_pNativeParser;
    int m_idADPThreadEvent;
};

#endif // ADDITIONALDIRPARSERTHREAD_H

