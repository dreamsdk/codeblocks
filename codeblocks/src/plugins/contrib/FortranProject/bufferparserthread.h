
#ifndef BUFFERPARSERTHREAD_H
#define BUFFERPARSERTHREAD_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/thread.h>
    #include <wx/event.h>

    #include <cbthreadpool.h>
#endif

class NativeParserF;

class BufferParserThread : public cbThreadedTask
{
public:
    BufferParserThread(NativeParserF* parent, int idBPThreadEvent);
    virtual ~BufferParserThread();
    int Execute();
    void ParseBuffer(wxString& buffer, wxString& filename, wxString& projFilename);
    static int s_BPTInstances;

private:
    NativeParserF* m_pNativeParser;
    int m_idBPThreadEvent;
};

#endif // BUFFERPARSERTHREAD_H

