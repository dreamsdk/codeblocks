
#include "bufferparserthread.h"
#include "parserthreadf.h"
#include "nativeparserf.h"

#include <logmanager.h>


int BufferParserThread::s_BPTInstances = 0;

BufferParserThread::BufferParserThread(NativeParserF* parent, int idBPThreadEvent) :
    m_pNativeParser(parent),
    m_idBPThreadEvent(idBPThreadEvent)
{
    BufferParserThread::s_BPTInstances++;
}

BufferParserThread::~BufferParserThread()
{
}

int BufferParserThread::Execute()
{
    wxString buffer;
    wxString filename;
    m_pNativeParser->GetCurrentBuffer(buffer, filename);

    ParseBuffer(buffer, filename);

    BufferParserThread::s_BPTInstances--;
    return 0;
}

void BufferParserThread::ParseBuffer(wxString& buffer, wxString& filename)
{
    FortranSourceForm fsForm;
    if (!m_pNativeParser->GetParser()->IsFileFortran(filename, fsForm))
        return;
    TokensArrayF* pTokens = new TokensArrayF();
    IncludeDB* pIncludeDB = new IncludeDB();

    ParserThreadF thread(UnixFilename(filename), pTokens, fsForm, pIncludeDB, buffer);
    thread.Parse();
    delete pIncludeDB;

    m_pNativeParser->GetParser()->SetNewCurrentTokens(pTokens);

    wxCommandEvent event( wxEVT_COMMAND_ENTER, m_idBPThreadEvent );
    m_pNativeParser->AddPendingEvent(event);
}

