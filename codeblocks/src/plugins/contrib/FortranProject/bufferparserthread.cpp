
#include "bufferparserthread.h"

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <logmanager.h>
#endif

#include "parserthreadf.h"
#include "nativeparserf.h"

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
    wxString projFilename;
    m_pNativeParser->GetCurrentBuffer(buffer, filename, projFilename);

    ParseBuffer(buffer, filename, projFilename);

    BufferParserThread::s_BPTInstances--;
    return 0;
}

void BufferParserThread::ParseBuffer(wxString& buffer, wxString& filename, wxString& projFilename)
{
    FortranSourceForm fsForm;
    if (!m_pNativeParser->GetParser()->IsFileFortran(filename, fsForm))
        return;
    TokensArrayF* pTokens = new TokensArrayF();
    IncludeDB* pIncludeDB = new IncludeDB();
    std::map<wxString,wxString>* aIncludeFiles = m_pNativeParser->GetAdditionalIncludeFiles();
    bool interpretCPP = m_pNativeParser->DoInterpretCPP();
    std::vector<wxString>* strMacrosVec = m_pNativeParser->GetProjectCPPMacrosCopy(projFilename);

    ParserThreadF thread(projFilename, UnixFilename(filename), pTokens, fsForm, pIncludeDB, interpretCPP, aIncludeFiles,
                         strMacrosVec, buffer);
    thread.Parse();
    delete pIncludeDB;
    delete strMacrosVec;

    m_pNativeParser->GetParser()->SetNewCurrentTokens(pTokens);

    wxCommandEvent event( wxEVT_COMMAND_ENTER, m_idBPThreadEvent );
    m_pNativeParser->AddPendingEvent(event);
}

