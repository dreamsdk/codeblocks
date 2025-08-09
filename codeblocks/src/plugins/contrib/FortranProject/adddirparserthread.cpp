
#include "adddirparserthread.h"

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <logmanager.h>
#endif

#include "parserthreadf.h"
#include "nativeparserf.h"

wxMutex s_AdditionalDirParserMutex;
wxMutex s_AdditionalDirNewTokensMutex;

ADirParserThread::ADirParserThread(NativeParserF* parent, int idADPThreadEvent) :
    m_pNativeParser(parent),
    m_idADPThreadEvent(idADPThreadEvent)
{
}

ADirParserThread::~ADirParserThread()
{
}

int ADirParserThread::Execute()
{
    s_AdditionalDirParserMutex.Lock();
    ParseFiles();
    s_AdditionalDirParserMutex.Unlock();

    return 0;
}

void ADirParserThread::ParseFiles()
{
    TokensArrayF* pTokens = new TokensArrayF();
    IncludeDB* pIncludeDB = new IncludeDB();
    wxArrayString* pADirFiles = m_pNativeParser->GetADirFiles();
    ArrayOfFortranSourceForm* pADirFileForms = m_pNativeParser->GetADirFileForms();
    bool interpretCPP = m_pNativeParser->DoInterpretCPP();

    for (size_t i=0; i<pADirFiles->size(); i++)
    {
        ParserThreadF* thread = new ParserThreadF("#%&ThisIsAdditionalFileSearchDirectory&%#", UnixFilename(pADirFiles->Item(i)), pTokens,
                                                  pADirFileForms->at(i), false, pIncludeDB, interpretCPP);
        thread->Parse();
        delete thread;
    }
    s_AdditionalDirNewTokensMutex.Lock();
    m_pNativeParser->GetParser()->SetNewADirTokens(pTokens);
    m_pNativeParser->GetParser()->SetNewADirIncludeDB(pIncludeDB);
    s_AdditionalDirNewTokensMutex.Unlock();

    wxCommandEvent event( wxEVT_COMMAND_ENTER, m_idADPThreadEvent );
    m_pNativeParser->AddPendingEvent(event);
}
