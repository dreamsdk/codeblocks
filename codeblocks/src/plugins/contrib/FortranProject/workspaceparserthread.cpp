
#include "workspaceparserthread.h"

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/stopwatch.h>

    #include <logmanager.h>
#endif

#include "parserthreadf.h"
#include "nativeparserf.h"

wxMutex s_WorkspaceParserMutex;
wxMutex s_NewTokensMutex;
wxMutex s_NewSkippedLinesMutex;

WorkspaceParserThread::WorkspaceParserThread(NativeParserF* parent, int idWSPThreadEvent) :
    m_pNativeParser(parent),
    m_idWSPThreadEvent(idWSPThreadEvent)
{
}

WorkspaceParserThread::~WorkspaceParserThread()
{
}

int WorkspaceParserThread::Execute()
{
    s_WorkspaceParserMutex.Lock();
    ParseFiles();
    s_WorkspaceParserMutex.Unlock();

    return 0;
}

void WorkspaceParserThread::ParseFiles()
{
    wxStopWatch sw;

    TokensArrayF* pTokens = new TokensArrayF();
    IncludeDB* pIncludeDB = new IncludeDB();
    std::map<wxString,wxString>* aIncludeFiles = m_pNativeParser->GetAdditionalIncludeFiles();

    wxArrayString* pWSFiles = m_pNativeParser->GetWSFiles();
    ArrayOfFortranSourceForm* pWSFileForms = m_pNativeParser->GetWSFileForms();
    wxArrayString* pWSProjFilenames = m_pNativeParser->GetWSFileProjFilenames();
    bool interpretCPP = m_pNativeParser->DoInterpretCPP();
    ParserF* pParser = m_pNativeParser->GetParser();

    for (size_t i=0; i<pWSFiles->size(); i++)
    {
        const std::vector<wxString>* aCPPMacros = m_pNativeParser->GetProjectCPPMacros(pWSProjFilenames->Item(i));
        ParserThreadF* thread = new ParserThreadF(pWSProjFilenames->Item(i), UnixFilename(pWSFiles->Item(i)), pTokens,
                                                  pWSFileForms->at(i), false, pIncludeDB, interpretCPP, aIncludeFiles, aCPPMacros);
        thread->Parse();

        // Take skipped lines in the parsed files.
        std::vector<wxString> parsedFileNames = thread->GetParsedFileNames();
        s_NewSkippedLinesMutex.Lock();
        for (const auto& fileName: parsedFileNames)
        {
            TokenizerPP::SkippedLinesStruct* skipStruct = thread->GetSkippedLines(fileName);
            if (skipStruct)
            {
                pParser->SetNewSkippedLines(fileName, skipStruct->lineStarts, skipStruct->lineEnds);
            }
        }
        s_NewSkippedLinesMutex.Unlock();

        delete thread;
    }
    s_NewTokensMutex.Lock();
    pParser->SetNewTokens(pTokens);
    pParser->SetNewIncludeDB(pIncludeDB);
    s_NewTokensMutex.Unlock();

    wxCommandEvent event( wxEVT_COMMAND_ENTER, m_idWSPThreadEvent );
    m_pNativeParser->AddPendingEvent(event);

    const wxString msg(wxString::Format(_("Parsing %zu files took %ld ms"), pWSFiles->size(), sw.Time()));
    std::cout << msg.ToStdString() << "\n";
    Manager::Get()->GetLogManager()->Log(msg);
}
