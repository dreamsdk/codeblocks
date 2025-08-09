/*
 * This file is part of the FortranProject plugin for Code::Blocks IDE
 * and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 */

#ifndef PARSERTHREADF_H
#define PARSERTHREADF_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/arrstr.h>
#endif
#include <set>

#include "tokenf.h"
#include "tokenizerpp.h"
#include "usetokenf.h"
#include "moduletokenf.h"
#include "submoduletokenf.h"
#include "includedb.h"
#include "docblock.h"


class ParserThreadF
{
    public:
        ParserThreadF(const wxString& projectFilename,
                             const wxString& bufferOrFilename,
                             TokensArrayF* tokens,
                             FortranSourceForm fsForm,
                             bool isBuffer=false,
                             IncludeDB* includeDB=NULL,
                             bool interpretCPP=true,
                             std::map<wxString,wxString>* aIncludeFiles=NULL,
                             const std::vector<wxString>* projPPDefineTokens=NULL);
        ParserThreadF(const wxString& projectFilename,
                             const wxString& filename,
                             TokensArrayF* tokens,
                             FortranSourceForm fsForm,
                             IncludeDB* includeDB,
                             bool interpretCPP,
                             std::map<wxString,wxString>* aIncludeFiles,
                             const std::vector<wxString>* projPPDefineTokens,
                             const wxString& buffer);
        virtual ~ParserThreadF();
        bool Parse();
        void ParseDeclarations(bool breakAtEnd=false, bool breakAtContains=false);
        static void SplitAssociateConstruct(const wxString& argLine, std::map<wxString,wxString>& assocMap);
        wxString GetAdditionalIncludeFile(wxString filename);
        std::vector<wxString> GetParsedFileNames();
        TokenizerPP::SkippedLinesStruct* GetSkippedLines(const wxString& fileName);
        bool HasProjPPDefineTokens(const wxString& token);
    protected:
    private:
        TokenF* DoAddToken(TokenKindF kind, const wxString& name, const wxString& args=wxEmptyString, const wxString& typeDefinition=wxEmptyString);
        TokenF* DoAddToken(TokenKindF kind, const wxString& name, const wxString& args, const unsigned int defStartLine);
        FileTokenF* DoAddFileToken(const wxString& filename, const wxString& projectFilename);
        void DoAddDefineToken(const wxString& defTokName);
        UseTokenF* DoAddUseToken(const wxString& modName);
        ModuleTokenF* DoAddModuleToken(const wxString& modName);
        SubmoduleTokenF* DoAddSubmoduleToken(const wxString& submName, const wxString& ancestorModule, const wxString& parentSubmodule, unsigned int defStartLine);

        TokenizerPP m_Tokens;
        TokensArrayF* m_pTokens;
        TokenF* m_pLastParent;
        wxString m_Filename;
        wxArrayString m_IncludeList;
        IncludeDB* m_pIncludeDB;
        bool m_interpretCPP;
        std::map<wxString,wxString>* m_pAIncludeFiles;  // additional include files

        int m_NumberOfBlockConstruct;

        wxString m_LastTokenName;
        DocBlock m_ParentDocs;

        unsigned int m_InterfaceOperator;
        unsigned int m_InterfaceAssignment;
        unsigned int m_InterfaceRead;
        unsigned int m_InterfaceWrite;

        const wxString m_Briefend;

        TokensArrayF* m_pPPDefineTokens;
        int m_inIfdef;
        bool m_addPPDefineTokens;

        void InitSecondEndPart();
        void HandleModule();
        void HandleSubmodule();
        void HandleFunction(TokenKindF, TokenAccessKind taKind=taPublic);
        void HandleType(bool& needDefault, TokenF* &newToken);
        void HandleType();
        void HandleUse();
        void HandleBlockConstruct();
        void HandleAssociateConstruct();
        void HandleSelectTypeConstruct();
        void HandleSelectCaseConstruct();
        void HandleInterface(TokenAccessKind taKind=taPublic);
        void HandleInterface(TokenAccessKind taKind, TokenF* &tokNew, bool &isGeneric);
        void HandleBlockData();
        void HandleInclude();
        void HandlePPDirective(wxString& token);
        void HandlePPDefine();
        void HandlePPUndefine();
        void HandlePPIfdef(wxString& ifToken);
        bool HasDefine(const wxString& token, unsigned int lnum);
        void SkipPPIfdef(wxString& tokenAtEnd);
        void HandleAccessList(TokenAccessKind taKind, bool& changeDefault, int& countAccess, wxArrayString& nameList);
        void HandleProcedureList();
        void HandlePrivatePublic();
        void GoThroughBody();
        bool IsEnd(wxString tok_low, wxString nex_low);
        bool ParseDeclarationsFirstPart(wxString& token, wxString& next);
        void ParseDeclarationsSecondPart(wxString& token, bool& needDefault, TokensArrayF& newTokenArr);
        void HandleSubmoduleProcedure();
        void CheckParseOneDeclaration(wxString& token, wxString& tok_low, wxString& next, wxString& next_low,
                                bool& needDefault, TokensArrayF& newTokenArr, bool& hasFunctionInLine);
        void ParseTypeBoundProcedures(const wxString& firstWord, bool breakAtEOL, bool passIn=true);
        void MakeArrayStringLower(wxArrayString &arr, wxArrayString &arrLw);
        void SetTokenAccess(ModuleTokenF* modToken, TokenF* token, TokenAccessKind defAKind);
        void GetDocBlock(DocBlock &docs, bool lookDown, unsigned int ln, bool takeSimpleDoc);
        wxString TrimRepetitives(wxString& inStr);
        wxString GetDocLine(unsigned int ln);
        void AddParamDocs(TokenF* pParToken, DocBlock &docs);
        void HandleBindTo();
        void CheckParseCallProcedure(wxString& token, wxString& tok_low, wxString& next);
        void TakeFunctionsCallsFromString(const wxString& strIn);
        void GetWordBefore(const wxString& str, int idxEnd, wxString& funName, int& idxStart);

        std::set<wxString> m_KnownEndSecPart;
        std::vector<wxString> m_ProjPPDefineTokens; // In project properties defined CPP directives.
};

#endif // PARSERTHREADF_H
