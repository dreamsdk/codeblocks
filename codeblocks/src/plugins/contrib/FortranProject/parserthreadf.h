/*
 * This file is part of the FortranProject plugin for Code::Blocks IDE
 * and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 */

#ifndef PARSERTHREADF_H
#define PARSERTHREADF_H

#include "tokenf.h"
#include "tokenizerf.h"
#include "usetokenf.h"
#include "moduletokenf.h"
#include "submoduletokenf.h"
#include "includedb.h"
#include "docblock.h"
#include <set>


class ParserThreadF
{
    public:
        ParserThreadF(const wxString& bufferOrFilename,
							 TokensArrayF* tokens,
							 FortranSourceForm fsForm,
							 bool isBuffer=false,
							 IncludeDB* includeDB=NULL);
        ParserThreadF(const wxString& filename,
							 TokensArrayF* tokens,
							 FortranSourceForm fsForm,
							 IncludeDB* includeDB,
							 const wxString& buffer);
        virtual ~ParserThreadF();
        bool Parse();
        void ParseDeclarations(bool breakAtEnd=false, bool breakAtContains=false);
        static void SplitAssociateConstruct(const wxString& argLine, std::map<wxString,wxString>& assocMap);
    protected:
    private:
        TokenF* DoAddToken(TokenKindF kind, const wxString& name, const wxString& args=wxEmptyString, const wxString& typeDefinition=wxEmptyString);
        TokenF* DoAddToken(TokenKindF kind, const wxString& name, const wxString& args, const unsigned int defStartLine);
        UseTokenF* DoAddUseToken(const wxString& modName);
        ModuleTokenF* DoAddModuleToken(const wxString& modName);
        SubmoduleTokenF* DoAddSubmoduleToken(const wxString& submName, const wxString& ancestorModule, const wxString& parentSubmodule, unsigned int defStartLine);

		Tokenizerf m_Tokens;
		TokensArrayF* m_pTokens;
		TokenF* m_pLastParent;
		wxString m_Filename;
		wxArrayString m_IncludeList;
		IncludeDB* m_pIncludeDB;

		int m_NumberOfBlockConstruct;

		wxString m_LastTokenName;
		DocBlock m_ParentDocs;

		unsigned int m_InterfaceOperator;
		unsigned int m_InterfaceAssignment;
		unsigned int m_InterfaceRead;
		unsigned int m_InterfaceWrite;

		const wxString m_Briefend;

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
		void ChangeTokenAccess(ModuleTokenF* modToken, TokenF* token);
		void GetDocBlock(DocBlock &docs, bool lookDown, unsigned int ln, bool takeSimpleDoc);
		wxString TrimRepetitives(wxString& inStr);
		wxString GetDocLine(unsigned int ln);
		void AddParamDocs(TokenF* pParToken, DocBlock &docs);
		void HandleBindTo();
		void CheckParseCallProcedure(wxString& token, wxString& tok_low, wxString& next);
        void TakeFunctionsCallsFromString(const wxString& strIn);
		void GetWordBefore(const wxString& str, int idxEnd, wxString& funName, int& idxStart);

        std::set<wxString> m_KnownEndSecPart;
};

#endif // PARSERTHREADF_H
