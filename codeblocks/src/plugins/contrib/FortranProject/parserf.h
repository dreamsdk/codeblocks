/*
 * This file is part of the FortranProject plugin for Code::Blocks IDE
 * and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 */

#ifndef PARSERF_H
#define PARSERF_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/string.h>
    #include <wx/event.h>
    #include <wx/file.h>

    #include <cbeditor.h>
#endif
#include <set>
#include <vector>

#include "tokenf.h"
#include "tokenizerf.h"
#include "includedb.h"
#include "farrays.h"
#include "fortranfileext.h"
#include "calledbydict.h"

typedef std::vector<FortranSourceForm> ArrayOfFortranSourceForm;

extern FortranFileExt g_FortranFileExt;

class ParserF
{
    public:
        ParserF(bool withIntrinsicModules=true);
        ~ParserF();
        bool Parse(const wxString& projectFilename, const wxString& filename, FortranSourceForm fsForm, const std::vector<wxString>* pCppMacros);
        bool Reparse(const wxString& projectFilename, const wxString& filename, FortranSourceForm fsForm, const std::vector<wxString>* pCppMacros);
        bool BatchParse(const wxArrayString& projectFilenames, const wxArrayString& filenames, ArrayOfFortranSourceForm& fileForms,
                        const std::vector<wxString>* pCppMacros);
        bool RemoveFile(const wxString& filename);
        void RemoveBuffer(const wxString& filename);
        TokensArrayF* GetTokens(){return m_pTokens;};
        void SetAdditionalIncludeFiles(std::map<wxString,wxString>* pAIncludeFiles);
        bool FindTypeBoundProcedures(const TokenFlat& interToken, const wxArrayString& searchArr, TokensArrayFlat& resTokenArr);
        bool FindMatchTokenInSameModule(const TokenFlat& procedureToken, const wxString& search, TokensArrayFlat& result, int tokenKindMask, int noChildrenOf);
        size_t FindMatchTokensDeclared(const wxString& search, TokensArrayFlat& result, int tokenKindMask, bool partialMatch=false, int noChildrenOf=0,
                                       bool onlyPublicNames=false, bool noIncludeFiles=false);
        size_t FindMatchTokens(wxString filename, wxString search, TokensArrayF& result);
        void Clear();
        void ObtainUsedDeclaredModules(const wxString& fileName, StringSet* fileUseModules, StringSet* fileDeclaredModules,
                                        StringSet* fileExtendsSModules, StringSet* fileDeclaredSubmodules, StringSet* fileIncludes);
        bool IsFileFortran(const wxString& filename, FortranSourceForm& fsForm);
        void FindMatchDeclarationsInCurrentScope(const wxString& search, cbEditor* ed, TokensArrayFlat& result, bool partialMatch, int endPos=-1, int* nLineStart=NULL);
        void FindMatchVariablesInModules(const wxString& search, TokensArrayFlat& result, bool partialMatch);
        bool FindMatchTypeComponents(TokenFlat& parentTok, const wxString& lineStr, TokensArrayFlat& result);
        bool FindMatchTypeComponents(cbEditor* ed, const wxString& line, TokensArrayFlat& result, bool partialMatch, bool onlyPublicNames,
                                     bool& isAfterPercent, bool getAsProcedure);
        bool FindMatchTypeComponents2(TokensArrayFlat* foundVariables, unsigned int myScopeLine, wxString& myFilename, wxArrayString& parts,
                                        TokensArrayFlat& result, bool partialMatch, bool onlyPublicNames, bool getAsProcedure);
        bool CutLineIntoParts(const wxString& lineCur, bool& isAfterPercent, wxArrayString& parts);
        void FindMatchTokensForToolTip(const wxString& nameUnder, int posEndOfWord, cbEditor* ed, bool onlyUseAssoc, bool onlyPublicNames, TokensArrayFlat& result, bool& isAfterPercent);
        void FindGenericTypeBoudComponents(TokenFlat* token, TokensArrayFlat& result);
        void FindMatchOperatorTokensForJump(wxString& nameOperator, TokensArrayFlat& result);
        void FindMatchTokensForJump(cbEditor* ed, bool onlyUseAssoc, bool onlyPublicNames, TokensArrayFlat& result);
        bool FindMatchTokensForCodeCompletion(bool useSmartCC, bool onlyUseAssoc, bool onlyPublicNames, const wxString& nameUnderCursor,
                                              cbEditor* ed, TokensArrayFlat& result, bool& isAfterPercent, int& tokKind, wxArrayString& firstWords);
        bool FindWordsBefore(cbEditor* ed, int numberOfWordsMax, wxString &curLine, wxArrayString &firstWords);
        void RereadOptions();
        bool FindTokenDeclaration(TokenFlat& token, const wxString& argName, wxString& argDecl, wxString& argDescription);
        bool FindTokenRange(TokenFlat& token, wxString& txtRange, wxString& buff, std::vector<int> &lineStarts, bool withDefinition=false, bool readFile=true);
        bool FindTokenRange(TokenFlat& token, wxString& txtRange);
        bool FindInfoLog(TokenFlat& token, bool logComAbove, bool logComBelow, bool logDeclar, bool logComVariab, wxString& msg);
        bool FindInfoLog(TokenFlat& token, bool logComAbove, bool logComBelow, bool logDeclar, bool logComVariab, wxString& msg, bool readFile);
        bool FindInfoLog(TokenFlat& token, bool logComAbove, bool logComBelow, bool logDeclar, bool logComVariab, wxString& msg, wxString& argsNew);
        bool FindInfoLog(TokenFlat& token, bool logComAbove, bool logComBelow, bool logDeclar, bool logComVariab, wxString& msg, const wxString& argsNew, bool readFile);
        bool FindTooltipForTypeBoundProc(wxString& msg, TokenFlat* token1, TokenFlat* token2);
        bool FindInfoLogForTypeBoundProc(TokensArrayFlat& tokenPair, bool logComAbove, bool logComBelow, bool logDeclar, bool logComVariab, wxString& msg,
                                         wxString* buff=NULL, std::vector<int>* lineStarts=NULL);
        bool FindInfoLogForGenericTBProc(TokensArrayFlat& tokens, bool logComAbove, bool logComBelow, bool logDeclar, bool logComVariab, wxString& msg);
        void FindMachDefineTokens(const wxString& search, cbEditor* ed, TokensArrayFlat& result);
        bool GetTokenStr(TokenFlat& token, wxString& msg);
        void FindChildrenOfInterface(TokenFlat* token, TokensArrayFlat& result);
        void GetPossitionOfDummyArgument(const wxString& args, const wxString& arg, int& start, int& end);
        void GetCallTipHighlight(const wxString& calltip, int commasWas, int& start, int& end);
        void FindUseAssociatedTokens(bool onlyPublicNames, TokenFlat* tok, const wxString& search, bool partialMatch, TokensArrayFlat& result, int tokenKindMask, bool changeDisplayName, TokensArrayFlat* useWithRenameTok=NULL);
        void FindUseAssociatedTokens(bool onlyPublicNames, cbEditor* ed, const wxString& search, bool partialMatch, TokensArrayFlat& result, int tokenKindMask, bool changeDisplayName, TokensArrayFlat* useWithRenameTok=NULL);
        void FindUseAssociatedTokens(bool onlyPublicNames, wxArrayString& address, const wxString& search, bool partialMatch, TokensArrayFlat& result, int tokenKindMask, bool changeDisplayName, TokensArrayFlat* useWithRenameTok=NULL);
        void GetTypeComponentsInFile(const wxString& fileName, const unsigned int line, const wxString& nameType, TokensArrayFlat* result);
        bool IsIncludeFile(wxString fileName);
        bool HasIncludeFiles();
        TokenF* FindFile(const wxString& filename);
        void FindFile(const wxString& filename, TokensArrayFlat& result);
        void SetNewTokens(TokensArrayF* pTokens);
        void SetNewIncludeDB(IncludeDB* pIncludeDB);
        void SetNewADirTokens(TokensArrayF* pTokens);
        void SetNewADirIncludeDB(IncludeDB* pIncludeDB);
        void ConnectToNewTokens();
        void ConnectToNewADirTokens();
        void SetNewCurrentTokens(TokensArrayF* pTokens);
        void ConnectToNewCurrentTokens();
        void ChangeLineIfRequired(cbEditor* ed, wxString& curLine);
        TokenF* FindTokenBetweenChildren(TokenF* pToken, const wxString& name);
        //void GetAddress(TokenF* token, wxArrayString& address);
        void GetAddressOfToken(TokenF* token, wxArrayString& address);
        TokenF* FindToken(const TokenFlat &token, TokensArrayF* children=NULL);
        void ChangeArgumentsTypeBoundProc(TokenFlat& tbProcTok, const TokenFlat& procTok);
        void GetChildren(TokenFlat* token, int tokenKindMask, TokensArrayFlat& result, int levelMax=1);
        void GetChildren(TokenF* pToken, int tokenKindMask, TokensArrayFlat& result, int level, int levelMax);
        void FindImplementedProcInMySubmodules(TokenFlat* tok, const wxString& search, TokensArrayFlat& result);
        void ChangeAddressWithInclude(TokensArrayFlat& tokArr);
        void BuildCalledByDict(CalledByDict& cByDict);
        void SetInterpretCPP(bool interpretCPP);
        std::vector<int>* GetSkippedLines(const wxString& fileName);
        void SetNewSkippedLines(const wxString& fileName, std::vector<int>& skipLineStart, std::vector<int>& skipLineEnd);
        void ConnectToNewSkippedLines();

    protected:
    private:
        void FindMatchChildren(TokensArrayF &m_Children, wxString search, TokensArrayF& result, bool exact=false);
        size_t GetFileIndex(const wxString& filename);
        TokensArrayF* FindFileTokens(const wxString& filename);
        TokenF* FindFileTokenWithName(const wxString& filename);
        TokenF* FindModuleSubmoduleToken(const wxString& moduleName);
        void ObtainUDModulesToken(TokenF* token, StringSet* fileUseModules, StringSet* fileDeclaredModules,
                                   StringSet* fileExtendsSModules, StringSet* fileDeclaredSubmodules, StringSet* fileIncludes);
        void FindMatchChildrenDeclared(TokensArrayF &m_Children, const wxString search, TokensArrayFlat& result, int tokenKindMask, bool partialMatch=false, int noChildrenOf=0, bool onlyPublicNames=false);
        bool FindLineScope(unsigned int line, int& lineStart, int tokenKindMask, TokensArrayF& children, TokenF* &pToken);
        void FindLineScopeLN(cbEditor* ed, int& lineStart, TokenFlat* &token, int endPos);
        bool CutBlocks(const wxChar& ch, wxString& line);
        bool GetTypeOfComponent(const wxString& nameType, const wxString& nameComponent, wxString& nameTypeComponent);
        bool GetTypeOfComponent(TokenF** pT, const wxString& nameComponent, wxString& nameTypeComponent);
        bool GetTypeOfChild(TokenF* pT, const wxString& nameComponent, wxString& nameTypeComponent);
        TokenF* GetType(const wxString& nameType);
        TokenF* GetTypeInFile(const wxString& fileName, const unsigned int line, const wxString& nameType);
        //void FindUseAssociatedTokens(TokenF* useToken, const wxString& searchLw, TokensArrayFlat& result, int tokenKindMask, bool partialMatch, bool changeDisplayName, bool onlyPublicNames, TokensArrayFlat* useWithRenameTok=NULL);
        //void FindMatchTokensInModuleAndUse(const wxString &modName, const wxString& searchLw, TokensArrayFlat& result, int tokenKindMask, int noChildrenOf, bool partialMatch, bool onlyPublicNames, bool changeDisplayName, TokensArrayFlat* useWithRenameTok);
        void FindUseAssociatedTokens2(TokenF* useToken, const wxString &searchLw, ArrOfSizeT &resChildrenIdx, BoolArray2D &resCanBeSeen2D, int tokenKindMask, bool partialMatch, bool changeDisplayName,
                                      bool onlyPublicNames, TokensArrayFlat& renamedTokens, TokensArrayFlat* useWithRenameTok);
        void FindMatchTokensInModuleAndUse2(const wxString& modName, const wxString& searchLw, ArrOfSizeT* &childrenIdx, BoolArray2D* &canBeSeen2D, int tokenKindMask,
                                             int noChildrenOf, bool partialMatch, bool onlyPublicNames, bool changeDisplayName, TokensArrayFlat* useWithRenameTok);
        void ChangeAssociatedName(wxString& line, TokenFlat* token);
        void FindAddress(cbEditor* ed, wxArrayString& address);
        void FindAddress(TokenFlat* tokFl, wxArrayString& address);
        void FindTokensForUse(const wxString& search, wxArrayString& firstWords, TokensArrayFlat& result, bool onlyPublicNames);
        void AddUniqueResult(TokensArrayFlat& result, const TokenF* token, bool isHostAssociated);
        void AddUniqueResult(TokensArrayFlat& result, const TokenFlat* token);
        void AddIncludeFileChildren(const TokenF* include, TokensArrayF& tokens);
        void GetSubmoduleHostTokens(TokenF* subModToken, std::vector<TokensArrayF*> &vpChildren);
        void ClearTokens(TokensArrayF* pTokens);
        void ParseIntrinsicModules();
        void ChangeCaseChildren(TokensArrayF &children, int dispCase);
        void FindImplementedProcInMySubmodules(cbEditor* ed, const wxString& search, TokensArrayFlat& result);
        void FindImplementedProcInMySubmodules(wxArrayString& address, const wxString& search, TokensArrayFlat& result);
        void FindSubmodulesWhichExtends(const wxString& moduleName, TokensArrayF* result);
        void FindMatchTokensAtInclude(cbEditor* ed, const wxString& findName, bool onlyPublicNames, bool partialMach, TokensArrayFlat& result);
        void GetChildrenAssociateConstruct(TokenF* token, int tokenKindMask, TokensArrayFlat& result);
        void SetSkippedLines(const wxString& fileName, std::vector<int>& skipLineStart, std::vector<int>& skipLineEnd);
        void FillSkippedLines(std::map<wxString,std::vector<int>*>& fileLineMap, const wxString& fileName,
                                 std::vector<int>& skipLineStart, std::vector<int>& skipLineEnd);

        TokensArrayF* m_pTokens;
        TokensArrayF* m_pIntrinsicModuleTokens;
        IncludeDB* m_pIncludeDB;
        TokensArrayF* m_pAdditionalDirTokens;
        IncludeDB* m_pIncludeDBADir;
        bool m_InterpretCPP;
        bool m_Done;

        wxString m_Buff;
        std::vector<int> m_LineStarts;

        int m_RecursiveDeep;
        //size_t maxResultCount;
        //bool reachedResultCountLimit;

        wxArrayString m_VisitedModules;
        PassedTokensArray2D m_PassedTokensVisited;
        ArrOfSizeT2D m_ChildrenIdxVisited;
        BoolArray3D m_CanBeSeenVisited;

        bool m_UseRenameArrays;
        int m_RenameDeep;

        int m_IncludeDeep;
        int m_SubmodDeep;

        wxArrayString       m_VisitedModulesRen;
        PassedTokensArray2D m_PassedTokensVisitedRen;
        ArrOfSizeT2D        m_ChildrenIdxVisitedRen;
        BoolArray3D         m_CanBeSeenVisitedRen;

        TokensArrayF* m_pTokensNew;
        IncludeDB*    m_pIncludeDBNew;
        TokensArrayF* m_pTokensADirNew;
        IncludeDB*    m_pIncludeDBADirNew;
        TokensArrayF* m_pBufferTokens;
        TokensArrayF* m_pCurrentBufferTokensNew;

        std::map<wxString,wxString>* m_pAIncludeFiles;     ///< Additional include files.
        std::map<wxString,std::vector<int>*> m_SkippedLinesMap;  ///< Skipped line indexes in the parsed files.
        std::map<wxString,std::vector<int>*> m_NewSkippedLinesMap;  ///< New skipped line indexes in the parsed files from separate thread.
};

#endif // PARSERF_H
