
#ifndef CONSTR_HIGHLIGHTER_H_INCLUDED
#define CONSTR_HIGHLIGHTER_H_INCLUDED

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/wxscintilla.h>
    #include <wx/string.h>
    #include <wx/stopwatch.h>
    #include <wx/colour.h>

    #include <cbeditor.h>
#endif
#include <set>
#include <list>
#include <map>

#include "fconstruct.h"
#include "tokenizerf.h"

/** \brief Highlighting of Fortran constructs in the editor.
 */
class ConstrHighlighter
{
    private:
        struct Keyword
        {
            wxString word;
            int posStart;
        };

        typedef std::list<Keyword> KeywordList;

        enum SearchDirection
        {
            sdirBack,
            sdirForth,
            sdirBackForth,
            sdirUnknown
        };

    public:
        ConstrHighlighter();
        ~ConstrHighlighter();
        void ClearHighlighting(cbStyledTextCtrl* control, bool forceAction=false);
        void DoWork(cbEditor* editor, FortranSourceForm fsForm);
        void ReadOptions();
        int GetConstructStartPos(cbEditor* editor, FortranSourceForm fsForm);

    private:
        bool m_MakeHighlight;
        wxColour m_FullColour;
        wxColour m_UnfinColour;
        int m_CurrentPosition;
        FortranSourceForm m_CurrentSForm;
        int m_IndicHighlight;
        bool m_WasCleared;
        std::set<wxString> m_KeywordSet;
        std::map<wxString, FConstruct::FConstructType> m_FConstructTypeMap;
        wxStopWatch m_Watch;
        long m_TimeMax;

        void MakeFConstructTypeMap();
        void FindMyPairs(cbStyledTextCtrl* control, Keyword &word1, Keyword &word2, Keyword &word3, FConstruct::FCLid flid, KeywordList &myPairs, bool &foundFull);
        void SearchUnCon(cbStyledTextCtrl* control, int pStartC, int pEndC, const FConstruct& fcon, const FConstruct& unFconBack,
                             bool& foundBack, KeywordList& myPairs);
        void SearchUnConGroup(cbStyledTextCtrl* control, int pStartC, int pEndC, FConstruct::FConstructType fct,
                              const FConstruct& unFcon, FConstruct::FCLid fcl, bool& foundEnd, KeywordList& myPairs);
        void SearchUnConIntermediate(cbStyledTextCtrl* control, int pStartC, int pEndC, const FConstruct& unFcon,
                                              std::vector<int>& cutEndVec, std::vector<int>& cutStartVec, KeywordList& myPairs);
        int GetWordsEnd(wxString& str1, int str1Pos, wxString& str2, int str2Pos, wxString& str3, int str3Pos);
        void PutToKeywordList(wxString& str1, wxString& str2, wxString& str3,
                              int str1Pos, int str2Pos, int str3Pos, KeywordList& kList);
        int FindFKeywordFull(cbStyledTextCtrl* control, int pStart, int pEnd, FConstruct::FCLid flid, const wxString& sWord1, const wxString& sWord2, const wxString& sWord3,
                                      wxString& str1, int& str1Pos, wxString& str2, int& str2Pos, wxString& str3, int& str3Pos);
        int FindProgGroupEndKeywordPos(cbStyledTextCtrl* control, int pStart, int pEnd, int& lineStartPos, int& lineEndPos,
                                                  FConstruct::FCLid& flid, bool& oneWord);
        int FindGroupKeyword(cbStyledTextCtrl* control, int pStart, int pEnd, FConstruct::FCLid flid,
                                      wxString& rstr1, int& rstr1Pos, wxString& rstr2, int& rstr2Pos, wxString& rstr3, int& rstr3Pos);
        int FindFKeyword(cbStyledTextCtrl* control, int pStart, int pEnd, FConstruct::FCLid flid, const wxString& sWord1, const wxString& sWord2, const wxString& sWord3,
                                  wxString& str1, int& str1Pos, wxString& str2, int& str2Pos, wxString& str3, int& str3Pos);
        void GetKeyworsFromLine(cbStyledTextCtrl* control, FConstruct::FCLid flid, int lineStartPos, int lineEndPos,
                                           wxString& str1, wxString& str2, wxString& str3, int& str1Pos, int& str2Pos, int& str3Pos);
        int FindText(cbStyledTextCtrl* control, int pStart, int pEnd, const wxString& sWord, int flag);
        bool IsCommentOrString(cbStyledTextCtrl* control, int pos);
        void GetFortranConstruct(const Keyword &word1, const Keyword &word2, const Keyword &word3, FConstruct &fcon);
        void GetSearchDirection(const Keyword &word1, const Keyword &word2, const Keyword &word3,
                                         SearchDirection& sdir, FConstruct& unFconBack, FConstruct& unFconForth);
        void GetFortranLine(cbStyledTextCtrl* control, int pos, wxString& fLine, int& lineStartPos, int& lineEndPos);
        wxString GetFortranLine2(cbStyledTextCtrl* control, int posC, int& posStart, int& posEnd, bool& tryBack, bool& withContinuation, bool goForth);
        wxString CutBracketsLevel2(const wxString& str);
        int FindLimitPos(cbStyledTextCtrl* control, const FConstruct& fcon, int posStart, SearchDirection sdir);

};

#endif // CONSTR_HIGHLIGHTER_H_INCLUDED
