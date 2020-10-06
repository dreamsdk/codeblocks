#ifndef AUTOINSERT_H
#define AUTOINSERT_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/string.h>
    #include <wx/regex.h>

    #include <cbeditor.h>
    #include <cbstyledtextctrl.h>
#endif
#include <map>


class AutoInsert
{
    public:
        AutoInsert();
        ~AutoInsert();
        void EditRule(const wxString& statement, int aiType, bool doAddName, bool alignToStatement);
        const std::map<wxString,wxString>* GetNameMap();
        bool GetItemValues(const wxString& statementName, int& aiType, bool& doAddName, bool& alignToStatement);
        bool GetItemChoices(const wxString& statementName, wxArrayString& aiTypeStrArr,
                            wxArrayString& alignStrArr, bool& addNameEnabled);
        void MakeAutoInsert(cbEditor* editor);
        void ReadAIOptions();
        void WriteAIOptions();

    private:
        enum AutoInsertType
        {
            aitSeparate = 1,
            aitTogether,
            aitTogetherCap,
            aitNone
        };
        std::map<wxString,wxString> m_NameMap;
        std::map<wxString,AutoInsertType> m_AITMap;
        std::map<wxString,bool> m_DoAddNameMap;
        std::map<wxString,bool> m_AlignTSMap; // do align to statement?
        std::map<wxString,wxRegEx*> m_RegMap;

        AutoInsertType GetAIT(int aiT);
        int GetAITInt(AutoInsertType aiT);
        wxString FindKey(const wxString& statementName);
        wxString GetWord(const wxString& line, size_t pos);
        int FindEndBracket(const wxString str, size_t istart) const;
        bool DoEndStatementIsRequired(cbStyledTextCtrl* stc, const wxString& key);
        bool GetIndentAndPos(cbStyledTextCtrl* stc, const wxString& lineStr, wxString& firstName, int& firstNameIndent, int& keyStartPos, int& keyIndent);
        bool IsAtLineEnd(cbStyledTextCtrl* stc);
        void GetLine(cbStyledTextCtrl* stc, wxString& lineStr, int line=-1);
        void GetFortranLine(cbStyledTextCtrl* stc, wxString& lineStr, int line=-1);

        wxRegEx m_ReDo;
        wxRegEx m_ReDoEnd;

        bool m_RulesWereChanged;
};

#endif // AUTOINSERT_H
