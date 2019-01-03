#ifndef TEXTCUTTER_H
#define TEXTCUTTER_H

#include "tokenizerf.h"

class TextCutter
{
	public:
        TextCutter(const wxString& allText, FortranSourceForm fsForm);
        ~TextCutter();

        void GetChunk(wxString& chunk, bool& isWord);

    private:
        bool SkipWhiteSpace();
        bool MoveToNextChar();
        void AdjustColumn();
        wxChar CurrentChar();
        wxChar NextChar();
        bool IsEOF(){ return m_CurIdx >= m_TextLen; };
        void SkipUnwanted();
        void SkipToChar(const wxChar& ch);
        bool CharInString(const char ch, const char* chars);
        void SkipToEOL();

        wxString m_Text;
        unsigned int m_TextLen;
        FortranSourceForm m_CurSourceForm;
        unsigned int m_CurIdx;
        unsigned int m_CurColumn;
};

#endif // TEXTCUTTER_H
