#include "textcutter.h"

TextCutter::TextCutter(const wxString& allText, FortranSourceForm fsForm)
{
    m_Text = allText;
    m_TextLen = m_Text.length();
    m_CurSourceForm = fsForm;
    m_CurIdx = 0;
    m_CurColumn = 1;
}

TextCutter::~TextCutter()
{
};

void TextCutter::GetChunk(wxString& chunk, bool& isWord)
{
    isWord = false;
    chunk = wxEmptyString;
    if (IsEOF())
        return;

    unsigned int start = m_CurIdx;
    if (isalpha(CurrentChar()) || CurrentChar() == '_')
    {
        while (!IsEOF() &&
               (isalnum(CurrentChar()) || CurrentChar() == '_'))
            MoveToNextChar();
        chunk = m_Text.Mid(start, m_CurIdx - start);
        isWord = true;
    }
    else
    {
        SkipWhiteSpace();
        SkipUnwanted();
        if (start != m_CurIdx)
        {
            chunk = m_Text.Mid(start, m_CurIdx - start);
            return;
        }

        if (isdigit(CurrentChar()))
        {
            // numbers
            while (!IsEOF() && CharInString(CurrentChar(), "0123456789.abcdefABCDEFXxLl"))
                MoveToNextChar();

        }
        else if (CurrentChar() == '"' ||
                CurrentChar() == '\'')
        {
            // string, char, etc.
            wxChar match = CurrentChar();
            MoveToNextChar();  // skip starting ' or "
            SkipToChar(match);
            MoveToNextChar(); // skip ending ' or "
        }
        else
        {
            MoveToNextChar();
        }
        chunk = m_Text.Mid(start, m_CurIdx - start);
    }
    return;
}

bool TextCutter::SkipWhiteSpace()
{
    if (IsEOF())
        return false;
    while (!IsEOF() && isspace(CurrentChar()))
        MoveToNextChar();
    return true;
}

bool TextCutter::MoveToNextChar()
{
    if (IsEOF())
    {
        return false;
    }
	++m_CurIdx;
	++m_CurColumn;
    AdjustColumn();
    return true;
}

void TextCutter::AdjustColumn()
{
	if (CurrentChar() == '\n')
		m_CurColumn = 0;
}

wxChar TextCutter::CurrentChar()
{
    return m_Text.GetChar(m_CurIdx);
}

wxChar TextCutter::NextChar()
{
    if (m_CurIdx+1 >= m_TextLen)
        return '\0';
    return m_Text.GetChar(m_CurIdx+1);
}

void TextCutter::SkipUnwanted()
{
    if (IsEOF())
    {
        return;
    }
	while (CurrentChar() == '#' ||
           CurrentChar() == '!' ||
           ((CurrentChar() == 'c' || CurrentChar() == 'C' || CurrentChar() == '*') && m_CurColumn == 1 && m_CurSourceForm == fsfFixed))
	{
        SkipToEOL();
        SkipWhiteSpace();
        if (IsEOF())
            return;
	}
}

void TextCutter::SkipToChar(const wxChar& ch)
{
	// skip everything until we find ch
	while (1)
	{
		while (!IsEOF() && CurrentChar() != ch && CurrentChar() != '\n')
			MoveToNextChar();
        break;
	}
}

bool TextCutter::CharInString(const char ch, const char* chars)
{
	int len = strlen(chars);
	for (int i = 0; i < len; ++i)
	{
		if (ch == chars[i])
			return true;
	}
	return false;
}

void TextCutter::SkipToEOL()
{
    while (!IsEOF() && CurrentChar() != '\n')
        MoveToNextChar();
}
