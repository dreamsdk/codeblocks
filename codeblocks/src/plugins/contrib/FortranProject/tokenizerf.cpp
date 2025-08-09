/*
 * This file is part of the FortranProject plugin for Code::Blocks IDE
 * and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 */

#include "tokenizerf.h"

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/file.h>
#endif

bool ReadFileToString(wxFile& file,wxString& st)
{
    st.Empty();
    if (!file.IsOpened())
        return false;
    int len = file.Length();
    if(!len)
    {
        file.Close();
        return true;
    }
#if wxUSE_UNICODE
    char* buff = new char[len+1];
    if (!buff)
    {
        file.Close();
        return false;
    }
    file.Read((void*)buff, len);
    file.Close();
    buff[len]='\0';

    st = wxString((const char *)buff, wxConvUTF8);
    if (st.Length() == 0)
    {
        // could not read as utf-8 encoding, try iso8859-1
        st = wxString((const char *)buff, wxConvISO8859_1);
    }
    delete[] buff;

#else
    char* buff = st.GetWriteBuf(len); // GetWriteBuf already handles the extra '\0'.
    file.Read((void*)buff, len);
    file.Close();
    st.UngetWriteBuf();
#endif
    return true;
}

///---------------------------------------

Tokenizerf::Tokenizerf(const wxString& filename, FortranSourceForm sourceForm)
    : m_Filename(filename),
      m_BufferLen(0),
      m_TokenIndex(0),
      m_UndoTokenIndex(0),
      m_PeekedTokenIndex(0),
      m_LineNumber(1),
      m_LineNumberStart(1),
      m_UndoLineNumber(1),
      m_UndoLineNumberStart(1),
      m_PeekedLineNumber(1),
      m_PeekedLineNumberStart(1),
      m_Column(1),
      m_UndoColumn(1),
      m_PeekedColumn(1),
      m_WasNextLine(false),
      m_UndoWasNextLine(false),
      m_PeekedWasNextLine(false),
      m_WasPeeked(false),
      m_IsOK(false),
      m_SourceForm(sourceForm),
      m_PeekedToken(),
      m_DetailedParsing(false)
{
    if (!m_Filename.IsEmpty())
        Init(m_Filename, m_SourceForm);
    m_LineStartIdx.push_back(0);
}

Tokenizerf::~Tokenizerf()
{
}

bool Tokenizerf::Init(const wxString& filename, FortranSourceForm sourceForm)
{
    BaseInit();
    if (filename.IsEmpty())
    {
        if (m_Filename.IsEmpty())
            return false;
    }
    else
        m_Filename = filename;

    if (!wxFileExists(m_Filename))
        return false;

    if (!ReadFile())
        return false;

    if (!m_BufferLen)
        return false;

    m_SourceForm = sourceForm;
    AdjustLineNumber();

    m_IsOK = true;
    return true;
}

bool Tokenizerf::InitFromBuffer(const wxString& buffer, FortranSourceForm sourceForm)
{
    BaseInit();
    m_Buffer = buffer;
    m_BufferLen = buffer.Length();
    m_IsOK = true;
    m_Filename.Clear();
    m_SourceForm = sourceForm;
    AdjustLineNumber();
    return true;
}

void Tokenizerf::BaseInit()
{
    m_TokenIndex = 0;
    m_UndoTokenIndex = 0;
    m_PeekedTokenIndex = 0;
    m_LineNumber = 1;
    m_UndoLineNumber = 1;
    m_PeekedLineNumber = 1;
    m_LineNumberStart = 1;
    m_UndoLineNumberStart = 1;
    m_PeekedLineNumberStart = 1;
    m_Column = 1;
    m_UndoColumn = 1;
    m_PeekedColumn = 1;
    m_BufferLen = 0;
    m_Buffer.Clear();
    m_IsOK = false;
    m_WasNextLine = false;
    m_UndoWasNextLine = false;
    m_PeekedWasNextLine = false;
    m_WasPeeked = false;
    m_LineStartIdx.clear();
    m_LineStartIdx.push_back(0);
}

bool Tokenizerf::ReadFile()
{
    if (!wxFileExists(m_Filename))
        return false;

    // open file
    wxFile file(m_Filename);

    //if (!cbRead(file,m_Buffer))
    if (!ReadFileToString(file,m_Buffer))
    {
        return false;
    }
    m_BufferLen = m_Buffer.Length();

    return true;
}

wxChar Tokenizerf::CurrentChar()
{
    return m_Buffer.GetChar(m_TokenIndex);
}

wxChar Tokenizerf::NextChar()
{
    if ((m_TokenIndex + 1) < 0 || (m_TokenIndex + 1) >= m_BufferLen)
        return 0;
    return m_Buffer.GetChar(m_TokenIndex + 1);
}

wxChar Tokenizerf::PreviousChar()
{
    if ((m_TokenIndex - 1) < 0 || (m_TokenIndex - 1) >= m_BufferLen)
        return 0;
    return m_Buffer.GetChar(m_TokenIndex - 1);
}

void Tokenizerf::AdjustLineNumber()
{
    if (m_WasNextLine)
    {
        ++m_LineNumber;
        m_WasNextLine = false;
        if (m_LineStartIdx.size() < m_LineNumber)
        {
            m_LineStartIdx.push_back(m_TokenIndex);
        }
    }
    if (CurrentChar() == '\n')
    {
        m_WasNextLine = true;
        m_Column = 0;
    }
}

bool Tokenizerf::MoveToNextChar()
{
    ++m_TokenIndex;
    ++m_Column;
    if (!IsEOF())
    {
        AdjustLineNumber();
        return true;
    }
    return false;
}

bool Tokenizerf::SkipWhiteSpace()
{
    // skip spaces, tabs, etc.
    while (!IsEOF() && isspace(CurrentChar()))
        MoveToNextChar();
    if (IsEOF())
        return false;
    return true;
}

bool Tokenizerf::SkipToChar(const wxChar& ch, bool toLineEnd)
{
    // skip everything until we find ch
    while (1)
    {
        while (!IsEOF() && CurrentChar() != ch && (!toLineEnd || (toLineEnd && CurrentChar() != '\n')))
            MoveToNextChar();
        break;
    }
    if (IsEOF())
        return false;
    return true;
}

bool Tokenizerf::CharInString(const char ch, const char* chars)
{
    int len = strlen(chars);
    for (int i = 0; i < len; ++i)
    {
        if (ch == chars[i])
            return true;
    }
    return false;
}


bool Tokenizerf::SkipToOneOfChars(const char* chars, bool toLineEnd)
{
    m_WasPeeked = false;
    // skip everything until we find any one of chars or end of line if toLineEnd
    while (1)
    {
        wxChar cch = CurrentChar();
        if(toLineEnd && cch == '\n')
        {
            break;
        }
        if(cch == '&')
        {
            MoveToNextChar();
            SkipWhiteSpace();
            cch = CurrentChar();
            if (cch == '\n' || cch == '\r' || cch == '!')
            {
                SkipToEOL();
                cch = CurrentChar();
            }
        }
        if(toLineEnd && cch == '!')
        {
            SkipToEOL();
            break;
        }
        else if(cch == '!')
        {
            SkipToEOL();
            cch = CurrentChar();
        }
        if(IsEOF() || CharInString(cch, chars))
        {
            break;
        }
        else
        {
            if (cch == '"' || cch == '\'')
            {
                // this is the case that match is inside a string!
                char ch = CurrentChar();
                MoveToNextChar();
                SkipToChar(ch, true);
            }
            MoveToNextChar();
        }
    }
    if (IsEOF())
        return false;
    return true;
}

bool Tokenizerf::SkipToEOL()
{
    // skip everything until we find EOL
    while (1)
    {
        while (!IsEOF() && CurrentChar() != '\n')
            MoveToNextChar();
        break;
    }
    if (IsEOF())
        return false;
    return true;
}

bool Tokenizerf::SkipBlock(const wxChar& ch, int maxLines)
{
    // skip blocks () [] {} <>
    wxChar match;
    switch (ch)
    {
    case '(':
        match = ')';
        break;
    case '[':
        match = ']';
        break;
    case '{':
        match = '}';
        break;
    case '<':
        match = '>';
        break;
    default :
        return false;
    }

    MoveToNextChar();
    int n_lines = 1;
    int count = 1; // counter for nested blocks (xxx())
    bool wasCont= false;
    while (!IsEOF())
    {
        while (!IsEOF())
        {
            if (CurrentChar() == '"' || CurrentChar() == '\'')
            {
                // this is the case that match is inside a string!
                char chOne = CurrentChar();
                MoveToNextChar();
                SkipToChar(chOne, true);
                MoveToNextChar();
            }
            else
                break;
        }
        if (CurrentChar() == ch)
            ++count;
        else if (CurrentChar() == match)
            --count;
        else if (CurrentChar() == '&' && m_SourceForm == fsfFree && !wasCont)
        {
            SkipToEOL();
            wasCont = true;
        }
        else if (maxLines > 0 && CurrentChar() == '\n')
        {
            n_lines++;
            if (n_lines > maxLines)
                count = 0;
        }
        else if (wasCont && m_SourceForm == fsfFree && !isspace(CurrentChar()))
            wasCont = false;

        MoveToNextChar();
        if (count == 0)
            break;
    }
    if (IsEOF())
        return false;
    return true;
}

bool Tokenizerf::SkipUnwanted()
{
    while ( //(CurrentChar() == '=' && !m_DetailedParsing) ||
            CurrentChar() == '!' ||
            ((CurrentChar() == 'c' || CurrentChar() == 'C' || CurrentChar() == '*') && m_Column == 1 && m_SourceForm == fsfFixed))
    {
        while ((CurrentChar() == 'c' || CurrentChar() == 'C' ||CurrentChar() == '*') && m_Column == 1 && m_SourceForm == fsfFixed)
        {
            if (IsBindTo())
                return true;
            SkipToEOL();
            if (!SkipWhiteSpace())
                return false;
        }
        while (CurrentChar() == '!')
        {
            if (IsBindTo())
                return true;
            SkipToEOL();
            if (!SkipWhiteSpace())
                return false;
        }

//        while (CurrentChar() == '=')
//        {
//            if (NextChar() != '>')
//            {
//                MoveToNextChar();
//                if (!SkipWhiteSpace())
//                    return false;
//                // skip assignments
//                if (CurrentChar() == '[' || CurrentChar() == '(')
//                    break;
//                if (!SkipToOneOfChars(";", true))
//                    return false;
//                if (!SkipWhiteSpace())
//                    return false;
//            }
//            else
//            {
//                return true;
//            }
//        }
    }
    return true;
}

wxString Tokenizerf::GetToken()
{
    m_UndoTokenIndex = m_TokenIndex;
    m_UndoLineNumber = m_LineNumber;
    m_UndoLineNumberStart = m_LineNumberStart;
    m_UndoColumn = m_Column;
    m_UndoWasNextLine = m_WasNextLine;
    if (m_WasPeeked)
    {
        m_TokenIndex = m_PeekedTokenIndex;
        m_LineNumber = m_PeekedLineNumber;
        m_LineNumberStart = m_PeekedLineNumberStart;
        m_Column = m_PeekedColumn;
        m_WasNextLine = m_PeekedWasNextLine;
        m_WasPeeked = false;
        return m_PeekedToken;
    }
    else
        return DoGetToken();
}

wxString Tokenizerf::GetTokenSameLine()
{
    unsigned int oldTokenIndex = m_TokenIndex;
    unsigned int oldLineNumber = m_LineNumber;
    unsigned int oldLineNumberStart = m_LineNumberStart;
    unsigned int oldColumn = m_Column;
    bool oldWasNextLine = m_WasNextLine;

    wxString token;

    if (m_WasPeeked)
    {
        m_TokenIndex = m_PeekedTokenIndex;
        m_LineNumber = m_PeekedLineNumber;
        m_LineNumberStart = m_PeekedLineNumberStart;
        m_Column = m_PeekedColumn;
        m_WasNextLine = m_PeekedWasNextLine;
        m_WasPeeked = false;
        token = m_PeekedToken;
    }
    else
        token = DoGetToken();


    if (oldLineNumber != m_LineNumberStart)
    {
        m_TokenIndex = oldTokenIndex;
        m_LineNumber = oldLineNumber;
        m_LineNumberStart = oldLineNumberStart;
        m_Column = oldColumn;
        m_WasNextLine = oldWasNextLine;
        token = wxEmptyString;
    }
    else
    {
        m_UndoTokenIndex = oldTokenIndex;
        m_UndoLineNumber = oldLineNumber;
        m_UndoLineNumberStart = oldLineNumberStart;
        m_UndoColumn = oldColumn;
        m_UndoWasNextLine = oldWasNextLine;
    }

    return token;
}

wxString Tokenizerf::GetTokenSameFortranLine()
{
    wxString token;

    if (m_SourceForm == fsfFree)
    {
        token = GetTokenSameLine();
        while (token.IsSameAs("&"))
        {
            token = GetToken();
        }
    }
    else
    {
        token = PeekToken();
        if (m_LineNumberStart == m_PeekedLineNumberStart)
            token = GetToken();
        else
        {
            if ( (m_PeekedColumn >= 7 && (m_PeekedColumn - token.Length()) >= 7) ||
                    ((m_PeekedColumn - token.Length()) < 6) ||
                    (token.Mid((token.Length() - (m_PeekedColumn - 6)),1).IsSameAs(_("0"))) )
                token = wxEmptyString;
            else
            {
                token = GetToken();
                if (m_Column > 7)
                    token = token.Mid(token.Length() - (m_Column - 7));
                else
                {
                    token = PeekToken();
                    if (m_LineNumberStart == m_PeekedLineNumberStart)
                        token = GetToken();
                    else
                        token = wxEmptyString;
                }
            }
        }
    }
    return token;
}


wxString Tokenizerf::PeekToken()
{
    unsigned int undoTokenIndex = m_TokenIndex;
    unsigned int undoLineNumber = m_LineNumber;
    unsigned int undoLineNumberStart = m_LineNumberStart;
    unsigned int undoColumn = m_Column;
    bool undoWasNextLine = m_WasNextLine;

    m_PeekedToken = DoGetToken();

    m_WasPeeked = true;
    m_PeekedTokenIndex = m_TokenIndex;
    m_PeekedLineNumber = m_LineNumber;
    m_PeekedLineNumberStart = m_LineNumberStart;
    m_PeekedColumn = m_Column;
    m_PeekedWasNextLine = m_WasNextLine;

    m_TokenIndex = undoTokenIndex;
    m_LineNumber = undoLineNumber;
    m_LineNumberStart = undoLineNumberStart;
    m_Column = undoColumn;
    m_WasNextLine = undoWasNextLine;

    return m_PeekedToken;
}

wxString Tokenizerf::PeekTokenSameFortranLine()
{
    unsigned int undoTokenIndex = m_TokenIndex;
    unsigned int undoLineNumber = m_LineNumber;
    unsigned int undoLineNumberStart = m_LineNumberStart;
    unsigned int undoColumn = m_Column;
    bool undoWasNextLine = m_WasNextLine;

    wxString token = GetTokenSameFortranLine();

    m_WasPeeked = false;

    m_TokenIndex = undoTokenIndex;
    m_LineNumber = undoLineNumber;
    m_LineNumberStart = undoLineNumberStart;
    m_Column = undoColumn;
    m_WasNextLine = undoWasNextLine;

    return token;
}

void Tokenizerf::UngetToken()
{
//    m_WasPeeked = true;
//	m_PeekedTokenIndex = m_TokenIndex;
//	m_PeekedLineNumber = m_LineNumber;
//	m_PeekedLineNumberStart = m_LineNumberStart;
//	m_PeekedColumn = m_Column;
//	m_PeekedWasNextLine = m_WasNextLine;

    m_WasPeeked = false;

    m_TokenIndex = m_UndoTokenIndex;
    m_LineNumber = m_UndoLineNumber;
    m_LineNumberStart = m_UndoLineNumberStart;
    m_Column = m_UndoColumn;
    m_WasNextLine = m_UndoWasNextLine;
}

wxString Tokenizerf::DoGetToken()
{
    if (IsEOF())
        return wxEmptyString;

    if (!SkipWhiteSpace())
        return wxEmptyString;

    if (!SkipUnwanted())
        return wxEmptyString;

    m_LineNumberStart = m_LineNumber;

    int start = m_TokenIndex;
    wxString ret_Str;

    if (IsBindTo())
    {
        m_TokenIndex += 8;
        ret_Str = "!bindto";
    }
    else if (isalpha(CurrentChar()) || CurrentChar() == '_' || CurrentChar() == '$' || CurrentChar() == '#')
    {
        // keywords, identifiers, etc.
        while (!IsEOF() &&
                (isalnum(CurrentChar()) ||
                 CurrentChar() == '_'    ||
                 CurrentChar() == '$'    ||
                 CurrentChar() == '#'))
            MoveToNextChar();
        if (IsEOF())
            return wxEmptyString;
        ret_Str = m_Buffer.Mid(start, m_TokenIndex - start);
    }
    else if (isdigit(CurrentChar()))
    {
        // numbers
        while (!IsEOF() && CharInString(CurrentChar(), "0123456789.abcdefABCDEFfXxLl"))
            MoveToNextChar();
        if (IsEOF())
            return wxEmptyString;
        ret_Str = m_Buffer.Mid(start, m_TokenIndex - start);
    }
    else if (CurrentChar() == '"' ||
             CurrentChar() == '\'')
    {
        // string, char, etc.
        wxChar match = CurrentChar();
        MoveToNextChar();  // skip starting ' or "
        if (!SkipToChar(match, true))
            return wxEmptyString;
        MoveToNextChar(); // skip ending ' or "
        ret_Str = m_Buffer.Mid(start, m_TokenIndex - start);
    }
    else if (CurrentChar() == ':')
    {
        if (NextChar() == ':')
        {
            MoveToNextChar();
            MoveToNextChar();
            ret_Str = "::";
        }
        else
        {
            MoveToNextChar();
            ret_Str = ":";
        }
    }
    else if (CurrentChar() == '=' && NextChar() == '>')
    {
        MoveToNextChar();
        MoveToNextChar();
        ret_Str = "=>";
    }
    else if (CurrentChar() == '(' || CurrentChar() == '[')
    {
        // skip block ()
        wxChar chBlock = CurrentChar();
        wxString tmp;
        if (m_SourceForm == fsfFree)
        {
            if (!SkipBlock(chBlock,1))
                return wxEmptyString;
            tmp = m_Buffer.Mid(start, m_TokenIndex - start);

            // skip fortran comments
            unsigned int tmpLen = tmp.Length() - 1;
            for (unsigned int i = 0; i < tmpLen; ++i)
            {
                if (tmp.GetChar(i) == '!')
                {
                    // replace comment line with spaces
                    tmp.SetChar(i,' ');
                    for(++i; i < tmpLen; ++i)
                    {
                        if (tmp.GetChar(i) == '\n')
                        {
                            tmp.SetChar(i,' ');
                            break;
                        }
                        else
                        {
                            tmp.SetChar(i,' ');
                        }
                    }
                }
            }
        }
        else
        {
            // fsfFixed
            if (!SkipBlock(chBlock, 20))
                return wxEmptyString;
            tmp = m_Buffer.Mid(start, m_TokenIndex - start);

            // skip fixed-form fortran comments
            int col = -1;
            unsigned int tmpLen = tmp.Length() - 1;
            for (unsigned int i = 0; i < tmpLen; ++i)
            {
                if (col !=  -1)
                {
                    col++;
                    if (col == 6 && tmp.GetChar(i) != ' ' && tmp.GetChar(i) != '0')
                    {
                        //this line is continuation line
                        tmp.SetChar(i,' ');
                    }
                    else if (col == 6)
                    {
                        // something is wrong
                        return wxEmptyString;
                    }
                }
                if ( (tmp.GetChar(i) == '!') ||
                        (col == 1 && (tmp.GetChar(i) == 'c' || tmp.GetChar(i) == 'C' || tmp.GetChar(i) == '*')) )
                {
                    // replace comment line with spaces
                    tmp.SetChar(i,' ');
                    for(++i; i < tmpLen; ++i)
                    {
                        if (tmp.GetChar(i) == '\n')
                        {
                            col = 0;
                            tmp.SetChar(i,' ');
                            break;
                        }
                        else
                        {
                            tmp.SetChar(i,' ');
                        }
                    }
                }
                else if (tmp.GetChar(i) == '\n')
                {
                    col = 0;
                }
            }
        }
        tmp.Replace("\t", " "); // replace tabs with spaces
        tmp.Replace("\n", " "); // replace LF with spaces
        tmp.Replace("\r", " "); // replace CR with spaces
        tmp.Replace("&", " "); // replace fortran line continuation with spaces
        // fix-up arguments (remove excessive spaces/tabs/newlines)
        unsigned int tmpLen = tmp.Length() - 1;
        for (unsigned int i = 0; i < tmpLen; ++i)
        {
            if (tmp.GetChar(i) == ' ' && tmp.GetChar(i + 1) == ' ')
                continue; // skip excessive spaces
            ret_Str << tmp.GetChar(i);
        }
        if (chBlock == '(')
        {
            ret_Str << ')'; // add closing parenthesis (see "i < tmp.Length() - 1" in previous "for")
            ret_Str.Replace("  ", " "); // replace two-spaces with single-space (introduced if it skipped comments or assignments)
            ret_Str.Replace("( ", "(");
            ret_Str.Replace(" )", ")");
        }
        else
        {
            ret_Str << ']'; // add closing parenthesis (see "i < tmp.Length() - 1" in previous "for")
            ret_Str.Replace("  ", " "); // replace two-spaces with single-space (introduced if it skipped comments or assignments)
            ret_Str.Replace("[ ", "[");
            ret_Str.Replace(" ]", "]");
        }
    }
    else
    {
        ret_Str = CurrentChar();
        MoveToNextChar();
    }

    return ret_Str;
}


void Tokenizerf::GetTokensToEOL(wxArrayString& arrStr, wxArrayString* arrStrLines)
{
    // get all tokens on line until EOL
    wxString o_tok;
    wxString tok;
    bool newLineNext = false;
    while (1)
    {
        unsigned int line = m_LineNumber;
        o_tok = tok;
        tok = GetToken();

        if (tok.IsEmpty())
            break;
        unsigned int n_line = m_LineNumber;
        if (m_SourceForm == fsfFree)
        {
            if ( (n_line > line) && !o_tok.IsSameAs("&") )
            {
                UngetToken();
                break;
            }
            else if (tok.IsSameAs(";"))
            {
                break;
            }
            else if (!tok.IsSameAs("&") && ((!m_DetailedParsing && !tok.IsSameAs(",")) || m_DetailedParsing) )
            {
                arrStr.Add(tok);
                if (arrStrLines)
                    arrStrLines->Add(GetCurrentLine());
            }
        }
        else
        {
            if ( (((n_line > line) && (m_Column != 0)) || newLineNext) && (m_Column != 7 || tok.Length() > 1) )
            {
                UngetToken();
                break;
            }
            else if (tok.IsSameAs(";") || (m_Column < 7 && m_Column != 0))
            {
                break;
            }
            else if ((m_Column > 7 || m_Column == 0) && ((!m_DetailedParsing && !tok.IsSameAs(",")) || m_DetailedParsing) )
            {
                arrStr.Add(tok);
                if (arrStrLines)
                    arrStrLines->Add(GetCurrentLine());
            }

            if (m_Column == 0)
                newLineNext = true;
            else
                newLineNext = false;
        }
    }
}


wxArrayString Tokenizerf::PeekTokensToEOL()
{
    // peek all tokens on line until EOL
    unsigned int undoTokenIndex = m_TokenIndex;
    unsigned int undoLineNumber = m_LineNumber;
    unsigned int undoLineNumberStart = m_LineNumberStart;
    unsigned int undoColumn = m_Column;
    bool undoWasNextLine = m_WasNextLine;

    wxArrayString arrStr;
    GetTokensToEOL(arrStr);

    m_WasPeeked = false;
    m_TokenIndex = undoTokenIndex;
    m_LineNumber = undoLineNumber;
    m_LineNumberStart = undoLineNumberStart;
    m_Column = undoColumn;
    m_WasNextLine = undoWasNextLine;

    return arrStr;
}

wxString Tokenizerf::GetCurrentLine()
{
    int curLineStart  = GetLineStartIndex(m_TokenIndex);
    int curLineEnd  = GetLineEndIndex(m_TokenIndex);
    wxString curLine = m_Buffer.Mid(curLineStart, curLineEnd - curLineStart);
    return curLine;
}

wxString Tokenizerf::GetLineFortran()
{
    // get current statements' line including continuation lines
    int curLineStart  = GetLineStartIndex(m_TokenIndex);
    int curInd = m_TokenIndex - curLineStart;
    int curLineEnd  = GetLineEndIndex(m_TokenIndex);
    wxString curLine = m_Buffer.Mid(curLineStart, curLineEnd - curLineStart);

    int comInd = curLine.Find('!');
    if (comInd != wxNOT_FOUND)
        curLine = curLine.Mid(0,comInd);

    bool startFound = false;
    bool endFound = false;
    int sc_ind = curLine.Find(';');
    if (sc_ind != wxNOT_FOUND)
    {
        if (sc_ind >= curInd)
        {
            curLine = curLine.Mid(0,sc_ind);
            endFound = true;
        }
        else
        {
            curLine = curLine.Mid(sc_ind+1);
            startFound = true;
        }

    }
    curLine = curLine.Trim().Trim(false);

    if (curLineStart != 0 && !startFound)
    {
        int beforeLineStart;
        int beforeLineEnd = curLineStart - 1;
        beforeLineStart = GetLineStartIndex(beforeLineEnd);

        while (beforeLineStart != 0)
        {
            wxString beforeLine = m_Buffer.Mid(beforeLineStart, beforeLineEnd - beforeLineStart);
            comInd = beforeLine.Find('!');
            if (comInd != wxNOT_FOUND)
                beforeLine = beforeLine.Mid(0,comInd);
            beforeLine = beforeLine.Trim().Trim(false);
            if (beforeLine.EndsWith("&"))
            {
                curLine = beforeLine.BeforeLast('&').Trim() + " " + curLine;
                sc_ind = curLine.Find(';');
                if (sc_ind != wxNOT_FOUND)
                {
                    curLine = curLine.Mid(sc_ind+1).Trim(false);
                    break;
                }
                beforeLineEnd = beforeLineStart - 1;
                beforeLineStart = GetLineStartIndex(beforeLineEnd);
            }
            else
            {
                break;
            }
        }
    }

    if (!endFound && curLine.EndsWith("&"))
    {
        curLine = curLine.BeforeLast('&').Trim();
        unsigned int afterLineStart = curLineEnd + 1;
        unsigned int afterLineEnd = GetLineEndIndex(afterLineStart);
        while (afterLineStart < afterLineEnd)
        {
            wxString afterLine = m_Buffer.Mid(afterLineStart, afterLineEnd - afterLineStart);
            comInd = afterLine.Find('!');
            if (comInd != wxNOT_FOUND)
                afterLine = afterLine.Mid(0,comInd);
            curLine  = curLine + " " + afterLine.Trim().Trim(false);
            sc_ind = curLine.Find(';');
            if (sc_ind != wxNOT_FOUND)
            {
                curLine = curLine.Mid(0,sc_ind).Trim();
                break;
            }
            if (curLine.EndsWith("&"))
            {
                curLine = curLine.BeforeLast('&');
                afterLineStart = afterLineEnd + 1;
                afterLineEnd = GetLineEndIndex(afterLineStart);
            }
            else
            {
                break;
            }
        }
    }
    return curLine;
}

unsigned int Tokenizerf::GetLineStartIndex(unsigned int indexInLine)
{
    unsigned int startIndex;
    bool foundEnd = false;
    for (int i=indexInLine-1; i>=0; i--)
    {
        if (m_Buffer.GetChar(i) == '\n')
        {
            startIndex = i+1;
            foundEnd = true;
            break;
        }
    }
    if (!foundEnd)
        startIndex = 0;

    return startIndex;
}

unsigned int Tokenizerf::GetLineEndIndex(unsigned int indexInLine)
{
    unsigned int endIndex;
    bool foundEnd = false;
    for (unsigned int i=indexInLine; i<m_Buffer.Len(); i++)
    {
        if (m_Buffer.GetChar(i) == '\n')
        {
            endIndex = i;
            foundEnd = true;
            break;
        }
    }
    if (!foundEnd)
        endIndex = m_Buffer.Len() - 1;

    return endIndex;
}

void Tokenizerf::SetDetailedParsing(bool detPars)
{
    m_DetailedParsing = detPars;
}

void Tokenizerf::SetFilename(const wxString& filename)
{
    m_Filename = filename;
}

wxString Tokenizerf::GetLine(unsigned int nl)
{
    // get line nl. Note: line numbers starts from 1.
    if (nl == 0 || nl > m_LineStartIdx.size())
        return wxEmptyString;

    unsigned int endIndex;
    if (nl >= m_LineStartIdx.size())
        endIndex = m_Buffer.Len() - 1;
    else
        endIndex = m_LineStartIdx[nl];

    wxString linenl = m_Buffer.Mid(m_LineStartIdx[nl-1], endIndex - m_LineStartIdx[nl-1]);
    return linenl;
}

bool Tokenizerf::IsBindTo()
{
    if (CurrentChar() == '!' ||
            ((CurrentChar() == 'c' || CurrentChar() == 'C' || CurrentChar() == '*') && m_Column == 1 && m_SourceForm == fsfFixed))
    {
        if (m_TokenIndex + 7 >= m_BufferLen)
            return false;

        wxString str = m_Buffer.Mid(m_TokenIndex+1,6);
        if (str.IsSameAs("bindto"),false)
        {
            unsigned int idx = m_TokenIndex + 7;
            if (m_Buffer.GetChar(idx) == ' ' || m_Buffer.GetChar(idx) == '\t')
                return true;
        }
    }
    return false;
}
