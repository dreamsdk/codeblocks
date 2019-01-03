/*
 * This file is part of the FortranProject plugin for Code::Blocks IDE
 * and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 */

#ifndef FTOKINIZER_H
#define FTOKINIZER_H

#include <wx/string.h>
#include <filemanager.h>
#include <vector>


//bool cbRead(wxFile& file,wxString& st);

// Writes a wxString to a non-unicode file. File must be open. File is closed automatically.
//bool cbWrite(wxFile& file, const wxString& buff);

bool ReadFileToString(wxFile& file,wxString& st);

enum FortranSourceForm
{
    fsfFixed=0,
    fsfFree,
};

class Tokenizerf
{
	public:
		Tokenizerf(const wxString& filename = wxEmptyString, FortranSourceForm sourceForm = fsfFree);
		~Tokenizerf();

		bool Init(const wxString& filename, FortranSourceForm sourceForm);
		bool InitFromBuffer(const wxString& buffer, FortranSourceForm sourceForm);
		wxString GetToken();
		wxString GetTokenSameLine();
		wxString GetTokenSameFortranLine();
		wxString PeekToken();
		wxString PeekTokenSameFortranLine();
		const wxString& GetFilename(){ return m_Filename; }
		unsigned int GetLineNumber(){ return m_LineNumber; }
		unsigned int GetPeekedLineNumber(){ return m_PeekedLineNumber; }
		unsigned int GetCurrentIndex(){ return m_TokenIndex; }
		unsigned int GetLineCount(){ return m_LineStartIdx.size(); }
		bool IsOK(){ return m_IsOK; }
		bool SkipToOneOfChars(const char* chars, bool toLineEnd = false);
		wxArrayString GetTokensToEOL(wxArrayString* arrStrLines = 0);
		wxArrayString PeekTokensToEOL();
		wxString GetCurrentLine();
		wxString GetLineFortran();
		wxString GetLine(unsigned int nl);
		unsigned int GetLineStartIndex(unsigned int indexInLine);
		unsigned int GetLineEndIndex(unsigned int indexInLine);
		void SetDetailedParsing(bool detPars);
		void SetFilename(const wxString& filename);
        void UngetToken();
		bool SkipToEOL();
	protected:
		void BaseInit();
		wxString DoGetToken();
		bool ReadFile();
		bool SkipWhiteSpace();
		bool SkipToChar(const wxChar& ch, bool toLineEnd = false);
		bool SkipBlock(const wxChar& ch, int maxLines = 0);
		bool SkipUnwanted(); // skips comments, assignments, preprocessor etc.
		bool IsEOF(){ return m_TokenIndex >= m_BufferLen; }
		bool MoveToNextChar();
		void AdjustLineNumber();
		wxChar CurrentChar();
		wxChar NextChar();
		wxChar PreviousChar();
		bool IsBindTo();
	private:
		bool CharInString(const char ch, const char* chars);
		wxString m_Filename;
		wxString m_Buffer;
		unsigned int m_BufferLen;
		unsigned int m_TokenIndex;
		unsigned int m_UndoTokenIndex;
		unsigned int m_PeekedTokenIndex;
		unsigned int m_LineNumber;
		unsigned int m_LineNumberStart;
		unsigned int m_UndoLineNumber;
		unsigned int m_UndoLineNumberStart;
		unsigned int m_PeekedLineNumber;
		unsigned int m_PeekedLineNumberStart;
		unsigned int m_Column;
		unsigned int m_UndoColumn;
		unsigned int m_PeekedColumn;
		bool m_WasNextLine;
		bool m_UndoWasNextLine;
		bool m_PeekedWasNextLine;
		bool m_WasPeeked;
		bool m_IsOK;
		FortranSourceForm m_SourceForm;
		wxString m_PeekedToken;
		bool m_DetailedParsing;
		std::vector<unsigned int> m_LineStartIdx;
};

#endif // FTOKINIZER_H
