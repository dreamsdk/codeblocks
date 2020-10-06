
#ifndef INDENTMAKER_H
#define INDENTMAKER_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <cbstyledtextctrl.h>
#endif

#include "indentestimator.h"

class IsMultiLines
{
public :
	IsMultiLines()
	{
		haveMultiLines = false;
		iFirstLineNo = -1;
		iEndLineNo = -1;
	}

	~IsMultiLines()
	{
	}

    void reset()
    {
        haveMultiLines = false;
		iFirstLineNo = -1;
		iEndLineNo = -1;
    }

	bool haveMultiLines;
	int iFirstLineNo;
	int iEndLineNo;
};



class FormatIndent
{
public:
    /** Constructor. */
    FormatIndent();
    /** Destructor. */
    ~FormatIndent();

    void Format();

private:
    void FormatProject();
    void FormatActiveFile();
    void FormatFile(const wxString &filename);
    void FormatSelection();
    void ReadConfig();
    void FormatText(const wxString& textIn, int indentStart, const wxString& eolChars, wxString& formattedText);
    void ReplaceTextInEditor(const wxString& text, bool isSelection, cbStyledTextCtrl* control);
    wxString GetEOLChars(cbStyledTextCtrl* control);

    IndentEstimator m_IndentEstimator;

    wxString m_IndentStr;
    bool m_TrimFromRight;
};

#endif // INDENTMAKER_H
