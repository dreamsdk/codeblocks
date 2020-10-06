///////////////////////////////////////////////////////////////////////////////
// Author:      YWX (wxFortranIndent@163.com)
// Licence:     GNU General Public License, version 3
///////////////////////////////////////////////////////////////////////////////

#ifndef INDENTESTIMATOR_H
#define INDENTESTIMATOR_H

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/string.h>
    #include <wx/regex.h>
    #include <wx/hashmap.h>
    #include <wx/arrstr.h>
#endif
#include <vector>

class FormatIndentCodeTree
{
public:
    enum CodeTreeKind
    {
        ctcProgramBlock,
        ctcEndProgramBlockSubFun,
        ctcModule,
        ctcEndModule,
        ctcInterface,
        ctcEndInterface,
        ctcSubroutine,
        ctcFunction,
        ctcTypeDefine,
        ctcEndTypeDefine,
        ctcDoForall,
        ctcEndDoForall,
        ctcSeclectCase,
        ctcSeclectType,
        ctcEndSeclectCase,
        ctcIfThen,
        ctcEndIf,
        ctcWhere,
        ctcEndWhere,
        ctcAssociate,
        ctcEndAssociate,
        ctcCritical,
        ctcEndCritical,
        ctcSubmodule,
        ctcEndSubmodule,
        ctcEnum,
        ctcEndEnum,
        ctcEnd,
        ctcModuleProcedure,
        ctcEndProcedure,

        ctcNone
    };

public:
    FormatIndentCodeTree();
    ~FormatIndentCodeTree();
    void Initialize(int firstLineIndent);
    void GetCodeTreeIndent(CodeTreeKind iKind, int& myIndent);
    CodeTreeKind GetParentKind();

private:
    typedef struct {
        CodeTreeKind kind;
        int indent;
    } treeNode;

    std::vector<treeNode> m_Tree;
    int m_RootIndent;
};

/// declaration
WX_DECLARE_STRING_HASH_MAP( wxRegEx *, FormatIndentRegEx );

class IndentEstimator
{
public:

    /** Constructor. */
    IndentEstimator( )
	{
		CreateFormatIndentRegEx();
	}

    /** Destructor. */
	~IndentEstimator( )
	{
		DelFormatIndentRegEx();
	}

	void CreateFormatIndentRegEx();
	void DelFormatIndentRegEx();
	void Initialize(int firstLineIndent);

	bool BuffersDiffer( const wxString &a, const wxString &b );
	bool GetIsHasLineContinuation( const wxString & srcLine );
	bool GetIsHasPreprocessor( const wxString & srcLine );
	void DelLineContinuation( wxString & srcLine );
	void DelComment( wxString & srcLine );
	void GetFortranIndentLine( const wxString& src, int& indentNum, int& indentNumNext);
	void CutStringAndComment(wxString& src);
	void ReadConfig();

protected:
	void CalcFortranIndentLine( const wxString & srcLine, int & deltaIndentCur, int & deltaIndentNext, FormatIndentCodeTree::CodeTreeKind & iKind );
	void PrepareLine(const wxString & srcIn, wxArrayString & srcLines);

	FormatIndentRegEx m_RegEx;
	FormatIndentCodeTree m_CodeTree;

	bool m_IndentProgFunSub;
	bool m_IndentModule;
	bool m_IndentContainsModule;
    bool m_IndentContainsModuleAfter;
    bool m_IndentContainsProcedure;
    bool m_IndentContainsProcedureAfter;
    bool m_IndentContainsTypedef;
    bool m_IndentContainsTypedefAfter;
    bool m_IndentSelectCaseAfter;
    bool m_IndentSelectTypeAfter;
};


#endif // INDENTESTIMATOR_H

