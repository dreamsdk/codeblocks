
#include "indentestimator.h"

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <configmanager.h>
#endif

#include <wx/tokenzr.h>

FormatIndentCodeTree::FormatIndentCodeTree()
{
    Initialize(0);
}

FormatIndentCodeTree::~FormatIndentCodeTree()
{

}

void FormatIndentCodeTree::Initialize(int firstLineIndent)
{
    m_Tree.clear();
    m_RootIndent = firstLineIndent;
}

void FormatIndentCodeTree::GetCodeTreeIndent(CodeTreeKind iKind, int& myIndent)
{
    if (iKind == ctcNone)
        return;
    else if (iKind == ctcAssociate || iKind == ctcCritical || iKind == ctcDoForall || iKind == ctcEnum ||
             iKind == ctcFunction || iKind == ctcIfThen || iKind == ctcInterface || iKind == ctcProgramBlock ||
             iKind == ctcModule || iKind == ctcSeclectCase || iKind == ctcSeclectType ||
             iKind == ctcSubmodule || iKind == ctcSubroutine || iKind == ctcTypeDefine ||
             iKind == ctcWhere)
    {
        treeNode tn;
        tn.indent = myIndent;
        tn.kind = iKind;
        m_Tree.push_back(tn);
        return;
    }

    CodeTreeKind startKind1 = ctcNone;
    CodeTreeKind startKind2 = ctcNone;
    CodeTreeKind startKind3 = ctcNone;
    CodeTreeKind startKind4 = ctcNone;
    CodeTreeKind startKind5 = ctcNone;
    if (iKind == ctcEndAssociate)
        startKind1 = ctcAssociate;
    else if (iKind == ctcEndCritical)
        startKind1 = ctcCritical;
    else if (iKind == ctcEndDoForall)
        startKind1 = ctcDoForall;
    else if (iKind == ctcEndEnum)
        startKind1 = ctcEnum;
    else if (iKind == ctcEndInterface)
        startKind1 = ctcInterface;
    else if (iKind == ctcEndProgramBlockSubFun)
    {
        startKind1 = ctcProgramBlock;
        startKind2 = ctcSubroutine;
        startKind3 = ctcFunction;
    }
    else if (iKind == ctcEndModule)
        startKind1 = ctcModule;
    else if (iKind == ctcEndSeclectCase)
    {
        startKind1 = ctcSeclectCase;
        startKind2 = ctcSeclectType;
    }
    else if (iKind == ctcEndSubmodule)
        startKind1 = ctcSubmodule;
    else if (iKind == ctcEndTypeDefine)
        startKind1 = ctcTypeDefine;
    else if (iKind == ctcEndWhere)
        startKind1 = ctcWhere;
    else if (iKind == ctcEndIf)
        startKind1 = ctcIfThen;
    else if (iKind == ctcEnd)
    {
        startKind1 = ctcProgramBlock;
        startKind2 = ctcSubroutine;
        startKind3 = ctcFunction;
        startKind4 = ctcModule;
        startKind5 = ctcSubmodule;
    }

    for (int i=int(m_Tree.size())-1; i >= 0; i--)
    {
        if ((m_Tree[i].kind == startKind1) ||
            (m_Tree[i].kind == startKind2) ||
            (m_Tree[i].kind == startKind3) ||
            (m_Tree[i].kind == startKind4) ||
            (m_Tree[i].kind == startKind5))
        {
            myIndent = m_Tree[i].indent;
            m_Tree.resize(i);
            break;
        }
    }
}

FormatIndentCodeTree::CodeTreeKind FormatIndentCodeTree::GetParentKind()
{
    CodeTreeKind pK = ctcNone;
    if (m_Tree.size() > 0)
        pK = m_Tree[m_Tree.size()-1].kind;

    return pK;
}




//***************************************************************
//*
//***************************************************************

void IndentEstimator::DelFormatIndentRegEx()
{
    if( m_RegEx.empty() )
    {
        return ;
    }
    FormatIndentRegEx::iterator it = m_RegEx.begin();
    // iterate over all the Hash Map
    while( it != m_RegEx.end() )
    {
        wxRegEx * pIt = it->second;
        delete pIt;
        ++it;
    }
    m_RegEx.clear();
}


void IndentEstimator::CreateFormatIndentRegEx()
{
#if wxCHECK_VERSION(3, 1, 6)
	int options = wxRE_EXTENDED | wxRE_ICASE ;
#else
	int options = wxRE_ADVANCED | wxRE_ICASE ;
#endif // wxCHECK_VERSION

	DelFormatIndentRegEx();

    wxString startLabel = "^([\\s\\t]*)(([0-9]+)([\\s\\t]+)|(([a-z0-9_]+)([\\s\\t]*)(:)))?([\\s\\t]*)";

	m_RegEx["regexMultiLines"] = new wxRegEx( "(&)((\r\n)|(\r)|(\n))?$", options );
	m_RegEx["regexEndProgram"] = new wxRegEx( "^(\\s*)(end)(\\s*)((program)|((block)(\\s*)(data))|(subroutine)|(function))((\\s+)([a-zA-Z0-9_]+))?((\\s*)!(.*))?(\\s*)$", options );
	m_RegEx["regexProgram"] = new wxRegEx( "^(\\s*)((program)|((block)(\\s*)(data)))((\\s+)([a-zA-Z0-9_]+))?((\\s*)!(.*))?(\\s*)$", options );
	m_RegEx["regexEndModule"] = new wxRegEx( "^(\\s*)(end)(\\s*)(module)((\\s+)([a-zA-Z0-9_]+))?((\\s*)!(.*))?(\\s*)$", options );
	m_RegEx["regexModule"] = new wxRegEx( "^(\\s*)(module)((\\s+)([a-zA-Z0-9_]+))?((\\s*)!(.*))?(\\s*)$", options );
	m_RegEx["regexInterface"] = new wxRegEx( "^(\\s*)((abstract)(\\s+))?(interface)((\\s+)(([a-zA-Z0-9_]+)|((assignment)(\\s*)\\((\\s*)(a+)(\\s*)\\))|((operator)(\\s*)\\((.+)\\))|((write|read)(\\s*)\\((.+)\\))))?((\\s*)!(.*))?(\\s*)$", options );
	m_RegEx["regexEndInterface"] = new wxRegEx( "^(\\s*)(end)(\\s*)(interface)((\\s+)(([a-zA-Z0-9_]+)|((assignment)(\\s*)\\((\\s*)(a+)(\\s*)\\))|((operator)(\\s*)\\((.+)\\))|((write|read)(\\s*)\\((.+)\\))))?((\\s*)!(.*))?(\\s*)$", options );
	m_RegEx["regexContains"] = new wxRegEx( "^(\\s*)(contains)((\\s*)!(.*))?(\\s*)$", options );
	m_RegEx["regexSubroutine"] = new wxRegEx( "^(\\s*)(((pure)|(impure)|(elemental)|(recursive)|(non_recursive)|(module))(\\s+))*((\\s+))?(subroutine)(\\s+)([a-zA-Z0-9_]+)((\\s*)(\\()(\\s*)(([a-zA-Z0-9_]+)((\\s*)(,)(\\s*)([a-zA-Z0-9_]+))*)?(\\s*)(\\))(\\s*))?", options );
	m_RegEx["regexFunction"] = new wxRegEx( "^(((.*)(\\s+))|(\\s*)){1}(function)(\\s+)([a-zA-Z0-9_]+)(\\s*)\\((\\s*)(([a-zA-Z0-9_]+)((\\s*)(,)(\\s*)([a-zA-Z0-9_]+))*)?(\\s*)\\)", options );
	m_RegEx["regexType"] = new wxRegEx( "^(\\s*)((type)(\\s*)(\\()(\\s*)([a-zA-Z0-9_]+)(\\s*)(\\)))(\\s*)", options );
#if wxCHECK_VERSION(3, 1, 6)
	m_RegEx["regexTypeDefine"] = new wxRegEx( "^(\\s*)((type)\\b((\\s*),(\\s*)((public)|(private)|(protected)))?((\\s*),(\\s*)((abstract)|((extends)(\\s*)\\((\\s*)([a-zA-Z0-9_]+)(\\s*)\\))))?((\\s*),(\\s*)((bind)(\\s*)\\((\\s*)([a-z0-9_]+)(\\s*)\\)))?((\\s*)(::)?(\\s*)([a-zA-Z0-9_]+)))(\\s*)", options );
#else
	m_RegEx["regexTypeDefine"] = new wxRegEx( "^(\\s*)((type)\\M((\\s*),(\\s*)((public)|(private)|(protected)))?((\\s*),(\\s*)((abstract)|((extends)(\\s*)\\((\\s*)([a-zA-Z0-9_]+)(\\s*)\\))))?((\\s*),(\\s*)((bind)(\\s*)\\((\\s*)([a-z0-9_]+)(\\s*)\\)))?((\\s*)(::)?(\\s*)([a-zA-Z0-9_]+)))(\\s*)", options );
#endif
	m_RegEx["regexEndType"] = new wxRegEx( "^(\\s*)(end)(\\s*)(type)((\\s+)([a-zA-Z0-9_]+))?(\\s*)", options );
	m_RegEx["regexEndDo"] = new wxRegEx( "^(\\s*)(end)(\\s*)((do)|(forall))((\\s+)([a-zA-Z0-9_]+))?(\\s*)", options );
	m_RegEx["regexDo"] = new wxRegEx( startLabel + "(do)((\\s+)([a-zA-Z0-9_])(.+))?((\\s*)!(.*))?(\\s*)$", options );
	m_RegEx["regexForall"] = new wxRegEx( startLabel + "((forall)|((do)(\\s+)(while)))(\\s*)(\\([^\\)]+\\))(\\s*)$", options );
	m_RegEx["regexEndSelect"] = new wxRegEx( "^(\\s*)(end)(\\s*)(select)((\\s+)([a-zA-Z0-9_]+))?(\\s*)", options );
	m_RegEx["regexSelectCase"] = new wxRegEx( startLabel + "((select)(\\s*)(case))(\\s*)(\\()(.+)(\\))((\\s*)!(.*))?(\\s*)$", options );
	m_RegEx["regexSelectType"] = new wxRegEx( startLabel + "((select)(\\s*)(type))(\\s*)(\\()(.+)(\\))((\\s*)!(.*))?(\\s*)$", options );
	m_RegEx["regexCase"] = new wxRegEx( "^(\\s*)((((case)|(((type)|(class))(\\s+)(is)))(\\s*)(\\()(.+)(\\))((\\s+)([a-zA-Z0-9_]+))?)|(((case)|(class))(\\s+)(default)((\\s+)([a-zA-Z0-9_]+))?))((\\s*)!(.*))?(\\s*)$", options );
	m_RegEx["regexIfThen"] = new wxRegEx( startLabel + "((if)(\\s*)(\\()(.+)(\\))(\\s*)(then))((\\s*)!(.*))?(\\s*)$", options );
	m_RegEx["regexElse"] = new wxRegEx( "^(\\s*)(else)(\\s*)(((\\s+)([a-zA-Z0-9_]+))|((if)(\\s*)(\\()(.+)(\\))(\\s*)(then)((\\s+)([a-zA-Z0-9_]+))?))?((\\s*)!(.*))?(\\s*)$", options );
	m_RegEx["regexEndIf"] = new wxRegEx( "^(\\s*)(end)(\\s*)(if)((\\s+)([a-zA-Z0-9_]+))?(\\s*)", options );
	m_RegEx["regexWhere"] = new wxRegEx( startLabel + "(where)(\\s*)(\\([^\\)]+\\))(\\s*)$", options );
	m_RegEx["regexElseWhere"] = new wxRegEx( "^(\\s*)(else)(\\s*)(where)(((\\s+)([a-zA-Z0-9_]+))|((\\s*)\\((.+)\\)((\\s+)([a-zA-Z0-9_]+))?))?((\\s*)!(.*))?(\\s*)$", options );
	m_RegEx["regexEndWhere"] = new wxRegEx( "^(\\s*)(end)(\\s*)(where)((\\s+)([a-zA-Z0-9_]+))?(\\s*)", options );
	m_RegEx["regexEndOnly"] = new wxRegEx( "^(\\s*)(end)((\\s*)!(.*))?(\\s*)$", options );

	m_RegEx["regexAssociate"] = new wxRegEx( startLabel + "(associate)(\\s*)\\((.+)\\)((\\s*)!(.*))?(\\s*)$", options );
	m_RegEx["regexEndAssociate"] = new wxRegEx( "^(\\s*)(end)(\\s*)(associate)((\\s+)([a-zA-Z0-9_]+))?((\\s*)!(.*))?(\\s*)$", options );

	m_RegEx["regexCritical"] = new wxRegEx( startLabel + "((block)|(critical))((\\s*)!(.*))?(\\s*)$", options );
	m_RegEx["regexEndCritical"] = new wxRegEx( "^(\\s*)(end)(\\s*)((block)|(critical))((\\s+)([a-zA-Z0-9_]+))?((\\s*)!(.*))?(\\s*)$", options );

	m_RegEx["regexSubmodule"] = new wxRegEx( "^(\\s*)(([a-zA-Z0-9_]+)(\\s*)(:)(\\s*))?((submodule)(\\s*)\\((.+)\\)(\\s*)([a-zA-Z0-9_]+))((\\s*)!(.*))?(\\s*)$", options );
	m_RegEx["regexEndSubmodule"] = new wxRegEx( "^(\\s*)(end)(\\s*)(submodule)((\\s+)([a-zA-Z0-9_]+))?((\\s*)!(.*))?(\\s*)$", options );

	m_RegEx["regexModuleProcedure"] = new wxRegEx( "^(\\s*)(module(\\s*)procedure)(\\s*)([a-zA-Z0-9_]+)((\\s*)!(.*))?(\\s*)$", options );
	m_RegEx["regexEndProcedure"] = new wxRegEx( "^(\\s*)(end)(\\s*)(procedure)((\\s+)([a-zA-Z0-9_]+))?((\\s*)!(.*))?(\\s*)$", options );

	m_RegEx["regexEnum"] = new wxRegEx( "^(\\s*)(enum)((\\s*),(\\s*)((bind)(\\s*)\\((\\s*)([a-zA-Z0-9_]+)(\\s*)\\)))((\\s*)!(.*))?(\\s*)$", options );
	m_RegEx["regexEndEnum"] = new wxRegEx( "^(\\s*)(end)(\\s*)(enum)((\\s*)!(.*))?(\\s*)$", options );

	m_RegEx["regexComment"] = new wxRegEx( "(!(.*))((\r\n)|(\r)|(\n))?$", options | wxRE_NEWLINE );

	m_RegEx["regexBlankLine"] = new wxRegEx( "([ \t]+)((\r\n)|(\r)|(\n))", options | wxRE_NEWLINE );

	m_RegEx["regexPreprocessorC"] = new wxRegEx( "^#(define|undef|ifdef|ifndef|if|elif|else|endif|include|error|warning|line)", options );

}


void IndentEstimator::Initialize(int firstLineIndent)
{
    m_CodeTree.Initialize(firstLineIndent);
}


void IndentEstimator::GetFortranIndentLine(const wxString& src1, int& indentNum, int& indentNumNext)
{
    indentNumNext = indentNum;
    wxArrayString srcLines;
    PrepareLine(src1, srcLines); // for case if several statements on one line
    for (size_t i=0; i<srcLines.size(); i++)
    {
        int deltaIndentCur;
        int deltaIndentNext;
        FormatIndentCodeTree::CodeTreeKind iKind;
        CalcFortranIndentLine(srcLines.Item(i), deltaIndentCur, deltaIndentNext, iKind);
        if (i == 0)
        {
            indentNum += deltaIndentCur;
            m_CodeTree.GetCodeTreeIndent(iKind, indentNum);
        }

        if (i+1 == srcLines.size())
        {
            m_CodeTree.GetCodeTreeIndent(iKind, indentNum);
            indentNumNext = indentNum + deltaIndentNext;
        }
    }
}


void IndentEstimator::CalcFortranIndentLine( const wxString & srcLine, int & deltaIndentCur, int & deltaIndentNext, FormatIndentCodeTree::CodeTreeKind & iKind )
{
    // module program subroutine function forall
    // Add a shiftwidth to statements following module, program, subroutine,
    // function and forall statements
    //Manager::Get()->GetLogManager()->Log( src1 );

    iKind = FormatIndentCodeTree::ctcNone;

    deltaIndentCur = 0;
    deltaIndentNext = 0;

    wxString src = srcLine;
    src.Trim(true); // trim from right

    // Program, Interface, Bblock Data, Subroutine, Function
    if ( m_RegEx["regexEndProgram"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcEndProgramBlockSubFun;
        if (m_IndentProgFunSub)
            deltaIndentCur = -1;
        else
            deltaIndentCur = 0;
        deltaIndentNext = 0;
    }
    else if ( m_RegEx["regexEndInterface"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcEndInterface;
        deltaIndentCur = -1;
        deltaIndentNext = 0;
    }
	else if ( m_RegEx["regexInterface"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcInterface;
        deltaIndentCur = 0;
        deltaIndentNext = 1;
    }
    else if ( m_RegEx["regexSubroutine"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcSubroutine;
        deltaIndentCur = 0;
        if (m_IndentProgFunSub)
            deltaIndentNext = 1;
        else
            deltaIndentNext = 0;
    }
    else if ( m_RegEx["regexFunction"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcFunction;
        deltaIndentCur = 0;
        if (m_IndentProgFunSub)
            deltaIndentNext = 1;
        else
            deltaIndentNext = 0;
    }
    else if ( m_RegEx["regexProgram"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcProgramBlock;
        deltaIndentCur = 0;
        if (m_IndentProgFunSub)
            deltaIndentNext = 1;
        else
            deltaIndentNext = 0;
        return ;
    }
    else if ( m_RegEx["regexEndModule"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcEndModule;
        if (m_IndentModule)
            deltaIndentCur = -1;
        else
            deltaIndentCur = 0;
        deltaIndentNext = 0;
    }
    else if ( m_RegEx["regexModule"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcModule;
        deltaIndentCur = 0;
        if (m_IndentModule)
            deltaIndentNext = 1;
        else
            deltaIndentNext = 0;
    }
    else if ( m_RegEx["regexContains"]->Matches( src ) )
    {
        FormatIndentCodeTree::CodeTreeKind pk = m_CodeTree.GetParentKind();

        if (pk == FormatIndentCodeTree::ctcModule ||
            pk == FormatIndentCodeTree::ctcSubmodule)
        {
            if (m_IndentContainsModule)
                deltaIndentCur = -1;
            if (m_IndentContainsModuleAfter)
                deltaIndentNext = 1;
        }
        else if (pk == FormatIndentCodeTree::ctcSubroutine ||
                 pk == FormatIndentCodeTree::ctcFunction ||
                 pk == FormatIndentCodeTree::ctcProgramBlock)
        {
            if (m_IndentContainsProcedure)
                deltaIndentCur = -1;
            if (m_IndentContainsProcedureAfter)
                deltaIndentNext = 1;
        }
        else if (pk == FormatIndentCodeTree::ctcTypeDefine)
        {
            if (m_IndentContainsTypedef)
                deltaIndentCur = -1;
            if (m_IndentContainsTypedefAfter)
                deltaIndentNext = 1;
        }
        else // could it be?
        {
            deltaIndentCur = -1;
            deltaIndentNext = 1;
        }
    }
    else if ( m_RegEx["regexEndDo"]->Matches( src ) )
    {
        // do while # end do
        // forall() # end forall
        // Indent do loops only if they are all guaranteed to be of do/end do type
        iKind = FormatIndentCodeTree::ctcEndDoForall;
        deltaIndentCur = -1;
        deltaIndentNext = 0;
    }
    else if ( m_RegEx["regexDo"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcDoForall;
        deltaIndentCur = 0;
        deltaIndentNext = 1;
    }
    else if ( m_RegEx["regexForall"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcDoForall;
        deltaIndentCur = 0;
        deltaIndentNext = 1;
    }
    else if ( m_RegEx["regexEndSelect"]->Matches( src ) )
    {
        // select case # case | case dafault # end select
        iKind = FormatIndentCodeTree::ctcEndSeclectCase;
        deltaIndentCur = -2;
        deltaIndentNext = 0;
    }
    else if ( m_RegEx["regexSelectCase"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcSeclectCase;
        deltaIndentCur = 0;
        if (m_IndentSelectCaseAfter)
            deltaIndentNext = 2;
        else
            deltaIndentNext = 1;
    }
    else if ( m_RegEx["regexSelectType"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcSeclectType;
        deltaIndentCur = 0;
        if (m_IndentSelectTypeAfter)
            deltaIndentNext = 2;
        else
            deltaIndentNext = 1;
    }
    else if ( m_RegEx["regexCase"]->Matches( src ) )
    {
        deltaIndentCur = -1;
        deltaIndentNext = 1;
    }
    else if ( m_RegEx["regexIfThen"]->Matches( src ) )
    {
        // if ## if then # else # end if
        iKind = FormatIndentCodeTree::ctcIfThen;
        deltaIndentCur = 0;
        deltaIndentNext = 1;
    }
    else if ( m_RegEx["regexElse"]->Matches( src ) )
    {
        deltaIndentCur = -1;
        deltaIndentNext = 1;
    }
    else if ( m_RegEx["regexEndIf"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcEndIf;
        deltaIndentCur = -1;
        deltaIndentNext = 0;
    }
    else if ( m_RegEx["regexWhere"]->Matches( src ) )
    {
        // where elsewhere
        iKind = FormatIndentCodeTree::ctcWhere;
        deltaIndentCur = 0;
        deltaIndentNext = 1;
    }
    else if ( m_RegEx["regexElseWhere"]->Matches( src ) )
    {
        deltaIndentCur = -1;
        deltaIndentNext = 1;
    }
    else if ( m_RegEx["regexEndWhere"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcEndWhere;
        deltaIndentCur = -1;
        deltaIndentNext = 0;
    }
    else if ( m_RegEx["regexEndType"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcEndTypeDefine;
        deltaIndentCur = -1;
        deltaIndentNext = 0;
    }
    else if ( ( false == m_RegEx["regexType"]->Matches( src ) ) &&
            ( m_RegEx["regexTypeDefine"]->Matches( src ) ) )
    {
        iKind = FormatIndentCodeTree::ctcTypeDefine;
        deltaIndentCur = 0;
        deltaIndentNext = 1;
    }
    else if ( m_RegEx["regexEndAssociate"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcEndAssociate;
        deltaIndentCur = -1;
        deltaIndentNext = 0;
    }
	else if ( m_RegEx["regexAssociate"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcAssociate;
        deltaIndentCur = 0;
        deltaIndentNext = 1;
    }
    else if ( m_RegEx["regexEndCritical"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcEndCritical;
        deltaIndentCur = -1;
        deltaIndentNext = 0;
    }
	else if ( m_RegEx["regexCritical"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcCritical;
        deltaIndentCur = 0;
        deltaIndentNext = 1;
    }
	else if ( m_RegEx["regexSubmodule"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcSubmodule;
        deltaIndentCur = 0;
        if (m_IndentModule)
            deltaIndentNext = 1;
        else
            deltaIndentNext = 0;
    }
	else if ( m_RegEx["regexEndSubmodule"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcEndSubmodule;
        if (m_IndentModule)
            deltaIndentCur = -1;
        else
            deltaIndentCur = 0;
        deltaIndentNext = 0;
    }
    else if ( m_RegEx["regexEndEnum"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcEndEnum;
        deltaIndentCur = -1;
        deltaIndentNext = 0;
    }
	else if ( m_RegEx["regexEnum"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcEnum;
        deltaIndentCur = 0;
        deltaIndentNext = 1;
    }
    else if ( m_RegEx["regexEndOnly"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcEnd;
        deltaIndentCur = -1;
        deltaIndentNext = 0;
    }
    else if ( m_RegEx["regexModuleProcedure"]->Matches( src ) &&
              m_CodeTree.GetParentKind() == FormatIndentCodeTree::ctcSubmodule )
    {
        iKind = FormatIndentCodeTree::ctcModuleProcedure;
        deltaIndentCur = 0;
        if (m_IndentProgFunSub)
            deltaIndentNext = 1;
        else
            deltaIndentNext = 0;
    }
    else if ( m_RegEx["regexEndProcedure"]->Matches( src ) )
    {
        iKind = FormatIndentCodeTree::ctcEndProcedure;
        if (m_IndentProgFunSub)
            deltaIndentCur = -1;
        else
            deltaIndentCur = 0;
        deltaIndentNext = 0;
    }
}

// Special code to compare strings which doesn't care
// about spaces leading up to the EOL.
bool IndentEstimator::BuffersDiffer( const wxString &a, const wxString &b )
{
    wxString acopy = a;
    acopy.Replace("\r\n", "\n");
    acopy.Replace("\r", "\n");
    wxStringTokenizer aTokenizer(acopy, "\n", wxTOKEN_RET_EMPTY_ALL);
    std::vector<wxString> aLines;
    while (aTokenizer.HasMoreTokens())
        aLines.push_back(aTokenizer.GetNextToken());

    wxString bcopy = b;
    bcopy.Replace("\r\n", "\n");
    bcopy.Replace("\r", "\n");
    wxStringTokenizer bTokenizer(bcopy, "\n", wxTOKEN_RET_EMPTY_ALL);
    std::vector<wxString> bLines;
    while (bTokenizer.HasMoreTokens())
        bLines.push_back(bTokenizer.GetNextToken());

    size_t aSize = aLines.size();
    if(aSize != bLines.size())
        return true;

    for (size_t i=0; i<aSize; i++)
    {
        wxString aLin = aLines[i];
        aLin.Trim();
        wxString bLin = bLines[i];
        bLin.Trim();
        if (aLin != bLin)
            return true;
    }
    return false;
}

bool IndentEstimator::GetIsHasLineContinuation( const wxString & srcLine )
{
    wxASSERT( m_RegEx["regexMultiLines"] );
    return m_RegEx["regexMultiLines"]->Matches( srcLine );
}


void IndentEstimator::DelLineContinuation( wxString & srcLine )
{
    wxASSERT( m_RegEx["regexMultiLines"] );
    m_RegEx["regexMultiLines"]->ReplaceAll( &srcLine, "" );
}


void IndentEstimator::DelComment( wxString & srcLine )
{
    wxASSERT( m_RegEx["regexComment"] );
    m_RegEx["regexComment"]->ReplaceAll( &srcLine, "" );
}

bool IndentEstimator::GetIsHasPreprocessor(const wxString & srcLine)
{
    bool bRet = false;
    wxASSERT( m_RegEx["regexPreprocessorC"] );
    bRet = m_RegEx["regexPreprocessorC"]->Matches( srcLine );

    return bRet;
}

void IndentEstimator::PrepareLine(const wxString& srcIn, wxArrayString& srcLines)
{
    wxString src = srcIn;
    CutStringAndComment(src);

    wxStringTokenizer tokenizer(src, ";", wxTOKEN_STRTOK);
    while (tokenizer.HasMoreTokens())
    {
        wxString token = tokenizer.GetNextToken();
        srcLines.Add(token);
    }

}

void IndentEstimator::CutStringAndComment(wxString& src)
{
    src.Trim(false);

    while(true)
    {
        int i1 = src.Find('\'');
        int j1 = src.Find('"');
        char curChar;
        if (i1 != wxNOT_FOUND && j1 != wxNOT_FOUND)
            curChar = i1<j1 ? '\'' : '"';
        else if (i1 != wxNOT_FOUND)
            curChar = '\'';
        else if (j1 != wxNOT_FOUND)
            curChar = '"';
        else
            break;

        i1 = src.Find(curChar);
        if (i1 != wxNOT_FOUND)
        {
            wxString str2 = src.Mid(i1+1);
            int i2 = str2.Find(curChar);
            if (i2 != wxNOT_FOUND)
                src = src.Mid(0,i1) + "$_$" + str2.Mid(i2+1);
            else
                src = src.Mid(0,i1);
        }
        else
            break;
    }

    src.Replace("$_$", "\" \"");
    src = src.BeforeFirst('!').Trim();
    if (src.IsEmpty())
        return;

    //Replace inside parentheses
    int istart = -1;
    int level = 0;
    for (size_t i=0; i<src.size(); i++)
    {
        if (src[i] == '(')
        {
            if (level == 0)
                istart = i;
            level++;
        }
        else if (src[i] == ')')
        {
            level--;
            if (level == 0)
            {
                for (size_t j=istart+1; j<i; j++)
                    src.SetChar(j, 'a');
            }
        }
    }
}

void IndentEstimator::ReadConfig()
{
    ConfigManager* cfg = Manager::Get()->GetConfigManager("fortran_project");

    m_IndentProgFunSub = cfg->ReadBool("/indent_prog_fun_sub_after", true);
    m_IndentModule = cfg->ReadBool("/indent_module_after", true);
    m_IndentContainsModule = cfg->ReadBool("/indent_contains_module", true);
    m_IndentContainsModuleAfter = cfg->ReadBool("/indent_contains_module_after", true);
    m_IndentContainsProcedure = cfg->ReadBool("/indent_contains_procedure", true);
    m_IndentContainsProcedureAfter = cfg->ReadBool("/indent_contains_procedure_after", true);
    m_IndentContainsTypedef = cfg->ReadBool("/indent_contains_typedef", true);
    m_IndentContainsTypedefAfter = cfg->ReadBool("/indent_contains_typedef_after", true);
    m_IndentSelectCaseAfter = cfg->ReadBool("/indent_selectcase_after", true);
    m_IndentSelectTypeAfter = cfg->ReadBool("/indent_selecttype_after", true);
}


