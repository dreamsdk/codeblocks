/*
 * This file is part of the FortranProject plugin for Code::Blocks IDE
 * and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 */

#include "tokenf.h"

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/intl.h>
#endif

wxCriticalSection s_CritSect;

TokenF::TokenF()
	: m_LineStart(0),
	m_LineEnd(0),
	m_TokenAccess(taPublic),
	m_Pass(true),
	m_IsAbstract(false),
	m_WasIncluded(false),
	m_pParent(0L)
{
}

TokenF::TokenF(const wxString& name, const wxString& filename, unsigned int line)
	: m_Name(name),
	m_Filename(filename),
	m_LineStart(line),
	m_TokenAccess(taPublic),
	m_Pass(true),
	m_IsAbstract(false),
	m_WasIncluded(false),
	m_pParent(0L)
{
	//ctor
}

TokenF::~TokenF()
{
	//dtor
}

void TokenF::Clear()
{
    size_t nChildren = m_Children.GetCount();
    for(size_t i=0; i<nChildren; ++i)
    {
        m_Children.Item(i)->Clear();
        delete m_Children.Item(i);
    }
    m_Children.Clear();
}

void TokenF::AddChild(TokenF* child)
{
	if (child)
		m_Children.Add(child);
}

wxString TokenF::GetTokenKindString()
{
	switch (m_TokenKind)
	{
	    case tkUse: return _("use");
        case tkModule: return _("module");
        case tkSubroutine: return _("subroutine");
        case tkFunction: return _("function");
        case tkProgram: return _("program");
        case tkType: return _("type");
        case tkInclude: return _("include");
        case tkBlockData: return _("blockdata");
        case tkCommonblock: return _("commonblock");
        case tkPreprocessor: return _("preprocessor");
        case tkFile: return _("file");
        case tkVariable: return _("variable");
        case tkOther: return _("other");
        //case tkInterfaceFunction: return _("function");
        //case tkInterfaceSubroutine: return _("subroutine");
        case tkInterface: return _("interface");
        case tkInterfaceExplicit: return _("explicit interface");
        case tkProcedure: return _("procedure");
        case tkAccessList: return _("access list");
        case tkBlockConstruct: return _("block construct");
        case tkAssociateConstruct: return _("associate construct");
        case tkSubmodule: return _("submodule");
        case tkSelectTypeChild: return _("select type");
        case tkSelectTypeDefault: return _("select type");
        case tkProcedureFinal: return _("final procedure");
        case tkBindTo: return _("BindTo");
        case tkCallSubroutine: return _("subroutine call");
        case tkCallFunction: return _("function call");
        case tkMacroDefine: return _("macro define");
	}
	return _("other");
}

void TokenF::AddLineEnd(int end)
{
    m_LineEnd = end;
}

//--------------------------------------------------------------------

TokensArrayClass::TokensArrayClass()
{
}

TokensArrayClass::~TokensArrayClass()
{
    for(size_t i=0; i<m_Tokens.GetCount(); i++)
    {
        m_Tokens.Item(i)->Clear();
        delete m_Tokens.Item(i);
    }
    m_Tokens.Clear();
}

//--------------------------------------------------------------------
TokenFlat::TokenFlat():
    TokenF()
{
    //ctor
    m_HostAssociated = false;
}



TokenFlat::TokenFlat(const TokenF* tok)
{
    m_Name = tok->m_Name;
    m_DisplayName = tok->m_DisplayName;
    m_Args = tok->m_Args;
    m_Filename = tok->m_Filename;
    m_TypeDefinition = tok->m_TypeDefinition;
    m_LineStart = tok->m_LineStart;
    m_LineEnd = tok->m_LineEnd;
    m_TokenKind = tok->m_TokenKind;
    m_DefinitionLength = tok->m_DefinitionLength;
    m_TokenAccess = tok->m_TokenAccess;

    if (tok->m_pParent)
    {
        m_ParentName = tok->m_pParent->m_Name;
        m_ParentDisplayName = tok->m_pParent->m_DisplayName;
        m_ParentTokenKind = tok->m_pParent->m_TokenKind;
    }

    m_PartFirst = tok->m_PartFirst;
    if (m_TokenKind == tkFunction)
    {
        m_ResultVariable = tok->m_ResultVariable;
    }
    else if (m_TokenKind == tkProcedure || m_TokenKind == tkType)
    {
        m_Pass = tok->m_Pass;
        m_IsAbstract = tok->m_IsAbstract;
        m_ExtendsType = tok->m_ExtendsType;
    }
    m_PartLast = tok->m_PartLast;
    m_DocString = tok->m_DocString;

    m_HostAssociated = false;

    m_WasIncluded = tok->m_WasIncluded;
    if (m_WasIncluded)
    {
        m_IncludeFilename = tok->m_IncludeFilename;
        m_IncludeLineStart = tok->m_IncludeLineStart;
        m_IncludeLineEnd = tok->m_IncludeLineEnd;
    }
}

TokenFlat::TokenFlat(const TokenFlat* tok)
{
    m_Name = tok->m_Name;
    m_DisplayName = tok->m_DisplayName;
    m_Args = tok->m_Args;
    m_Filename = tok->m_Filename;
    m_TypeDefinition = tok->m_TypeDefinition;
    m_LineStart = tok->m_LineStart;
    m_LineEnd = tok->m_LineEnd;
    m_TokenKind = tok->m_TokenKind;
    m_DefinitionLength = tok->m_DefinitionLength;
    m_TokenAccess = tok->m_TokenAccess;

    m_ParentName = tok->m_ParentName;
    m_ParentDisplayName = tok->m_ParentDisplayName;
    m_ParentTokenKind = tok->m_ParentTokenKind;

    m_PartFirst = tok->m_PartFirst;
    if (m_TokenKind == tkFunction)
    {
        m_ResultVariable = tok->m_ResultVariable;
    }
    else if (m_TokenKind == tkProcedure || m_TokenKind == tkType)
    {
        m_PartLast = tok->m_PartLast;
        m_Pass = tok->m_Pass;
        m_IsAbstract = tok->m_IsAbstract;
        m_ExtendsType = tok->m_ExtendsType;
    }
    m_PartLast = tok->m_PartLast;
    m_DocString = tok->m_DocString;
    m_Rename = tok->m_Rename;
    m_HostAssociated = tok->m_HostAssociated;

    m_WasIncluded = tok->m_WasIncluded;
    if (m_WasIncluded)
    {
        m_IncludeFilename = tok->m_IncludeFilename;
        m_IncludeLineStart = tok->m_IncludeLineStart;
    }
}

TokenFlat::~TokenFlat()
{
	//dtor
}

void TokenFlat::Rename(const wxString& newName)
{
    m_Name = newName.Lower();
    m_DisplayName = newName;
}

void TokenFlat::ChangeDisplayName(const wxString& newName)
{
    m_DisplayName = newName;
}


//--------------------------------------------------------------------

TokensArrayFlatClass::TokensArrayFlatClass()
{
}

TokensArrayFlatClass::~TokensArrayFlatClass()
{
    this->Clear();
}

void TokensArrayFlatClass::Clear()
{
    for(size_t i=0; i<m_Tokens.GetCount(); i++)
    {
        m_Tokens.Item(i)->Clear();
        delete m_Tokens.Item(i);
    }
    m_Tokens.Clear();
}

bool TokensArrayFlatClass::HasTokensWithName(const wxString& name, ArrOfSizeT& idx)
{
    bool found = false;
    for(size_t i=0; i<m_Tokens.size(); i++)
    {
        if (m_Tokens.Item(i)->m_Name.IsSameAs(name))
        {
            if (!found)
                found = true;
            idx.Add(i);
        }
    }
    return found;
}

void TokensArrayFlatClass::DelTokensWithName(const wxString& name)
{
    size_t toksiz = m_Tokens.size();
    for(size_t i=0; i<toksiz; i++)
    {
        if (m_Tokens.Item(i)->m_Name.IsSameAs(name))
        {
            m_Tokens.Item(i)->Clear();
            delete m_Tokens.Item(i);
            m_Tokens.RemoveAt(i);
            toksiz--;
            i--;
        }
    }
}

//--------------------------------------------------------------------
