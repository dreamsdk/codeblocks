
#include "docblock.h"

#include <sdk.h>
#ifndef CB_PRECOMP
#endif
#include <iostream>

#include <wx/html/htmlwin.h>

#include "cbcolourmanager.h"

DocBlock::DocBlock():
    m_Description("**description**"),
    m_Brief("**brief**")
{
    //ctor
}

DocBlock::~DocBlock()
{
    //dtor
}

wxString DocBlock::GetDescription()
{
    if (m_DocMap.count(m_Description) == 1)
        return m_DocMap[m_Description];

    return wxEmptyString;
}

bool DocBlock::HasDescription()
{
    return (m_DocMap.count(m_Description) == 1);
}

void DocBlock::AddDescription(const wxString &descr)
{
    m_DocMap[m_Description] = descr;
}

wxString DocBlock::GetBrief()
{
    if (m_DocMap.count(m_Brief) == 1)
        return m_DocMap[m_Brief];

    return wxEmptyString;
}

bool DocBlock::HasBrief()
{
    return (m_DocMap.count(m_Brief) == 1);
}

int DocBlock::GetParamCount()
{
    int pc = 0;
    for (std::map<wxString,wxString>::iterator it = m_DocMap.begin(); it != m_DocMap.end(); ++it)
    {
        if (!it->first.IsSameAs(m_Description) && !it->first.IsSameAs(m_Brief))
            pc++;
    }
    return pc;
}

wxString DocBlock::GetValue(wxString& key)
{
    if (m_DocMap.count(key) == 1)
        return m_DocMap[key];

    return wxEmptyString;
}

void DocBlock::AddBrief(const wxString &bline)
{
    m_DocMap["**brief**"] = bline;
}

void DocBlock::AddParam(const wxString &name, const wxString &descr)
{
    m_DocMap[name.Lower()] = descr;
}

void DocBlock::Clear()
{
    m_DocMap.clear();
}

//*****************************************
namespace HTMLTags
{
    static const wxString br = "<br>";
    static const wxString sep = " ";
    static const wxString b1 = "<b>";
    static const wxString b0 = "</b>";

    static const wxString a1 = "<a>";
    static const wxString a0 = "</a>";

    static const wxString i1 = "<i>";
    static const wxString i0 = "</i>";

    static const wxString pre1 = "<pre>";
    static const wxString pre0 = "</pre>";

    static const wxString nbsp("&nbsp;");
    static const wxString tab = nbsp + nbsp + nbsp;

    static const wxString commandTag = "cmd=";
}

wxString HtmlDoc::GenerateHtmlDoc(TokenFlat* token, int token_idx, bool& hasDoc)
{
    //http://docs.wxwidgets.org/2.8/wx_wxhtml.html#htmltagssupported
    using namespace HTMLTags;

    ColourManager *colours = Manager::Get()->GetColourManager();

    wxString html = "<html><body bgcolor=\"";
    html += colours->GetColour("cc_docs_back").GetAsString(wxC2S_HTML_SYNTAX) + "\" text=\"";
    html += colours->GetColour("cc_docs_fore").GetAsString(wxC2S_HTML_SYNTAX) + "\" link=\"";
    html += colours->GetColour("cc_docs_link").GetAsString(wxC2S_HTML_SYNTAX) + "\">";

    html += "<a name=\"top\"></a>";

    hasDoc = false;
    if (!token || token->m_DisplayName.IsEmpty())
        return wxEmptyString;

    // add parent:
    if (!token->m_ParentDisplayName.IsEmpty())
    {
        wxString parent;
        if (token->m_ParentTokenKind == tkModule)
            html += "module: " + b1 + token->m_ParentDisplayName + b0 + br;
    }

    html += br;
    wxString moreInfo;

    //add scope and name:
    switch (token->m_TokenKind)
    {
    case tkFunction:
        html += token->m_PartFirst + " function " + b1 + token->m_DisplayName + b0;
        html += " " + token->m_Args.Trim(false);
        html += sep + token->m_PartLast;
        html += br;
        break;

    case tkSubroutine:
        html += "subroutine " + b1 + token->m_DisplayName + b0;
        html += sep + token->m_Args;
        html += br;
        if (token->m_ParentTokenKind == tkFile)
            moreInfo = "global";
        else if (token->m_TokenAccess == taPrivate)
            moreInfo = "private";
        break;

    case tkVariable:
        html += token->m_TypeDefinition + " :: " + b1 + token->m_DisplayName + b0 + token->m_Args + br;
        moreInfo = token->GetTokenKindString();
        break;

    case tkInterface:
        html += token->m_TypeDefinition + nbsp + b1 + token->m_DisplayName + b0 + br;
        if (token->m_TypeDefinition.IsEmpty())
            moreInfo = "interface";
        else
            moreInfo = "generic interface";
        break;

    default:
        html += b1 + token->m_DisplayName + b0 + br;
        moreInfo = token->GetTokenKindString();
    }

    //add kind:
    if (!moreInfo.IsEmpty())
        html += i1 + "<font color=\"green\" size=3>" + "(" +
                moreInfo + ")" + "</font>" + i0 + br;

    if (!token->m_DocString.IsEmpty())
    {
        const wxString brsep = "@brief_end@";
        size_t brf = token->m_DocString.find(brsep);
        size_t bre_idx = 11;
        if (brf != wxString::npos)
            bre_idx += brf;
        else
            bre_idx = 0;

        if (bre_idx > 11)
        {
            html += br + i1 + b1 + "Brief:" + b0 + i0 + br;
            html += tab + token->m_DocString.substr(0,brf) + br;
            hasDoc = true;
        }

        if (bre_idx < token->m_DocString.size())
        {
            html += br + i1 + b1 + "Description:" + b0 + i0 + br;
            html += tab + token->m_DocString.substr(bre_idx) + br;
            hasDoc = true;
        }
    }

    //add go to declaration
    html += br + br + "<a href=\"" + commandTag + "goto" + wxString::Format("%i", token_idx)
               + "\">" +  "Open declaration" + "</a>" + br + br;

    // Append 'close' link:
    html += "<a href=\"" + commandTag + "close"
               + "\">" +  "close" + "</a>",

    html += "</body></html>";

    return html;
}


wxString HtmlDoc::OnDocumentationLink(wxHtmlLinkEvent &event, bool &dismissPopup, bool &isGoto, long int &tokenIdx)
{
    using namespace HTMLTags;

    const wxString& href = event.GetLinkInfo().GetHref();
    wxString args;
    wxString tidx_str;

    dismissPopup = false;
    isGoto = false;

    if (!href.StartsWith(commandTag, &args))
        return wxEmptyString;

    if (args.StartsWith("goto", &tidx_str))
    {
        if(tidx_str.ToLong(&tokenIdx))
        {
            dismissPopup = true;
            isGoto = true;
        }
    }
    else if (args.StartsWith("close"))
        dismissPopup = true;

    return wxEmptyString;
}

wxString HtmlDoc::GetDocForTooltip(TokenFlat* token)
{
    return HtmlDoc::GetDocShort(token->m_DocString);
}

wxString HtmlDoc::GetDocShort(const wxString& tokDoc)
{
    wxString doc;
    if (!tokDoc.IsEmpty())
    {
        const wxString brsep = "@brief_end@";
        size_t brf = tokDoc.find(brsep);
        size_t bre_idx = 11;
        if (brf != wxString::npos)
            bre_idx += brf;
        else
            bre_idx = 0;

        if (bre_idx > 11)
        {
            doc = tokDoc.substr(0,brf);
        }
        else if (bre_idx < tokDoc.size())
        {
            doc = tokDoc.substr(bre_idx);
            if (doc.size() > 120) // limit length of doc
                doc = doc.substr(0,120) + "...";
        }
    }
    return doc;
}


