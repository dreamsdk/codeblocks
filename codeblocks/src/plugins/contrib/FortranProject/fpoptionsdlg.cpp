/*
 * This file is part of the Code::Blocks IDE and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 */
#include "fpoptionsdlg.h"

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <wx/intl.h>
    #include <wx/listbox.h>
    #include <wx/spinctrl.h>
    #include <wx/checkbox.h>
    #include <wx/combobox.h>
    #include <wx/radiobox.h>
    #include <wx/radiobut.h>
    #include <wx/treectrl.h>
    #include <wx/slider.h>
    #include <wx/button.h>
    #include <wx/stattext.h>
    #include <wx/regex.h>
    #include <wx/colordlg.h>
    #include <wx/choice.h>
    #include <wx/window.h>

    #include <configmanager.h>
    #include <manager.h>
    #include <globals.h>
#endif
#include <algorithm>
#include <vector>

#include <wx/clrpicker.h>
#include <wx/xrc/xmlres.h>

#include "fortranproject.h"

BEGIN_EVENT_TABLE(FPOptionsDlg, wxPanel)
    EVT_UPDATE_UI(-1, FPOptionsDlg::OnUpdateUI)
    EVT_LISTBOX(XRCID("lbAIStatements"), FPOptionsDlg::OnAISelectionChanged)
END_EVENT_TABLE()

FPOptionsDlg::FPOptionsDlg(wxWindow* parent, NativeParserF* np, FortranProject* fp)
    : m_pNativeParser(np),
    m_pFortranProject(fp)
{
    ConfigManager* cfg = Manager::Get()->GetConfigManager("fortran_project");

    wxXmlResource::Get()->LoadPanel(this, parent, "dlgFPSettings");

    XRCCTRL(*this, "chkEnableCC", wxCheckBox)->SetValue(cfg->ReadBool("/use_code_completion", true));
    XRCCTRL(*this, "chkReplaceAlwaysCodeCompletion", wxCheckBox)->SetValue(cfg->ReadBool("/use_replace_always_code_completion", false));
    XRCCTRL(*this, "chkSmartCodeCompletion", wxCheckBox)->SetValue(cfg->ReadBool("/use_smart_code_completion", true));
    XRCCTRL(*this, "chkOnlyUseAssociated", wxCheckBox)->SetValue(cfg->ReadBool("/only_use_associated", true));
    XRCCTRL(*this, "chkShowHiddenEntities", wxCheckBox)->SetValue(cfg->ReadBool("/show_hidden_entities", false));
    XRCCTRL(*this, "chkShowTypeVariables", wxCheckBox)->SetValue(cfg->ReadBool("/show_type_variables", true));
    XRCCTRL(*this, "spnMaxMatches", wxSpinCtrl)->SetValue(cfg->ReadInt("/max_matches", 16384));

    XRCCTRL(*this, "chkEnableSB", wxCheckBox)->SetValue(cfg->ReadBool("/use_symbols_browser", true));
    XRCCTRL(*this, "chkFloatSB", wxCheckBox)->SetValue(cfg->ReadBool("/as_floating_window", false));
    XRCCTRL(*this, "chkBottomTree", wxCheckBox)->SetValue(cfg->ReadBool("/visible_bottom_tree", true));
    XRCCTRL(*this, "chkSortSB", wxCheckBox)->SetValue(cfg->ReadBool("/browser_sort_alphabetically", true));
    XRCCTRL(*this, "chkLocVarSB", wxCheckBox)->SetValue(cfg->ReadBool("/browser_show_local_variables", true));
    XRCCTRL(*this, "chkInclSepar", wxCheckBox)->SetValue(cfg->ReadBool("/browser_show_include_files_separately", true));

    XRCCTRL(*this, "chkKL_1", wxCheckBox)->SetValue(cfg->ReadBool("/lexer_keywords_set1", true));
    XRCCTRL(*this, "chkKL_2", wxCheckBox)->SetValue(cfg->ReadBool("/lexer_keywords_set2", true));
    XRCCTRL(*this, "chkKL_3", wxCheckBox)->SetValue(cfg->ReadBool("/lexer_keywords_set3", false));
    XRCCTRL(*this, "chkKL_4", wxCheckBox)->SetValue(cfg->ReadBool("/lexer_keywords_set4", false));

    XRCCTRL(*this, "rbCase", wxRadioBox)->SetSelection(cfg->ReadInt("/keywords_case", 0));

    XRCCTRL(*this, "chkCallTipsArrays", wxCheckBox)->SetValue(cfg->ReadBool("/call_tip_arrays", true));

    wxCheckBox* chkCH = XRCCTRL(*this, "chkConstrHighlighterEnable", wxCheckBox);
    chkCH->SetValue(cfg->ReadBool("/do_construct_highlighting", true));
    wxColourPickerCtrl* cpCHFull = XRCCTRL(*this, "cpCHFullColour", wxColourPickerCtrl);
    cpCHFull->SetColour(cfg->ReadColour("/chighlighter_full_colour",wxColour(165, 165, 255)));
    wxColourPickerCtrl* cpCHUnfin = XRCCTRL(*this, "cpCHUnfinColour", wxColourPickerCtrl);
    cpCHUnfin->SetColour(cfg->ReadColour("/chighlighter_unfinished_colour",wxColour(255, 165, 0)));
    if (!chkCH->GetValue())
    {
        XRCCTRL(*this, "stCHFullColour", wxStaticText)->Enable(false);
        XRCCTRL(*this, "stCHUnfinColour", wxStaticText)->Enable(false);
        cpCHFull->Enable(false);
        cpCHUnfin->Enable(false);
    }

    XRCCTRL(*this, "spnCallTreeDepthLimit", wxSpinCtrl)->SetValue(cfg->ReadInt("/calltree_depthmax", 5));

    bool interpretCPP = cfg->ReadBool("/interpret_cpreproc", true);
    XRCCTRL(*this, "chkInterpretCPP", wxCheckBox)->SetValue(interpretCPP);
    XRCCTRL(*this, "chkShadowInactiveCPP", wxCheckBox)->SetValue(cfg->ReadBool("/make_cpp_shadow", true));
    wxColourPickerCtrl* cpCPPShadowColour = XRCCTRL(*this, "cpCPPShadowColour", wxColourPickerCtrl);
    cpCPPShadowColour->SetColour(cfg->ReadColour("/cpp_shadow_colour",wxColour(240, 240, 240)));
    XRCCTRL(*this, "spnCPPShadowOpacity", wxSpinCtrl)->SetValue(cfg->ReadInt("/cpp_shadow_opacity", 45));
    if (!interpretCPP)
    {
        XRCCTRL(*this, "chkShadowInactiveCPP", wxCheckBox)->Enable(false);
        XRCCTRL(*this, "stCPPShadowColour", wxStaticText)->Enable(false);
        cpCPPShadowColour->Enable(false);
        XRCCTRL(*this, "stCPPShadowOpacity", wxStaticText)->Enable(false);
        XRCCTRL(*this, "spnCPPShadowOpacity", wxSpinCtrl)->Enable(false);
    }

    XRCCTRL(*this, "chkFortranInfo", wxCheckBox)->SetValue(cfg->ReadBool("/use_log_window", true));
    XRCCTRL(*this, "chkComAbove", wxCheckBox)->SetValue(cfg->ReadBool("/include_comments_above", true));
    XRCCTRL(*this, "chkComBelow", wxCheckBox)->SetValue(cfg->ReadBool("/include_comments_below", true));
    XRCCTRL(*this, "chkDeclarLog", wxCheckBox)->SetValue(cfg->ReadBool("/include_declarations_log", true));
    XRCCTRL(*this, "chkLogComRight", wxCheckBox)->SetValue(cfg->ReadBool("/include_log_comments_variable", true));

    int iShowDocW = cfg->ReadInt("/show_docs_window", 1);
    if (iShowDocW == 0)
        XRCCTRL(*this, "rbutDocWAlways", wxRadioButton)->SetValue(true);
    else if (iShowDocW == 1)
        XRCCTRL(*this, "rbutDocWOnly", wxRadioButton)->SetValue(true);
    else
        XRCCTRL(*this, "rbutDocWDont", wxRadioButton)->SetValue(true);

    XRCCTRL(*this, "cbAIEnable", wxCheckBox)->SetValue(cfg->ReadBool("/ainsert_enable", true));
    FillAutoInsert();
    XRCCTRL(*this, "cbACorrectIndentEnable", wxCheckBox)->SetValue(cfg->ReadBool("/acorrect_indent_enabled", true));
}

FPOptionsDlg::~FPOptionsDlg()
{
}

void FPOptionsDlg::FillAutoInsert()
{
    const std::map<wxString,wxString>* ainames = m_AInsert.GetNameMap();
    std::map<wxString,wxString>::const_iterator it;
    std::vector<wxString> vecnames;

    for (it = ainames->begin(); it != ainames->end(); ++it)
    {
        vecnames.push_back(it->second);
    }

    std::sort(vecnames.begin(), vecnames.end());

    for (size_t i=0; i<vecnames.size(); ++i)
    {
        XRCCTRL(*this, "lbAIStatements", wxListBox)->Append(vecnames[i]);
    }
    m_AISelIdx = 0;
    XRCCTRL(*this, "lbAIStatements", wxListBox)->SetSelection(m_AISelIdx);
    ShowCurrientAInsert(m_AISelIdx);
}

void FPOptionsDlg::ShowCurrientAInsert(int idx)
{
    m_AISelIdx = -1;

    wxString sel = XRCCTRL(*this, "lbAIStatements", wxListBox)->GetString(idx);
    wxArrayString aitstr;
    wxArrayString alignstr;
    bool isen;
    if (!m_AInsert.GetItemChoices(sel, aitstr, alignstr, isen))
        return;

    XRCCTRL(*this, "cbAIInsert", wxChoice)->Clear();
    for (size_t i=0; i<aitstr.Count(); i++)
    {
        XRCCTRL(*this, "cbAIInsert", wxChoice)->Append(aitstr[i]);
    }

    XRCCTRL(*this, "cbAIAlign", wxChoice)->Clear();
    if (alignstr.Count() > 0)
    {
        XRCCTRL(*this, "cbAIAlign", wxChoice)->Enable(true);
        m_cbAIAlign_wasEnabled = true;
        for (size_t i=0; i<alignstr.Count(); i++)
        {
            XRCCTRL(*this, "cbAIAlign", wxChoice)->Append(alignstr[i]);
        }
    }
    else
    {
        XRCCTRL(*this, "cbAIAlign", wxChoice)->Enable(false);
        m_cbAIAlign_wasEnabled = false;
    }

    XRCCTRL(*this, "cbAIAddName", wxCheckBox)->Enable(isen);
    m_cbAIAddName_wasEnabled = isen;

    int aiType;
    bool doAddName;
    bool alignToStatement;
    if (!m_AInsert.GetItemValues(sel, aiType, doAddName, alignToStatement))
        return;

    XRCCTRL(*this, "cbAIInsert", wxChoice)->SetSelection(aiType);
    int ats = alignToStatement? 0: 1;
    XRCCTRL(*this, "cbAIAlign", wxChoice)->SetSelection(ats);
    XRCCTRL(*this, "cbAIAddName", wxCheckBox)->SetValue(doAddName);
    m_AISelIdx = idx;
}

void FPOptionsDlg::OnAISelectionChanged(cb_unused wxCommandEvent& event)
{
    int idx = XRCCTRL(*this, "lbAIStatements", wxListBox)->GetSelection();
    if (idx != wxNOT_FOUND)
    {
        ReadAIChoice();
        ShowCurrientAInsert(idx);
    }
}

void FPOptionsDlg::ReadAIChoice()
{
    wxString sel = XRCCTRL(*this, "lbAIStatements", wxListBox)->GetString(m_AISelIdx);

    int aiType = XRCCTRL(*this, "cbAIInsert", wxChoice)->GetSelection();
    int iAlign = XRCCTRL(*this, "cbAIAlign", wxChoice)->GetSelection();
    bool alignToStatement = iAlign==0 ? true : false;
    bool doAddName = XRCCTRL(*this, "cbAIAddName", wxCheckBox)->GetValue();

    m_AInsert.EditRule(sel, aiType, doAddName, alignToStatement);
}

void FPOptionsDlg::OnUpdateUI(cb_unused wxUpdateUIEvent& event)
{
    bool en = XRCCTRL(*this, "chkEnableCC", wxCheckBox)->GetValue();

    XRCCTRL(*this, "chkReplaceAlwaysCodeCompletion", wxCheckBox)->Enable(en);
    XRCCTRL(*this, "chkSmartCodeCompletion", wxCheckBox)->Enable(en);
    XRCCTRL(*this, "chkOnlyUseAssociated", wxCheckBox)->Enable(en);
    XRCCTRL(*this, "chkShowHiddenEntities", wxCheckBox)->Enable(en);
    XRCCTRL(*this, "chkShowTypeVariables", wxCheckBox)->Enable(en);
    XRCCTRL(*this, "spnMaxMatches", wxSpinCtrl)->Enable(en);
    XRCCTRL(*this, "rbCase", wxRadioBox)->Enable(en);
    XRCCTRL(*this, "chkKL_1", wxCheckBox)->Enable(en);
    XRCCTRL(*this, "chkKL_2", wxCheckBox)->Enable(en);
    XRCCTRL(*this, "chkKL_3", wxCheckBox)->Enable(en);
    XRCCTRL(*this, "chkKL_4", wxCheckBox)->Enable(en);

    en = XRCCTRL(*this, "chkEnableSB", wxCheckBox)->GetValue();
    XRCCTRL(*this, "chkFloatSB", wxCheckBox)->Enable(en);
    XRCCTRL(*this, "chkBottomTree", wxCheckBox)->Enable(en);
    XRCCTRL(*this, "chkSortSB", wxCheckBox)->Enable(en);
    XRCCTRL(*this, "chkLocVarSB", wxCheckBox)->Enable(en);
    XRCCTRL(*this, "chkInclSepar", wxCheckBox)->Enable(en);

    bool chkCH_en = XRCCTRL(*this, "chkConstrHighlighterEnable", wxCheckBox)->GetValue();
    XRCCTRL(*this, "cpCHFullColour", wxColourPickerCtrl)->Enable(chkCH_en);
    XRCCTRL(*this, "cpCHUnfinColour", wxColourPickerCtrl)->Enable(chkCH_en);
    XRCCTRL(*this, "stCHFullColour", wxStaticText)->Enable(chkCH_en);
    XRCCTRL(*this, "stCHUnfinColour", wxStaticText)->Enable(chkCH_en);

    bool interpretCPP = XRCCTRL(*this, "chkInterpretCPP", wxCheckBox)->GetValue();
    XRCCTRL(*this, "chkShadowInactiveCPP", wxCheckBox)->Enable(interpretCPP);
    XRCCTRL(*this, "stCPPShadowColour", wxStaticText)->Enable(interpretCPP);
    XRCCTRL(*this, "cpCPPShadowColour", wxColourPickerCtrl)->Enable(interpretCPP);
    XRCCTRL(*this, "stCPPShadowOpacity", wxStaticText)->Enable(interpretCPP);
    XRCCTRL(*this, "spnCPPShadowOpacity", wxSpinCtrl)->Enable(interpretCPP);

    en = XRCCTRL(*this, "chkFortranInfo", wxCheckBox)->GetValue();
    XRCCTRL(*this, "chkComAbove", wxCheckBox)->Enable(en);
    XRCCTRL(*this, "chkComBelow", wxCheckBox)->Enable(en);
    XRCCTRL(*this, "chkDeclarLog", wxCheckBox)->Enable(en);
    XRCCTRL(*this, "chkLogComRight", wxCheckBox)->Enable(en);

    en = XRCCTRL(*this, "cbAIEnable", wxCheckBox)->GetValue();
    XRCCTRL(*this, "lbAIStatements", wxListBox)->Enable(en);
    XRCCTRL(*this, "stAIText1", wxStaticText)->Enable(en);
    XRCCTRL(*this, "stAIText2", wxStaticText)->Enable(en);
    XRCCTRL(*this, "cbAIInsert", wxChoice)->Enable(en);

    if (en && m_cbAIAlign_wasEnabled)
        XRCCTRL(*this, "cbAIAlign", wxChoice)->Enable(en);
    else if (!en)
        XRCCTRL(*this, "cbAIAlign", wxChoice)->Enable(en);

    if (en && m_cbAIAddName_wasEnabled)
        XRCCTRL(*this, "cbAIAddName", wxCheckBox)->Enable(en);
    else if (!en)
        XRCCTRL(*this, "cbAIAddName", wxCheckBox)->Enable(en);
}

void FPOptionsDlg::OnApply()
{
    ConfigManager* cfg = Manager::Get()->GetConfigManager("fortran_project");

    // force parser to read its options that we write in the config
    cfg->Write("/use_code_completion", (bool)XRCCTRL(*this, "chkEnableCC", wxCheckBox)->GetValue());

    // set all other member options
    cfg->Write("/use_replace_always_code_completion", (bool)XRCCTRL(*this, "chkReplaceAlwaysCodeCompletion", wxCheckBox)->GetValue());
    cfg->Write("/use_smart_code_completion", (bool)XRCCTRL(*this, "chkSmartCodeCompletion", wxCheckBox)->GetValue());
    cfg->Write("/only_use_associated", (bool)XRCCTRL(*this, "chkOnlyUseAssociated", wxCheckBox)->GetValue());
    cfg->Write("/show_hidden_entities", (bool)XRCCTRL(*this, "chkShowHiddenEntities", wxCheckBox)->GetValue());
    cfg->Write("/show_type_variables", (bool)XRCCTRL(*this, "chkShowTypeVariables", wxCheckBox)->GetValue());
    cfg->Write("/max_matches", (int)XRCCTRL(*this, "spnMaxMatches", wxSpinCtrl)->GetValue());

    cfg->Write("/use_symbols_browser", (bool)XRCCTRL(*this, "chkEnableSB", wxCheckBox)->GetValue());
    cfg->Write("/as_floating_window", (bool)XRCCTRL(*this, "chkFloatSB", wxCheckBox)->GetValue());
    cfg->Write("/visible_bottom_tree", (bool)XRCCTRL(*this, "chkBottomTree", wxCheckBox)->GetValue());
    cfg->Write("/browser_sort_alphabetically", (bool)XRCCTRL(*this, "chkSortSB", wxCheckBox)->GetValue());
    cfg->Write("/browser_show_local_variables", (bool)XRCCTRL(*this, "chkLocVarSB", wxCheckBox)->GetValue());
    cfg->Write("/browser_show_include_files_separately", (bool)XRCCTRL(*this, "chkInclSepar", wxCheckBox)->GetValue());

    cfg->Write("/lexer_keywords_set1", (bool)XRCCTRL(*this, "chkKL_1", wxCheckBox)->GetValue());
    cfg->Write("/lexer_keywords_set2", (bool)XRCCTRL(*this, "chkKL_2", wxCheckBox)->GetValue());
    cfg->Write("/lexer_keywords_set3", (bool)XRCCTRL(*this, "chkKL_3", wxCheckBox)->GetValue());
    cfg->Write("/lexer_keywords_set4", (bool)XRCCTRL(*this, "chkKL_4", wxCheckBox)->GetValue());

    cfg->Write("/keywords_case", (int)XRCCTRL(*this, "rbCase", wxRadioBox)->GetSelection());

    cfg->Write("/call_tip_arrays", (bool)XRCCTRL(*this, "chkCallTipsArrays", wxCheckBox)->GetValue());

    cfg->Write("/chighlighter_full_colour", XRCCTRL(*this, "cpCHFullColour", wxColourPickerCtrl)->GetColour());
    cfg->Write("/chighlighter_unfinished_colour", XRCCTRL(*this, "cpCHUnfinColour", wxColourPickerCtrl)->GetColour());

    cfg->Write("/calltree_depthmax", (int)XRCCTRL(*this, "spnCallTreeDepthLimit", wxSpinCtrl)->GetValue());

    cfg->Write("/interpret_cpreproc", (bool)XRCCTRL(*this, "chkInterpretCPP", wxCheckBox)->GetValue());
    cfg->Write("/make_cpp_shadow", (bool)XRCCTRL(*this, "chkShadowInactiveCPP", wxCheckBox)->GetValue());
    cfg->Write("/cpp_shadow_colour", XRCCTRL(*this, "cpCPPShadowColour", wxColourPickerCtrl)->GetColour());
    cfg->Write("/cpp_shadow_opacity", (int)XRCCTRL(*this, "spnCPPShadowOpacity", wxSpinCtrl)->GetValue());

    cfg->Write("/use_log_window", (bool)XRCCTRL(*this, "chkFortranInfo", wxCheckBox)->GetValue());
    cfg->Write("/include_comments_above", (bool)XRCCTRL(*this, "chkComAbove", wxCheckBox)->GetValue());
    cfg->Write("/include_comments_below", (bool)XRCCTRL(*this, "chkComBelow", wxCheckBox)->GetValue());
    cfg->Write("/include_declarations_log", (bool)XRCCTRL(*this, "chkDeclarLog", wxCheckBox)->GetValue());
    cfg->Write("/include_log_comments_variable", (bool)XRCCTRL(*this, "chkLogComRight", wxCheckBox)->GetValue());

    int iShowDocW;
    if (XRCCTRL(*this, "rbutDocWAlways", wxRadioButton)->GetValue())
        iShowDocW = 0;
    else if (XRCCTRL(*this, "rbutDocWOnly", wxRadioButton)->GetValue())
        iShowDocW = 1;
    else
        iShowDocW = 2;

    cfg->Write("/show_docs_window", iShowDocW);

    cfg->Write("/ainsert_enable", (bool)XRCCTRL(*this, "cbAIEnable", wxCheckBox)->GetValue());
    ReadAIChoice();
    m_AInsert.WriteAIOptions();
    cfg->Write("/acorrect_indent_enabled", (bool)XRCCTRL(*this, "cbACorrectIndentEnable", wxCheckBox)->GetValue());

    cfg->Write("/do_construct_highlighting", (bool)XRCCTRL(*this, "chkConstrHighlighterEnable", wxCheckBox)->GetValue());

    m_pNativeParser->RereadOptions();
    m_pFortranProject->RereadOptions();
}
