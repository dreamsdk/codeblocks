/*
 * This file is part of the FortranProject plugin for Code::Blocks IDE
 * and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 * Author: Darius Markauskas
 *
 */
#include "constrhighlighter.h"

#include <sdk.h>
#ifndef CB_PRECOMP
    #include <configmanager.h>
    #include <editormanager.h>
    #include <logmanager.h>
#endif
#include <algorithm>

#include <cbstyledtextctrl.h>

ConstrHighlighter::ConstrHighlighter():
    m_MakeHighlight(true),
    m_FullColour(100, 100, 255),
    m_UnfinColour(255, 165, 0),
    m_CurrentPosition(0),
    m_IndicHighlight(24),
    m_WasCleared(true)
{
    m_KeywordSet.insert("if");
    m_KeywordSet.insert("elseif");
    m_KeywordSet.insert("then");
    m_KeywordSet.insert("else");
    m_KeywordSet.insert("endif");
    m_KeywordSet.insert("do");
    m_KeywordSet.insert("enddo");
    m_KeywordSet.insert("while");
    m_KeywordSet.insert("concurrent");
    m_KeywordSet.insert("subroutine");
    m_KeywordSet.insert("endsubroutine");
    m_KeywordSet.insert("function");
    m_KeywordSet.insert("endfunction");
    m_KeywordSet.insert("abstract");
    m_KeywordSet.insert("interface");
    m_KeywordSet.insert("endinterface");
    m_KeywordSet.insert("associate");
    m_KeywordSet.insert("endassociate");
    m_KeywordSet.insert("block");
    m_KeywordSet.insert("endblock");
    m_KeywordSet.insert("blockdata");
    m_KeywordSet.insert("data");
    m_KeywordSet.insert("endblockdata");
    m_KeywordSet.insert("critical");
    m_KeywordSet.insert("endcritical");
    m_KeywordSet.insert("module");
    m_KeywordSet.insert("endmodule");
    m_KeywordSet.insert("program");
    m_KeywordSet.insert("endprogram");
    m_KeywordSet.insert("select");
    m_KeywordSet.insert("case");
    m_KeywordSet.insert("selectcase");
    m_KeywordSet.insert("default");
    m_KeywordSet.insert("rank");
    m_KeywordSet.insert("is");
    m_KeywordSet.insert("class");
    m_KeywordSet.insert("selecttype");
    m_KeywordSet.insert("endselect");
    m_KeywordSet.insert("type");
    m_KeywordSet.insert("endtype");
    m_KeywordSet.insert("where");
    m_KeywordSet.insert("elsewhere");
    m_KeywordSet.insert("endwhere");
    m_KeywordSet.insert("enum");
    m_KeywordSet.insert("endenum");
    m_KeywordSet.insert("forall");
    m_KeywordSet.insert("endforall");
    m_KeywordSet.insert("submodule");
    m_KeywordSet.insert("endsubmodule");
    m_KeywordSet.insert("end");
    m_KeywordSet.insert("change");
    m_KeywordSet.insert("team");
    m_KeywordSet.insert("endteam");
    m_KeywordSet.insert("procedure");
    m_KeywordSet.insert("endprocedure");

    MakeFConstructTypeMap();
    FConstruct::MakeFCLReMap();
    FConstruct::MakeFCLWordMap();
    FConstruct::MakeWordFCLidMap();
    m_TimeMax = 100;
}

ConstrHighlighter::~ConstrHighlighter()
{
    FConstruct::DelFCLReMap();
}

void ConstrHighlighter::ReadOptions()
{
    ConfigManager* cfg = Manager::Get()->GetConfigManager("fortran_project");
    m_MakeHighlight = cfg->ReadBool("/do_construct_highlighting", true);
    m_FullColour = cfg->ReadColour("/chighlighter_full_colour",wxColour(165, 165, 255));
    m_UnfinColour = cfg->ReadColour("/chighlighter_unfinished_colour",wxColour(255, 165, 0));
}

void ConstrHighlighter::MakeFConstructTypeMap()
{
    m_FConstructTypeMap["end"] = FConstruct::ctProgramGroup;

    m_FConstructTypeMap["subroutine"] = FConstruct::ctProgramGroup;
    m_FConstructTypeMap["endsubroutine"] = FConstruct::ctProgramGroup;

    m_FConstructTypeMap["function"] = FConstruct::ctProgramGroup;
    m_FConstructTypeMap["endfunction"] = FConstruct::ctProgramGroup;

    m_FConstructTypeMap["program"] = FConstruct::ctProgramGroup;
    m_FConstructTypeMap["endprogram"] = FConstruct::ctProgramGroup;

    m_FConstructTypeMap["module"] = FConstruct::ctProgramGroup;
    m_FConstructTypeMap["endmodule"] = FConstruct::ctProgramGroup;

    m_FConstructTypeMap["submodule"] = FConstruct::ctProgramGroup;
    m_FConstructTypeMap["endsubmodule"] = FConstruct::ctProgramGroup;

    m_FConstructTypeMap["moduleprocedure"] = FConstruct::ctProgramGroup;
    m_FConstructTypeMap["endprocedure"] = FConstruct::ctProgramGroup;

    m_FConstructTypeMap["blockdata"] = FConstruct::ctProgramGroup;
    m_FConstructTypeMap["endblockdata"] = FConstruct::ctProgramGroup;

    m_FConstructTypeMap["interface"] = FConstruct::ctInterface;
    m_FConstructTypeMap["abstractinterface"] = FConstruct::ctInterface;
    m_FConstructTypeMap["endinterface"] = FConstruct::ctInterface;

    m_FConstructTypeMap["ifthen"] = FConstruct::ctIf;
    m_FConstructTypeMap["elseifthen"] = FConstruct::ctIf;
    m_FConstructTypeMap["else"] = FConstruct::ctIf;
    m_FConstructTypeMap["endif"] = FConstruct::ctIf;

    m_FConstructTypeMap["do"] = FConstruct::ctDo;
    m_FConstructTypeMap["dowhile"] = FConstruct::ctDo;
    m_FConstructTypeMap["doconcurrent"] = FConstruct::ctDo;
    m_FConstructTypeMap["enddo"] = FConstruct::ctDo;

    m_FConstructTypeMap["associate"] = FConstruct::ctAssiciate;
    m_FConstructTypeMap["endassociate"] = FConstruct::ctAssiciate;

    m_FConstructTypeMap["block"] = FConstruct::ctBlock;
    m_FConstructTypeMap["endblock"] = FConstruct::ctBlock;

    m_FConstructTypeMap["selectcase"] = FConstruct::ctSelectGroup;
    m_FConstructTypeMap["case"] = FConstruct::ctSelectGroup;
    m_FConstructTypeMap["casedefault"] = FConstruct::ctSelectGroup;
    m_FConstructTypeMap["endselect"] = FConstruct::ctSelectGroup;

    m_FConstructTypeMap["selecttype"] = FConstruct::ctSelectGroup;
    m_FConstructTypeMap["typeis"] = FConstruct::ctSelectGroup;
    m_FConstructTypeMap["classis"] = FConstruct::ctSelectGroup;
    m_FConstructTypeMap["classdefault"] = FConstruct::ctSelectGroup;

    m_FConstructTypeMap["selectrank"] = FConstruct::ctSelectGroup;
    m_FConstructTypeMap["rank"] = FConstruct::ctSelectGroup;
    m_FConstructTypeMap["rankdefault"] = FConstruct::ctSelectGroup;

    m_FConstructTypeMap["type"] = FConstruct::ctType;
    m_FConstructTypeMap["endtype"] = FConstruct::ctType;

    m_FConstructTypeMap["critical"] = FConstruct::ctCritical;
    m_FConstructTypeMap["endcritical"] = FConstruct::ctCritical;

    m_FConstructTypeMap["where"] = FConstruct::ctWhere;
    m_FConstructTypeMap["elsewhere"] = FConstruct::ctWhere;
    m_FConstructTypeMap["endwhere"] = FConstruct::ctWhere;

    m_FConstructTypeMap["enum"] = FConstruct::ctEnum;
    m_FConstructTypeMap["endenum"] = FConstruct::ctEnum;

    m_FConstructTypeMap["forall"] = FConstruct::ctForall;
    m_FConstructTypeMap["endforall"] = FConstruct::ctForall;

    m_FConstructTypeMap["changeteam"] = FConstruct::ctTeam;
    m_FConstructTypeMap["endteam"] = FConstruct::ctTeam;
}

void ConstrHighlighter::ClearHighlighting(cbStyledTextCtrl* control, bool forceAction)
{
    if (!control)
        return;

    if (!m_WasCleared || forceAction)
    {
        const int old_indic = control->GetIndicatorCurrent();
        control->SetIndicatorCurrent(m_IndicHighlight);
        control->IndicatorClearRange(0, control->GetLength());
        m_WasCleared = true;
        control->SetIndicatorCurrent(old_indic);
        if (forceAction)
            m_CurrentPosition = 0;
    }
}

void ConstrHighlighter::DoWork(cbEditor* editor, FortranSourceForm fsForm)
{
    cbStyledTextCtrl* control = editor->GetControl();

    if (!m_MakeHighlight || !control->GetSelectionEmpty())
    {
        ClearHighlighting(control);
        return;
    }

    m_TimeMax = 100;
    m_Watch.Start();

    int cpos = control->GetCurrentPos();
    if (cpos == m_CurrentPosition)
        return; // nothing is changed
    m_CurrentPosition = cpos;
    m_CurrentSForm = fsForm;

    const int old_indic = control->GetIndicatorCurrent();
    if (!m_WasCleared)
    {
        control->SetIndicatorCurrent(m_IndicHighlight);
        control->IndicatorClearRange(0, control->GetLength());
        m_WasCleared = true;
        control->SetIndicatorCurrent(old_indic);
    }

    int style = control->GetStyleAt(cpos);
    if (style != wxSCI_F_WORD)
        return;

    int wstart = control->WordStartPosition(cpos, true);
    if (wstart == cpos)
        return; // do not highlight if the cursor is on the start of keyword
    int wend = control->WordEndPosition(cpos, true);
    wxString cword = control->GetTextRange(wstart,wend).Lower();
    if (cword.IsEmpty())
        return;
    else if (m_KeywordSet.count(cword) == 0)
        return;

    Keyword word1;
    Keyword word2;
    Keyword word3;
    FConstruct::FCLid flid;
    KeywordList myPairs;
    bool foundFull;

    std::vector<FConstruct::FCLid> flidAll;
    flidAll = FConstruct::WordFCLidMap[cword];
    if (flidAll.empty())
        return;
    bool containsK = false;
    int flineStartPos;
    int flineEndPos;
    wxString fLine;
    GetFortranLine(control, wstart, fLine, flineStartPos, flineEndPos);
    for (size_t i=0; i<flidAll.size(); i++)
    {
        if (FConstruct::FCLReMap.count(flidAll[i]) == 0)
            continue;

        bool match = false;
        if (flidAll[i] == FConstruct::fclIf_else)
        {
            if ( FConstruct::FCLReMap[FConstruct::fclIf_else]->Matches(fLine) &&
                !FConstruct::FCLReMap[FConstruct::fclWhere_else_where]->Matches(fLine))
                    match = true;
        }
        else if (flidAll[i] == FConstruct::fclBlock_end_block)
        {
            if ( FConstruct::FCLReMap[FConstruct::fclBlock_end_block]->Matches(fLine) &&
                !FConstruct::FCLReMap[FConstruct::fclBlockdata_end_blockdata]->Matches(fLine))
                    match = true;
        }
        else if (FConstruct::FCLReMap[flidAll[i]]->Matches(fLine))
        {
            match = true;
        }

        if (match)
        {
            wxString wstr1, wstr2, wstr3;
            FConstruct::GetWordsFromFCLid(flidAll[i], wstr1, wstr2, wstr3);

            wxString str1, str2, str3;
            int str1Pos, str2Pos, str3Pos;
            int pos = FindFKeyword(control, flineStartPos, control->GetLength(), flidAll[i], wstr1, wstr2, wstr3,
                                   str1, str1Pos, str2, str2Pos, str3, str3Pos);
            word1.word = str1;
            word1.posStart = str1Pos;
            word2.word = str2;
            word2.posStart = str2Pos;
            word3.word = str3;
            word3.posStart = str3Pos;

            if ((str1 + str2 + str3) == "end")
                flid = FConstruct::fclProgGroup_end;
            else
                flid = flidAll[i];

            if (pos == wxSCI_INVALID_POSITION)
                containsK = false;
            else
                containsK = true;
            break;
        }
    }
    if (!containsK)
        return;

    FindMyPairs(control, word1, word2, word3, flid, myPairs, foundFull);

    if (m_Watch.Time() > m_TimeMax)
        return;

    if (foundFull)
        control->IndicatorSetForeground(m_IndicHighlight, m_FullColour);
    else
        control->IndicatorSetForeground(m_IndicHighlight, m_UnfinColour);
    control->IndicatorSetStyle(m_IndicHighlight, wxSCI_INDIC_ROUNDBOX);
    control->IndicatorSetAlpha(m_IndicHighlight, 100);
    control->IndicatorSetOutlineAlpha(m_IndicHighlight, 255);
    control->IndicatorSetUnder(m_IndicHighlight,true);
    control->SetIndicatorCurrent(m_IndicHighlight);

    m_WasCleared = false;

    for (KeywordList::iterator it = myPairs.begin(); it != myPairs.end(); ++it)
    {
        control->IndicatorFillRange(it->posStart, it->word.length());
    }
    control->SetIndicatorCurrent(old_indic);
}


void ConstrHighlighter::FindMyPairs(cbStyledTextCtrl* control, Keyword &word1, Keyword &word2, Keyword &word3, FConstruct::FCLid flid,
                                    KeywordList &myPairs, bool &foundFull)
{
    foundFull = false;
    // Get Fortran construct to search for
    FConstruct fcon;       // full construct
    FConstruct unFconBack; // unfinished construct we should search
    FConstruct unFconForth;
    GetFortranConstruct(word1, word2, word3, fcon);
    if (fcon.GetType() != FConstruct::ctProgramGroup && fcon.GetType() != FConstruct::ctSelectGroup && fcon.Size() == 0)
        return;

    SearchDirection sdir;
    GetSearchDirection(word1, word2, word3, sdir, unFconBack, unFconForth);
    if (sdir == sdirUnknown)
        return;

    myPairs.push_back(word1);
    int wordsEndPos = word1.posStart + word1.word.size();
    if (!word2.word.IsEmpty())
    {
        myPairs.push_back(word2);
        wordsEndPos = word2.posStart + word2.word.size();
        if (!word3.word.IsEmpty())
        {
            myPairs.push_back(word3);
            wordsEndPos = word3.posStart + word3.word.size();
        }
    }

    bool foundBack = true;
    if (sdir == sdirBack || sdir == sdirBackForth)
    {
        // Search backward
        int pLimitBack = FindLimitPos(control, fcon, word1.posStart, sdirBack);
        if (m_Watch.Time() > m_TimeMax)
            return;
        if (fcon.GetType() == FConstruct::ctProgramGroup || fcon.GetType() == FConstruct::ctSelectGroup)
            SearchUnConGroup(control, word1.posStart, pLimitBack, fcon.GetType(), unFconBack, flid, foundBack, myPairs);
        else
            SearchUnCon(control, word1.posStart, pLimitBack, fcon, unFconBack, foundBack, myPairs);
    }

    bool foundForth = true;
    if (sdir == sdirForth || sdir == sdirBackForth)
    {
        // Search forth
        int pLimitForth = FindLimitPos(control, fcon, wordsEndPos, sdirForth);
        if (m_Watch.Time() > m_TimeMax)
            return;
        if (fcon.GetType() == FConstruct::ctProgramGroup || fcon.GetType() == FConstruct::ctSelectGroup)
            SearchUnConGroup(control, wordsEndPos, pLimitForth, fcon.GetType(), unFconForth, flid, foundForth, myPairs);
        else
            SearchUnCon(control, wordsEndPos, pLimitForth, fcon, unFconForth, foundForth, myPairs);
    }
    foundFull = (foundBack && foundForth);
}

void ConstrHighlighter::SearchUnCon(cbStyledTextCtrl* control, int pStartC, int pEndC, const FConstruct& fcon, const FConstruct& unFcon,
                                       bool& foundEnd, KeywordList& myPairs)
{
    foundEnd = false;
    wxString word1;
    wxString word2;
    wxString word3;
    FConstruct::FCLid flid;
    fcon.GetWords(0, word1, word2, word3, flid);

    wxString wordE1;
    wxString wordE2;
    wxString wordE3;
    FConstruct::FCLid flidE;
    fcon.GetWords(fcon.Size()-1, wordE1, wordE2, wordE3, flidE);

    bool searchBack = pStartC > pEndC;
    int pStart   = pStartC;
    int pEnd     = pEndC;
    int level    = 0;
    int cutEnd   = -1;
    int cutStart = -1;
    std::vector<int> cutEndVec;
    std::vector<int> cutStartVec;

    wxString str1, str2, str3;
    int str1Pos, str2Pos, str3Pos;

    wxString strE1, strE2, strE3;
    int strE1Pos, strE2Pos, strE3Pos;
    bool haveStart = false;
    bool haveEnd   = false;

    while (true)
    {
        if (m_Watch.Time() > m_TimeMax)
            return;
        int posFCstart;
        int posFCend;
        if (!haveStart)
            posFCstart = FindFKeyword(control, pStart, pEnd, flid, word1, word2, word3,
                                      str1, str1Pos, str2, str2Pos, str3, str3Pos);
        if (!haveEnd)
            posFCend = FindFKeyword(control, pStart, pEnd, flidE, wordE1, wordE2, wordE3,
                                      strE1, strE1Pos, strE2, strE2Pos, strE3, strE3Pos);

        if (searchBack)
        {
            if (posFCstart == wxSCI_INVALID_POSITION && posFCend != wxSCI_INVALID_POSITION)
            {
                pEnd = GetWordsEnd(strE1, strE1Pos, strE2, strE2Pos, strE3, strE3Pos);
                break; // try to search intermediate keywords
            }
            else if (posFCstart == wxSCI_INVALID_POSITION && posFCend == wxSCI_INVALID_POSITION)
            {
                pEnd = pEndC;
                break; // try to search intermediate keywords
            }
            else if (posFCstart != wxSCI_INVALID_POSITION && posFCend == wxSCI_INVALID_POSITION)
                posFCend = -1;
        }
        else
        {
            if (posFCstart != wxSCI_INVALID_POSITION && posFCend == wxSCI_INVALID_POSITION)
            {
                pEnd = posFCstart;
                break; // try to search intermediate keywords
            }
            else if (posFCstart == wxSCI_INVALID_POSITION && posFCend == wxSCI_INVALID_POSITION)
            {
                pEnd = pEndC;
                break; // try to search intermediate keywords
            }
            else if (posFCstart == wxSCI_INVALID_POSITION && posFCend != wxSCI_INVALID_POSITION)
                posFCstart = control->GetLength();
        }

        if (searchBack)
        {
            if (posFCstart < posFCend)
            {
                // Starts another level of construct
                if (level == 0)
                    cutEnd = GetWordsEnd(strE1, strE1Pos, strE2, strE2Pos, strE3, strE3Pos);
                level += 1;
                pStart = posFCend;
                haveStart = true;
                haveEnd   = false;
            }
            else // (posFCstart > posFCend)
            {
                if (level == 0)
                {
                    PutToKeywordList(str1, str2, str3, str1Pos, str2Pos, str3Pos, myPairs);
                    foundEnd = true;
                    pEnd = GetWordsEnd(str1, str1Pos, str2, str2Pos, str3, str3Pos);
                    break;
                }
                else if (level == 1)
                {
                    cutStart = posFCstart;
                    cutEndVec.push_back(cutEnd);
                    cutStartVec.push_back(cutStart);
                }
                level -= 1;
                pStart = posFCstart;
                haveStart = false;
                haveEnd   = true;
            }
        }
        else // search forth
        {
            if (posFCstart < posFCend)
            {
                // Starts another level of construct
                if (level == 0)
                    cutStart = posFCstart;
                level += 1;
                pStart = GetWordsEnd(str1, str1Pos, str2, str2Pos, str3, str3Pos);
                haveStart = false;
                haveEnd   = true;
            }
            else // (posFCstart > posFCend)
            {
                if (level == 0)
                {
                    PutToKeywordList(strE1, strE2, strE3, strE1Pos, strE2Pos, strE3Pos, myPairs);
                    foundEnd = true;
                    pEnd = strE1Pos;
                    break;
                }
                else if (level == 1)
                {
                    cutEnd = GetWordsEnd(strE1, strE1Pos, strE2, strE2Pos, strE3, strE3Pos);
                    cutEndVec.push_back(cutEnd);
                    cutStartVec.push_back(cutStart);
                }
                level -= 1;
                pStart = GetWordsEnd(strE1, strE1Pos, strE2, strE2Pos, strE3, strE3Pos);
                haveStart = true;
                haveEnd   = false;
            }
        }
    }

    // Find intermediate keywords
    SearchUnConIntermediate(control, pStartC, pEnd, unFcon, cutEndVec, cutStartVec, myPairs);
}

void ConstrHighlighter::SearchUnConGroup(cbStyledTextCtrl* control, int pStartC, int pEndC, FConstruct::FConstructType fct,
                                       const FConstruct& unFcon, FConstruct::FCLid fcl, bool& foundEnd, KeywordList& myPairs)
{
    foundEnd = false;

    bool searchBack = pStartC > pEndC;
    int pStart = pStartC;
    int pEnd   = pEndC;
    int level  = 0;
    int cutEnd   = -1;
    int cutStart = -1;
    std::vector<int> cutEndVec;
    std::vector<int> cutStartVec;

    wxString str1, str2, str3;
    int str1Pos, str2Pos, str3Pos;

    wxString strE1, strE2, strE3;
    int strE1Pos, strE2Pos, strE3Pos;

    FConstruct::FCLid flid;
    FConstruct::FCLid flidE;
    if (fct == FConstruct::ctProgramGroup)
    {
        flid = FConstruct::fclProgGroup_start;
        flidE = FConstruct::fclProgGroup_end;
    }
    else if (fct == FConstruct::ctSelectGroup)
    {
        flid = FConstruct::fclSelGroup_start;
        flidE = FConstruct::fclSelGroup_end;
    }
    else
        return; // this should not happen

    bool haveStart = false;
    bool haveEnd = false;
    int posFCstart;
    int posFCend;

    while (true)
    {
        if (m_Watch.Time() > m_TimeMax)
            return;
        int lineStartPos;
        int lineEndPos;
        FConstruct::FCLid flidFound;
        if (!haveStart)
            posFCstart = FindGroupKeyword(control, pStart, pEnd, flid,
                                      str1, str1Pos, str2, str2Pos, str3, str3Pos);

        bool oneWord;
        if (!haveEnd)
        {
            if (fct == FConstruct::ctProgramGroup)
                posFCend = FindProgGroupEndKeywordPos(control, pStart, pEnd, lineStartPos, lineEndPos, flidFound, oneWord);
            else
                posFCend = FindGroupKeyword(control, pStart, pEnd, flidE, strE1, strE1Pos, strE2, strE2Pos, strE3, strE3Pos);
        }

        if (searchBack)
        {
            if (posFCstart == wxSCI_INVALID_POSITION)
                break;
            else if (posFCend == wxSCI_INVALID_POSITION)
                posFCend = -1;
        }
        else
        {
            if (posFCend == wxSCI_INVALID_POSITION)
                break;
            else if (posFCstart == wxSCI_INVALID_POSITION)
                posFCstart = control->GetLength();
        }

        if (searchBack)
        {
            if (posFCstart < posFCend)
            {
                // Starts another level of construct
                if (level == 0)
                    cutEnd = GetWordsEnd(strE1, strE1Pos, strE2, strE2Pos, strE3, strE3Pos);
                level += 1;
                pStart = posFCend;
                haveStart = true;
                haveEnd = false;
                continue;
            }
            else if (posFCstart > posFCend)
            {
                if (level == 0)
                {
                    wxString strFound = str1 + str2 + str3;
                    if ( fcl == FConstruct::fclProgGroup_end ||
                        (fcl == FConstruct::fclSub_end_sub && strFound.IsSameAs("subroutine")) ||
                        (fcl == FConstruct::fclFun_end_fun && strFound.IsSameAs("function")) ||
                        (fcl == FConstruct::fclMod_end_module && strFound.IsSameAs("module")) ||
                        (fcl == FConstruct::fclSubmod_end_submod && strFound.IsSameAs("submodule")) ||
                        (fcl == FConstruct::fclProg_end_prog && strFound.IsSameAs("program")) ||
                        (fcl == FConstruct::fclBlockdata_end_blockdata && strFound.IsSameAs("blockdata")) ||
                        (fcl == FConstruct::fclProc_end_proc && strFound.IsSameAs("moduleprocedure")) ||

                         fcl == FConstruct::fclSelect_end ||
                        (fcl == FConstruct::fclSelectCase_case && strFound.IsSameAs("selectcase")) ||
                        (fcl == FConstruct::fclSelectType_type_is && strFound.IsSameAs("selecttype")) ||
                        (fcl == FConstruct::fclSelectType_class_is && strFound.IsSameAs("selecttype")) ||
                        (fcl == FConstruct::fclSelectType_class_default && strFound.IsSameAs("selecttype")) ||
                        (fcl == FConstruct::fclSelectRank_rank && strFound.IsSameAs("selectrank")) )
                    {
                        PutToKeywordList(str1, str2, str3, str1Pos, str2Pos, str3Pos, myPairs);
                        foundEnd = true;
                        pEnd = GetWordsEnd(str1, str1Pos, str2, str2Pos, str3, str3Pos);
                    }
                    break;
                }
                else if (fct == FConstruct::ctSelectGroup && level == 1)
                {
                    cutStart = posFCstart;
                    cutEndVec.push_back(cutEnd);
                    cutStartVec.push_back(cutStart);
                }
                level -= 1;
                pStart = posFCstart;
                haveStart = false;
                haveEnd = true;
            }
        }
        else // search forth
        {
            if (posFCstart < posFCend)
            {
                // Starts another level of construct
                if (level == 0)
                    cutStart = posFCstart;
                level += 1;
                pStart = GetWordsEnd(str1, str1Pos, str2, str2Pos, str3, str3Pos);
                haveStart = false;
                haveEnd = true;
                continue;
            }
            else if (posFCstart > posFCend)
            {
                if (level == 0)
                {
                    if (fct == FConstruct::ctProgramGroup)
                    {
                        if ( oneWord ||
                            (fcl == FConstruct::fclBlockdata_blockdata && flidFound == FConstruct::fclBlockdata_end_blockdata))
                        {
                            GetKeyworsFromLine(control, flidFound, lineStartPos, lineEndPos, strE1, strE2, strE3, strE1Pos, strE2Pos, strE3Pos);

                            wxString strFound = strE1 + strE2 + strE3;
                            if ( strFound.IsSameAs("end") ||
                                 (fcl == FConstruct::fclSub_sub && strFound.IsSameAs("endsubroutine")) ||
                                 (fcl == FConstruct::fclFun_fun && strFound.IsSameAs("endfunction")) ||
                                 (fcl == FConstruct::fclMod_module && strFound.IsSameAs("endmodule")) ||
                                 (fcl == FConstruct::fclSubmod_submod && strFound.IsSameAs("endsubmodule")) ||
                                 (fcl == FConstruct::fclProg_prog && strFound.IsSameAs("endprogram")) ||
                                 (fcl == FConstruct::fclProc_mod_proc && strFound.IsSameAs("endprocedure")) )
                            {
                                PutToKeywordList(strE1, strE2, strE3, strE1Pos, strE2Pos, strE3Pos, myPairs);
                                foundEnd = true;
                            }
                        }
                        else if ((fcl == FConstruct::fclSub_sub && flidFound == FConstruct::fclSub_end_sub) ||
                                 (fcl == FConstruct::fclFun_fun && flidFound == FConstruct::fclFun_end_fun) ||
                                 (fcl == FConstruct::fclMod_module && flidFound == FConstruct::fclMod_end_module) ||
                                 (fcl == FConstruct::fclSubmod_submod && flidFound == FConstruct::fclSubmod_end_submod) ||
                                 (fcl == FConstruct::fclProg_prog && flidFound == FConstruct::fclProg_end_prog) ||
                                 (fcl == FConstruct::fclProc_mod_proc && flidFound == FConstruct::fclProc_end_proc))
                        {
                            std::vector<wxString> sWords = FConstruct::FCLWordMap[flidFound];
                            strE1 = sWords[0]+sWords[1];
                            strE2 = wxEmptyString;
                            strE3 = wxEmptyString;
                            PutToKeywordList(strE1, strE2, strE3, posFCend, 0, 0, myPairs);
                            foundEnd = true;
                        }
                    }
                    else
                    {
                        wxString strFound = strE1 + strE2 + strE3;
                        if (strFound.IsSameAs("endselect"))
                        {
                            PutToKeywordList(strE1, strE2, strE3, strE1Pos, strE2Pos, strE3Pos, myPairs);
                            foundEnd = true;
                            pEnd = strE1Pos;
                        }
                    }
                    break;
                }
                else if (fct == FConstruct::ctSelectGroup && level == 1)
                {
                    cutEndVec.push_back(GetWordsEnd(strE1, strE1Pos, strE2, strE2Pos, strE3, strE3Pos));
                    cutStartVec.push_back(cutStart);
                }
                level -= 1;
                if (fct == FConstruct::ctProgramGroup)
                    pStart = lineEndPos;
                else
                    pStart = GetWordsEnd(strE1, strE1Pos, strE2, strE2Pos, strE3, strE3Pos);
                haveStart = true;
                haveEnd = false;
            }
        }
    }

    if (fct == FConstruct::ctSelectGroup)
    {
        // Find intermediate keywords
        SearchUnConIntermediate(control, pStartC, pEnd, unFcon, cutEndVec, cutStartVec, myPairs);
    }
}

void ConstrHighlighter::SearchUnConIntermediate(cbStyledTextCtrl* control, int pStartC, int pEndC, const FConstruct& unFcon,
                                              std::vector<int>& cutEndVec, std::vector<int>& cutStartVec, KeywordList& myPairs)
{
    // Find intermediate keywords
    bool searchBack = pStartC > pEndC;
    FConstruct::FCLid flid;
    wxString str1, str2, str3;
    int str1Pos, str2Pos, str3Pos;

    for (size_t i=1; i<unFcon.Size(); i++)
    {
        int ik = searchBack? i : i-1;
        wxString word1, word2, word3;
        unFcon.GetWords(ik, word1, word2, word3, flid);
        if (flid == FConstruct::fclUnknown)
            return; // something wrong
        int pInterStart = pStartC;
        int pInterEnd = pEndC;

        while (true)
        {
            int pos = FindFKeyword(control, pInterStart, pInterEnd, flid, word1, word2, word3,
                                      str1, str1Pos, str2, str2Pos, str3, str3Pos);
            if (pos == wxSCI_INVALID_POSITION)
                break;

            bool wasCut = false;
            for (size_t j=0; j<cutStartVec.size(); j++)
            {
                if (pos > cutStartVec[j] && pos < cutEndVec[j])
                {
                    wasCut = true;
                    break;
                }
            }

            if (!wasCut)
                PutToKeywordList(str1, str2, str3, str1Pos, str2Pos, str3Pos, myPairs);

            if (searchBack)
                pInterStart = pos;
            else
                pInterStart = GetWordsEnd(str1, str1Pos, str2, str2Pos, str3, str3Pos);
        }
    }
}

int ConstrHighlighter::GetWordsEnd(wxString& str1, int str1Pos, wxString& str2, int str2Pos, wxString& str3, int str3Pos)
{
    int wend = -1;
    if (!str3.IsEmpty())
        wend = str3Pos + str3.length();
    else if (!str2.IsEmpty())
        wend = str2Pos + str2.length();
    else if (!str1.IsEmpty())
        wend = str1Pos + str1.length();
    return wend;
}

void ConstrHighlighter::PutToKeywordList(wxString& str1, wxString& str2, wxString& str3,
                                       int str1Pos, int str2Pos, int str3Pos, KeywordList& kList)
{
    Keyword sk = {str1, str1Pos};
    kList.push_back(sk);
    if (str2Pos != wxSCI_INVALID_POSITION)
    {
        sk.word = str2;
        sk.posStart = str2Pos;
        kList.push_back(sk);
        if (str3Pos != wxSCI_INVALID_POSITION)
        {
            sk.word = str3;
            sk.posStart = str3Pos;
            kList.push_back(sk);
        }
    }
}

int ConstrHighlighter::FindFKeywordFull(cbStyledTextCtrl* control, int pStart, int pEnd, FConstruct::FCLid flid, const wxString& sWord1, const wxString& sWord2, const wxString& sWord3,
                                      wxString& str1, int& str1Pos, wxString& str2, int& str2Pos, wxString& str3, int& str3Pos)
{
    if (FConstruct::FCLReMap.count(flid) == 0)
        return wxSCI_INVALID_POSITION;

    bool searchBack = pStart > pEnd;
    int flag;
    wxString sw1;
    int cStart = pStart;
    if (sWord2.IsEmpty())
    {
        sw1 = sWord1;
        flag = wxSCI_FIND_WORDSTART;
    }
    else
    {
        sw1 = sWord2;
        flag = 0;
    }

    while (true)
    {
        int pos = FindText(control, cStart, pEnd, sw1, flag);
        if (pos == wxSCI_INVALID_POSITION)
            return wxSCI_INVALID_POSITION;

        wxString fLine;
        int lineStartPos;
        int lineEndPos;
        GetFortranLine(control, pos, fLine, lineStartPos, lineEndPos);

        if (FConstruct::FCLReMap[flid]->Matches(fLine))
        {
            int nfilled = 0;
            pos = FindText(control, lineStartPos, lineEndPos, sWord1, wxSCI_FIND_WHOLEWORD);
            if (pos != wxSCI_INVALID_POSITION)
            {
                str1 = sWord1;
                str1Pos = pos;
                if (sWord2.IsEmpty())
                    return pos;
                sw1 = sWord2;
                nfilled++;
            }
            else
                sw1 = sWord1 + sWord2;

            pos = FindText(control, lineStartPos, lineEndPos, sw1, wxSCI_FIND_WHOLEWORD);
            if (pos != wxSCI_INVALID_POSITION)
            {
                if (nfilled == 0)
                {
                    str1 = sWord1;
                    str1Pos = pos;
                }
                else
                {
                    str2 = sWord2;
                    str2Pos = pos;
                }

                if (sWord3.IsEmpty())
                    return pos;
                sw1 = sWord3;
                nfilled++;
            }
            else
                return wxSCI_INVALID_POSITION;

            pos = FindText(control, lineStartPos, lineEndPos, sw1, wxSCI_FIND_WHOLEWORD);
            if (pos != wxSCI_INVALID_POSITION)
            {
                if (nfilled == 1)
                {
                    str2 = sw1;
                    str2Pos = pos;
                }
                else
                {
                    str3 = sw1;
                    str3Pos = pos;
                }
            }
            else
                return wxSCI_INVALID_POSITION;
        }
        else
        {
            if(searchBack)
            {
                cStart = lineStartPos;
                if (cStart <= pEnd)
                    return wxSCI_INVALID_POSITION;
            }
            else
            {
                cStart = lineEndPos;
                if (cStart >= pEnd)
                    return wxSCI_INVALID_POSITION;
            }
        }
    }
}

int ConstrHighlighter::FindProgGroupEndKeywordPos(cbStyledTextCtrl* control, int pStart, int pEnd, int& lineStartPos, int& lineEndPos,
                                                  FConstruct::FCLid& flid, bool& oneWord)
{
    int nUsed = 0;
    bool searchBack = pStart > pEnd;

    flid = FConstruct::fclUnknown;
    oneWord = false;

    std::vector<FConstruct::FCLid> flidAll;
    flidAll.push_back(FConstruct::fclFun_end_fun);
    flidAll.push_back(FConstruct::fclSub_end_sub);
    flidAll.push_back(FConstruct::fclProg_end_prog);
    flidAll.push_back(FConstruct::fclMod_end_module);
    flidAll.push_back(FConstruct::fclSubmod_end_submod);
    flidAll.push_back(FConstruct::fclBlockdata_end_blockdata);
    flidAll.push_back(FConstruct::fclProc_end_proc);

    wxString sW = "end";
    while (true)
    {
        int pos = FindText(control, pStart, pEnd, sW, wxSCI_FIND_WORDSTART);

        if (pos == wxSCI_INVALID_POSITION)
            break;

        int wordendpos = control->WordEndPosition(pos, true);
        wxString foundWord = control->GetTextRange(pos, wordendpos).Lower();
        if (foundWord.IsSameAs(sW))
        {
            nUsed = 1;
        }
        else if (foundWord.IsSameAs("endsubroutine"))
        {
            flid = FConstruct::fclSub_end_sub;
            nUsed = 2;
        }
        else if (foundWord.IsSameAs("endfunction"))
        {
            flid = FConstruct::fclFun_end_fun;
            nUsed = 2;
        }
        else if (foundWord.IsSameAs("endprogram"))
        {
            flid = FConstruct::fclProg_end_prog;
            nUsed = 2;
        }
        else if (foundWord.IsSameAs("endmodule"))
        {
            flid = FConstruct::fclMod_end_module;
            nUsed = 2;
        }
        else if (foundWord.IsSameAs("endsubmodule"))
        {
            flid = FConstruct::fclSubmod_end_submod;
            nUsed = 2;
        }
        else if (foundWord.IsSameAs("endblock"))
        {
            flid = FConstruct::fclBlockdata_end_blockdata;
            nUsed = 2;
        }
        else if (foundWord.IsSameAs("endblockdata"))
        {
            flid = FConstruct::fclBlockdata_end_blockdata;
            nUsed = 3;
        }
        else if (foundWord.IsSameAs("endprocedure"))
        {
            flid = FConstruct::fclProc_end_proc;
            nUsed = 2;
        }
        else
        {
            pStart = searchBack? pos : pos+foundWord.length();
            continue;
        }

        wxString fLine;
        GetFortranLine(control, pos, fLine, lineStartPos, lineEndPos);

        if (nUsed == 1)
        {
            for (size_t i=0; i<flidAll.size(); i++)
            {
                if (FConstruct::FCLReMap[flidAll[i]]->Matches(fLine))
                {
                    oneWord = true;
                    flid = flidAll[i];
                    return pos;
                }
            }
        }
        else
        {
            if (FConstruct::FCLReMap[flid]->Matches(fLine))
                return pos;
        }

        pStart = searchBack? pos : pos+foundWord.length();
    }
    return wxSCI_INVALID_POSITION;
}

int ConstrHighlighter::FindGroupKeyword(cbStyledTextCtrl* control, int pStart, int pEnd, FConstruct::FCLid flid,
                                      wxString& rstr1, int& rstr1Pos, wxString& rstr2, int& rstr2Pos, wxString& rstr3, int& rstr3Pos)
{
    int posGK = wxSCI_INVALID_POSITION;
    rstr1 = wxEmptyString;
    rstr1Pos = wxSCI_INVALID_POSITION;
    rstr2 = wxEmptyString;
    rstr2Pos = wxSCI_INVALID_POSITION;
    rstr3 = wxEmptyString;
    rstr3Pos = wxSCI_INVALID_POSITION;

    std::vector<FConstruct::FCLid> flidAll;
    if (flid == FConstruct::fclProgGroup_end)
    {
        flidAll.push_back(FConstruct::fclFun_end_fun);
        flidAll.push_back(FConstruct::fclSub_end_sub);
        flidAll.push_back(FConstruct::fclProg_end_prog);
        flidAll.push_back(FConstruct::fclMod_end_module);
        flidAll.push_back(FConstruct::fclSubmod_end_submod);
        flidAll.push_back(FConstruct::fclBlockdata_end_blockdata);
        flidAll.push_back(FConstruct::fclProc_end_proc);
    }
    else if (flid == FConstruct::fclProgGroup_start)
    {
        flidAll.push_back(FConstruct::fclFun_fun);
        flidAll.push_back(FConstruct::fclSub_sub);
        flidAll.push_back(FConstruct::fclProg_prog);
        flidAll.push_back(FConstruct::fclMod_module);
        flidAll.push_back(FConstruct::fclSubmod_submod);
        flidAll.push_back(FConstruct::fclBlockdata_blockdata);
        flidAll.push_back(FConstruct::fclProc_mod_proc);
    }
    else if (flid == FConstruct::fclSelGroup_end)
    {
        flidAll.push_back(FConstruct::fclSelect_end);
    }
    else if (flid == FConstruct::fclSelGroup_start)
    {
        flidAll.push_back(FConstruct::fclSelectCase_start);
        flidAll.push_back(FConstruct::fclSelectType_start);
        flidAll.push_back(FConstruct::fclSelectRank_start);
    }
    else
        return posGK;

    wxString str1, str2, str3;
    int str1Pos, str2Pos, str3Pos;
    bool searchBack = pStart > pEnd;

    for (size_t i=0; i<flidAll.size(); i++)
    {
        std::vector<wxString> sWords;
        if (FConstruct::FCLWordMap.count(flidAll[i]) == 0)
            continue;
        sWords = FConstruct::FCLWordMap[flidAll[i]];

        int pos = FindFKeyword(control, pStart, pEnd, flidAll[i], sWords[0], sWords[1], sWords[2],
                               str1, str1Pos, str2, str2Pos, str3, str3Pos);
        if (pos != wxSCI_INVALID_POSITION)
        {
            if ((posGK == wxSCI_INVALID_POSITION) ||
                (posGK != wxSCI_INVALID_POSITION && searchBack && pos > posGK) ||
                (posGK != wxSCI_INVALID_POSITION && !searchBack && pos < posGK))
            {
                posGK = pos;
                rstr1 = str1;
                rstr1Pos = str1Pos;
                rstr2 = str2;
                rstr2Pos = str2Pos;
                rstr3 = str3;
                rstr3Pos = str3Pos;
                if (searchBack)
                    pEnd = pos + 2;
                else
                    pEnd = pos - 2;
            }
        }
    }
    return posGK;
}

int ConstrHighlighter::FindFKeyword(cbStyledTextCtrl* control, int pStart, int pEnd, FConstruct::FCLid flid, const wxString& sWord1, const wxString& sWord2, const wxString& sWord3,
                                  wxString& str1, int& str1Pos, wxString& str2, int& str2Pos, wxString& str3, int& str3Pos)
{
    const int flag = wxSCI_FIND_WORDSTART; // wxSCI_FIND_WHOLEWORD;
    int nUsed;
    str1 = wxEmptyString;
    str1Pos = wxSCI_INVALID_POSITION;
    str2 = wxEmptyString;
    str2Pos = wxSCI_INVALID_POSITION;
    str3 = wxEmptyString;
    str3Pos = wxSCI_INVALID_POSITION;
    bool searchBack = pStart > pEnd;

    while (true)
    {
        wxString sW = sWord1;
        int pos = FindText(control, pStart, pEnd, sW, flag);

        if (pos == wxSCI_INVALID_POSITION)
            return wxSCI_INVALID_POSITION;

        int wordendpos = control->WordEndPosition(pos, true);
        wxString foundWord = control->GetTextRange(pos, wordendpos).Lower();
        if (foundWord.IsSameAs(sWord1))
            nUsed = 1;
        else if (!sWord2.empty() && foundWord.IsSameAs(sWord1+sWord2))
            nUsed = 2;
        else if (flid == FConstruct::fclBlockdata_end_blockdata && foundWord.IsSameAs("endblockdata"))
            nUsed = 3;
        else
        {
            pStart = searchBack? pos : pos+foundWord.length();
            continue;
        }

        str1 = foundWord;
        str1Pos = pos;
        wxString fLine;
        int lineStartPos;
        int lineEndPos;
        GetFortranLine(control, pos, fLine, lineStartPos, lineEndPos);

        if (FConstruct::FCLReMap.count(flid) == 0)
            return wxSCI_INVALID_POSITION;

        bool match = false;
        if (flid == FConstruct::fclIf_else)
        {
            if ( FConstruct::FCLReMap[FConstruct::fclIf_else]->Matches(fLine) &&
                !FConstruct::FCLReMap[FConstruct::fclWhere_else_where]->Matches(fLine))
                    match = true;
        }
        else if (flid == FConstruct::fclBlock_end_block)
        {
            if ( FConstruct::FCLReMap[FConstruct::fclBlock_end_block]->Matches(fLine) &&
                !FConstruct::FCLReMap[FConstruct::fclBlockdata_end_blockdata]->Matches(fLine))
                    match = true;
        }
        else if (FConstruct::FCLReMap[flid]->Matches(fLine))
            match = true;

        if (match)
        {
            // We already found keyword.
            // Now find positions of sWord2 and sWord3
            if (nUsed == 1 && sWord2.IsEmpty())
            {
                if (flid == FConstruct::fclInterf_interf)
                {
                    // check "abstract interface"
                    pos = FindText(control, pos-16, pos-1, "abstract", wxSCI_FIND_WHOLEWORD);
                    if (pos != wxSCI_INVALID_POSITION)
                    {
                        str2Pos = str1Pos;
                        str2 = str1;
                        str1Pos = pos;
                        str1 = "abstract";
                    }
                }
                else if (flid == FConstruct::fclDo_do)
                {
                    pos = FindText(control, str1Pos+3, str1Pos+10, "while", wxSCI_FIND_WHOLEWORD);
                    if (pos != wxSCI_INVALID_POSITION)
                    {
                        str2Pos = pos;
                        str2 = "while";
                    }
                    else
                    {
                        pos = FindText(control, str1Pos+3, str1Pos+15, "concurrent", wxSCI_FIND_WHOLEWORD);
                        if (pos != wxSCI_INVALID_POSITION)
                        {
                            str2Pos = pos;
                            str2 = "concurrent";
                        }
                    }
                }
                return str1Pos;
            }
            else if (nUsed == 1)
            {
                sW = sWord2;
                nUsed = 2;
            }
            else if ((nUsed == 2) && sWord3.IsEmpty())
                return str1Pos;
            else if (nUsed == 3)
                return str1Pos;
            else
            {
                sW = sWord3;
                nUsed = 3;
            }

            if (nUsed == 2 && flid == FConstruct::fclBlockdata_end_blockdata)
            {
                pos = FindText(control, pos+1, lineEndPos, sW, wxSCI_FIND_WORDSTART);
                wordendpos = control->WordEndPosition(pos, true);
                foundWord = control->GetTextRange(pos, wordendpos).Lower();
                if (foundWord.IsSameAs("block"))
                {
                    str2 = foundWord;
                    str2Pos = pos;
                    sW = sWord3;
                    pos = FindText(control, pos+1, lineEndPos, sW, wxSCI_FIND_WHOLEWORD);
                    if (pos == wxSCI_INVALID_POSITION)
                        return str1Pos;
                    else
                    {
                        str3 = sW;
                        str3Pos = pos;
                        return str1Pos;
                    }
                }
                else if (foundWord.IsSameAs("blockdata"))
                {
                    str2 = foundWord;
                    str2Pos = pos;
                    return str1Pos;
                }
                else
                    return wxSCI_INVALID_POSITION;
            }
            else
                pos = FindText(control, pos+1, lineEndPos, sW, wxSCI_FIND_WHOLEWORD);

            if (pos == wxSCI_INVALID_POSITION)
                return str1Pos; // Second word was not found.
            if (nUsed == 2)
            {
                str2 = sW;
                str2Pos = pos;
                if (!sWord3.IsEmpty())
                {
                    pos = FindText(control, pos+1, lineEndPos, sWord3, wxSCI_FIND_WHOLEWORD);
                    if (pos != wxSCI_INVALID_POSITION)
                    {
                        str3 = sWord3;
                        str3Pos = pos;
                    }
                }
            }
            else // (nUsed == 3)
            {
                str2 = sW;
                str2Pos = pos;
            }

            return str1Pos;
        }
        else
        {
            pStart = searchBack? pos : pos+foundWord.length();
        }
    }
    return wxSCI_INVALID_POSITION;
}

void ConstrHighlighter::GetKeyworsFromLine(cbStyledTextCtrl* control, FConstruct::FCLid flid, int lineStartPos, int lineEndPos,
                                           wxString& str1, wxString& str2, wxString& str3, int& str1Pos, int& str2Pos, int& str3Pos)
{
    str1 = wxEmptyString;
    str2 = wxEmptyString;
    str3 = wxEmptyString;

    if (FConstruct::FCLWordMap.count(flid) == 0)
        return;

    std::vector<wxString> sWords = FConstruct::FCLWordMap[flid];
    wxString sw;
    int nFilled = 0;
    for (size_t i=0; i<3; i++)
    {
        if (sWords[i].IsEmpty())
            break;
        sw.Append(sWords[i]);

        int pos = FindText(control, lineStartPos, lineEndPos, sw, wxSCI_FIND_WHOLEWORD);
        if (pos != wxSCI_INVALID_POSITION)
        {
            if (nFilled == 0)
            {
                str1 = sw;
                str1Pos = pos;
            }
            else if (nFilled == 1)
            {
                str2 = sw;
                str2Pos = pos;
            }
            else
            {
                str3 = sw;
                str3Pos = pos;
            }
            sw = wxEmptyString;
            nFilled++;
        }
    }
}

int ConstrHighlighter::FindText(cbStyledTextCtrl* control, int pStart, int pEnd, const wxString& sWord, int flag)
{
    int pos;
    int pS = pStart;
    while (true)
    {
        pos = control->FindText(pS, pEnd, sWord, flag);
        if (pos == wxSCI_INVALID_POSITION)
            return pos;
        else if (IsCommentOrString(control, pos))
            pS = pos + 1;
        else
            return pos;
    }
}

bool ConstrHighlighter::IsCommentOrString(cbStyledTextCtrl* control, int pos)
{
    int line = control->LineFromPosition(pos);
    int lineStartPos = control->PositionFromLine(line);
    bool inStr1 = false;
    bool inStr2 = false;
    bool inCom = false;

    if (m_CurrentSForm == fsfFixed && lineStartPos <= pos)
    {
            const wxChar cch = control->GetCharAt(lineStartPos);
            if (cch == 'C' || cch == 'c' || cch == '*' || cch == '!')
                return true; // comment line in fixed format file
    }

    for (int i=lineStartPos; i<pos; i++)
    {
        const wxChar cch = control->GetCharAt(i);
        if (cch == '!' && !inStr1 && !inStr2)
        {
            inCom = true;
            break;
        }
        else if (cch == '\'' && !inStr2)
        {
            if (inStr1)
                inStr1 = false;
            else
                inStr1 = true;
        }
        else if (cch == '"' && !inStr1)
        {
            if (inStr2)
                inStr2 = false;
            else
                inStr2 = true;
        }
    }

    if (inCom || inStr1 || inStr2)
        return true;
    else
        return false;
}

void ConstrHighlighter::GetFortranConstruct(const Keyword &word1, const Keyword &word2, const Keyword &word3, FConstruct &fcon)
{
    fcon.Clear();
    wxString str = word1.word + word2.word + word3.word;
    if (m_FConstructTypeMap.count(str) == 0)
        return;

    wxString estr;
    FConstruct::FConstructType fct = m_FConstructTypeMap[str];
    if (fct == FConstruct::ctProgramGroup || fct == FConstruct::ctSelectGroup)
        fcon.SetType(fct);
    else if (fct == FConstruct::ctIf)
    {
        fcon.AddPart("if", "then", estr);
        fcon.AddPart("else", "if", "then");
        fcon.AddPart("else", estr, estr);
        fcon.AddPart("end", "if", estr);
        fcon.SetType(FConstruct::ctIf);
    }
    else if (fct == FConstruct::ctDo)
    {
        fcon.AddPart("do", estr, estr);
        fcon.AddPart("end", "do", estr);
        fcon.SetType(FConstruct::ctDo);
    }
    else if (fct == FConstruct::ctInterface)
    {
        fcon.AddPart("interface", estr, estr);
        fcon.AddPart("end", "interface", estr);
        fcon.SetType(FConstruct::ctInterface);
    }
    else if (fct == FConstruct::ctFunction)
    {
        fcon.AddPart("function", estr, estr);
        fcon.AddPart("end", "function", estr);
        fcon.SetType(FConstruct::ctFunction);
    }
    else if (fct == FConstruct::ctSubroutine)
    {
        fcon.AddPart("subroutine", estr, estr);
        fcon.AddPart("end", "subroutine", estr);
        fcon.SetType(FConstruct::ctSubroutine);
    }
    else if (fct == FConstruct::ctProgram)
    {
        fcon.AddPart("program", estr, estr);
        fcon.AddPart("end", "program", estr);
        fcon.SetType(FConstruct::ctProgram);
    }
    else if (fct == FConstruct::ctModule)
    {
        fcon.AddPart("module", estr, estr);
        fcon.AddPart("end", "module", estr);
        fcon.SetType(FConstruct::ctModule);
    }
    else if (fct == FConstruct::ctSubmodule)
    {
        fcon.AddPart("submodule", estr, estr);
        fcon.AddPart("end", "submodule", estr);
        fcon.SetType(FConstruct::ctSubmodule);
    }
    else if (fct == FConstruct::ctType)
    {
        fcon.AddPart("type", estr, estr);
        fcon.AddPart("end", "type", estr);
        fcon.SetType(FConstruct::ctType);
    }
    else if (fct == FConstruct::ctEnum)
    {
        fcon.AddPart("enum", estr, estr);
        fcon.AddPart("end", "enum", estr);
        fcon.SetType(FConstruct::ctEnum);
    }
    else if (fct == FConstruct::ctCritical)
    {
        fcon.AddPart("critical", estr, estr);
        fcon.AddPart("end", "critical", estr);
        fcon.SetType(FConstruct::ctCritical);
    }
    else if (fct == FConstruct::ctForall)
    {
        fcon.AddPart("forall", estr, estr);
        fcon.AddPart("end", "forall", estr);
        fcon.SetType(FConstruct::ctForall);
    }
    else if (fct == FConstruct::ctAssiciate)
    {
        fcon.AddPart("associate", estr, estr);
        fcon.AddPart("end", "associate", estr);
        fcon.SetType(FConstruct::ctAssiciate);
    }
    else if (fct == FConstruct::ctBlock)
    {
        fcon.AddPart("block", estr, estr);
        fcon.AddPart("end", "block", estr);
        fcon.SetType(FConstruct::ctBlock);
    }
    else if (fct == FConstruct::ctTeam)
    {
        fcon.AddPart("change", "team", estr);
        fcon.AddPart("end", "team", estr);
        fcon.SetType(FConstruct::ctTeam);
    }
    else if (fct == FConstruct::ctWhere)
    {
        fcon.AddPart("where", estr, estr);
        fcon.AddPart("else", "where", estr);
        fcon.AddPart("end", "where", estr);
        fcon.SetType(FConstruct::ctWhere);
    }
    else if (fct == FConstruct::ctBlockdata)
    {
        fcon.AddPart("block", "data", estr);
        fcon.AddPart("end", "block", "data");
        fcon.SetType(FConstruct::ctBlockdata);
    }
    else if (fct == FConstruct::ctProcedure)
    {
        fcon.AddPart("module", "procedure", estr);
        fcon.AddPart("end", "procedure", estr);
        fcon.SetType(FConstruct::ctProcedure);
    }
}

void ConstrHighlighter::GetSearchDirection(const Keyword &word1, const Keyword &word2, const Keyword &word3,
                                         SearchDirection& sdir, FConstruct& unFconBack, FConstruct& unFconForth)
{
    sdir = sdirUnknown;
    unFconBack.Clear();
    unFconForth.Clear();
    wxString str = word1.word + word2.word + word3.word;
    if (m_FConstructTypeMap.count(str) == 0)
        return;

    FConstruct::FConstructType fct = m_FConstructTypeMap[str];
    wxString estr;

    if (fct == FConstruct::ctProgramGroup)
    {
        if (str.IsSameAs("end") || str.IsSameAs("endfunction") || str.IsSameAs("endsubroutine") ||
            str.IsSameAs("endprogram") || str.IsSameAs("endmodule") || str.IsSameAs("endsubmodule") ||
            str.IsSameAs("endblockdata") || str.IsSameAs("endprocedure"))
        {
            sdir = sdirBack;
        }
        else if (str.IsSameAs("function") || str.IsSameAs("subroutine") || str.IsSameAs("program") ||
                 str.IsSameAs("module") || str.IsSameAs("submodule") || str.IsSameAs("blockdata") ||
                 str.IsSameAs("moduleprocedure"))
        {
            sdir = sdirForth;
        }
    }
    else if (fct== FConstruct::ctSelectGroup)
    {
        if (str.IsSameAs("selectcase") || str.IsSameAs("selecttype") || str.IsSameAs("selectrank"))
        {
            sdir = sdirForth;
            unFconForth.AddPart("case", estr, estr);
            unFconForth.AddPart("case", "default", estr);
            unFconForth.AddPart("type", "is", estr);
            unFconForth.AddPart("class", "is", estr);
            unFconForth.AddPart("class", "default", estr);
            unFconForth.AddPart("rank", estr, estr);
            unFconForth.AddPart("rank", "default", estr);
            unFconForth.AddPart("end", "select", estr);
        }
        else if (str.IsSameAs("endselect"))
        {
            sdir = sdirBack;
            unFconBack.AddPart("select", "case", estr);
            unFconBack.AddPart("case", estr, estr);
            unFconBack.AddPart("case", "default", estr);
            unFconBack.AddPart("type", "is", estr);
            unFconBack.AddPart("class", "is", estr);
            unFconBack.AddPart("class", "default", estr);
            unFconBack.AddPart("select", "rank", estr);
            unFconBack.AddPart("rank", estr, estr);
            unFconBack.AddPart("rank", "default", estr);
        }
        else if (str.IsSameAs("case") || str.IsSameAs("casedefault"))
        {
            sdir = sdirBackForth;
            unFconForth.AddPart("case", estr, estr);
            unFconForth.AddPart("case", "default", estr);
            unFconForth.AddPart("end", "select", estr);

            unFconBack.AddPart("select", "case", estr);
            unFconBack.AddPart("case", estr, estr);
        }
        else if (str.IsSameAs("typeis") || str.IsSameAs("classis") || str.IsSameAs("classdefault"))
        {
            sdir = sdirBackForth;
            unFconForth.AddPart("type", "is", estr);
            unFconForth.AddPart("class", "is", estr);
            unFconForth.AddPart("class", "default", estr);
            unFconForth.AddPart("end", "select", estr);

            unFconBack.AddPart("select", "type", estr);
            unFconBack.AddPart("type", "is", estr);
            unFconBack.AddPart("class", "is", estr);
            unFconBack.AddPart("class", "default", estr);
        }
        else if (str.IsSameAs("rank") || str.IsSameAs("rankdefault"))
        {
            sdir = sdirBackForth;
            unFconForth.AddPart("rank", estr, estr);
            unFconForth.AddPart("rank", "default", estr);
            unFconForth.AddPart("end", "select", estr);

            unFconBack.AddPart("select", "rank", estr);
            unFconBack.AddPart("rank", estr, estr);
        }
    }
    else if (fct == FConstruct::ctIf)
    {
        if (str == "ifthen" || str == "elseifthen")
        {
            sdir = sdirForth;
            unFconForth.AddPart("else", "if", "then");
            unFconForth.AddPart("else", estr, estr);
            unFconForth.AddPart("end", "if", estr);

            if (str == "elseifthen")
            {
                sdir = sdirBackForth;
                unFconBack.AddPart("if", "then", estr);
                unFconBack.AddPart("else", "if", "then");
            }
        }
        else if (str == "else")
        {
            sdir = sdirBackForth;
            unFconForth.AddPart("end", "if", estr);

            unFconBack.AddPart("if", "then", estr);
            unFconBack.AddPart("else", "if", "then");
        }
        else if (str == "endif")
        {
            sdir = sdirBack;
            unFconBack.AddPart("if", "then", estr);
            unFconBack.AddPart("else", "if", "then");
            unFconBack.AddPart("else", estr, estr);
        }
    }
    else if (fct == FConstruct::ctDo)
    {
        if (str.StartsWith("do"))
        {
            sdir = sdirForth;
            unFconForth.AddPart("end", "do", estr);
        }
        else if (str == "enddo")
        {
            sdir = sdirBack;
            unFconBack.AddPart("do", estr, estr);
        }
    }
    else if (fct == FConstruct::ctInterface)
    {
        if (str == "interface" || str == "abstractinterface")
        {
            sdir = sdirForth;
            unFconForth.AddPart("end", "interface", estr);
        }
        else if (str == "endinterface")
        {
            sdir = sdirBack;
            unFconBack.AddPart("interface", estr, estr);
        }
    }
    else if (fct == FConstruct::ctType)
    {
        if (str == "type")
        {
            sdir = sdirForth;
            unFconForth.AddPart("end", "type", estr);
        }
        else if (str == "endtype")
        {
            sdir = sdirBack;
            unFconBack.AddPart("type", estr, estr);
        }
    }
    else if (fct == FConstruct::ctEnum)
    {
        if (str == "enum")
        {
            sdir = sdirForth;
            unFconForth.AddPart("end", "enum", estr);
        }
        else if (str == "endenum")
        {
            sdir = sdirBack;
            unFconBack.AddPart("enum", estr, estr);
        }
    }
    else if (fct == FConstruct::ctCritical)
    {
        if (str == "critical")
        {
            sdir = sdirForth;
            unFconForth.AddPart("end", "critical", estr);
        }
        else if (str == "endcritical")
        {
            sdir = sdirBack;
            unFconBack.AddPart("critical", estr, estr);
        }
    }
    else if (fct == FConstruct::ctForall)
    {
        if (str == "forall")
        {
            sdir = sdirForth;
            unFconForth.AddPart("end", "forall", estr);
        }
        else if (str == "endforall")
        {
            sdir = sdirBack;
            unFconBack.AddPart("forall", estr, estr);
        }
    }
    else if (fct == FConstruct::ctAssiciate)
    {
        if (str == "associate")
        {
            sdir = sdirForth;
            unFconForth.AddPart("end", "associate", estr);
        }
        else if (str == "endassociate")
        {
            sdir = sdirBack;
            unFconBack.AddPart("associate", estr, estr);
        }
    }
    else if (fct == FConstruct::ctBlock)
    {
        if (str == "block")
        {
            sdir = sdirForth;
            unFconForth.AddPart("end", "block", estr);
        }
        else if (str == "endblock")
        {
            sdir = sdirBack;
            unFconBack.AddPart("block", estr, estr);
        }
    }
    else if (fct == FConstruct::ctTeam)
    {
        if (str == "changeteam")
        {
            sdir = sdirForth;
            unFconForth.AddPart("end", "team", estr);
        }
        else if (str == "endteam")
        {
            sdir = sdirBack;
            unFconBack.AddPart("change", "team", estr);
        }
    }
    else if (fct == FConstruct::ctWhere)
    {
        if (str == "where")
        {
            sdir = sdirForth;
            unFconForth.AddPart("else", "where", estr);
            unFconForth.AddPart("end", "where", estr);
        }
        else if (str == "endwhere")
        {
            sdir = sdirBack;
            unFconBack.AddPart("where", estr, estr);
            unFconBack.AddPart("else", "where", estr);
        }
        else if (str == "elsewhere")
        {
            sdir = sdirBackForth;
            unFconBack.AddPart("where", estr, estr);
            unFconBack.AddPart("else", "where", estr);

            unFconForth.AddPart("else", "where", estr);
            unFconForth.AddPart("end", "where", estr);
        }
    }
}

void ConstrHighlighter::GetFortranLine(cbStyledTextCtrl* control, int pos, wxString& fLine, int& lineStartPos, int& lineEndPos)
{
    bool tryBack;
    bool withContinuation;
    fLine = GetFortranLine2(control, pos, lineStartPos, lineEndPos, tryBack, withContinuation, true);
    if (m_CurrentSForm == fsfFree)
    {
        while (tryBack)
        {
            if (control->LineFromPosition(lineStartPos) == 0)
                break;
            pos = lineStartPos - 1;
            int lep;
            int lsBackPos;
            wxString lineBack = GetFortranLine2(control, pos, lsBackPos, lep, tryBack, withContinuation, false);
            if (withContinuation)
            {
                lineStartPos = lsBackPos;
                fLine.Prepend(lineBack);
            }
            else
                break;
        }
    }
    else // (m_CurrentSForm == fsfFixed)
    {
        while (tryBack && withContinuation)
        {
            if (control->LineFromPosition(lineStartPos) == 0)
                break;
            pos = lineStartPos - 1;
            int lep;
            int lsBackPos;
            wxString lineBack = GetFortranLine2(control, pos, lsBackPos, lep, tryBack, withContinuation, false);
            lineStartPos = lsBackPos;
            fLine.Prepend(lineBack);
        }
    }
    fLine = CutBracketsLevel2(fLine);
    fLine.Replace("\t", " ");
    fLine.Trim();
}

wxString ConstrHighlighter::GetFortranLine2(cbStyledTextCtrl* control, int posC, int& posStart, int& posEnd, bool& tryBack,
                                            bool& withContinuation, bool goForth)
{
    int line = control->LineFromPosition(posC);
    wxString lineStr = control->GetLine(line);
    posStart = control->PositionFromLine(line);
    posEnd   = control->PositionFromLine(line+1);
    tryBack = true;
    withContinuation = false;
    bool inStr1 = false;
    bool inStr2 = false;
    bool spaceOnly = true;
    int lsl = lineStr.length();
    if (m_CurrentSForm == fsfFixed)
    {
        if (lsl <= 6)
            return wxEmptyString;
        const wxChar cch = lineStr.GetChar(0);
        if (cch == 'C' || cch == 'c' || cch == '*' || cch == '!')
            return wxEmptyString; // comment line in fixed format file
        if (lineStr.GetChar(5) != ' ' && lineStr.GetChar(5) != '0')
            withContinuation = true;
    }
    int i = 0;
    while (i < lsl)
    {
        if (inStr1 && lineStr.GetChar(i) == '\'')
            inStr1 = false;
        else if (inStr2 && lineStr.GetChar(i) == '"')
            inStr2 = false;
        else if (inStr1 || inStr2)
            lineStr.SetChar(i, ' ');
        else if (lineStr.GetChar(i) == '\'')
        {
            inStr1 = true;
            spaceOnly = false;
        }
        else if (lineStr.GetChar(i) == '"')
        {
            inStr2 = true;
            spaceOnly = false;
        }
        else if (lineStr.GetChar(i) == '!')
        {
            if (posStart + i >= posC)
            {
                lineStr = lineStr.Mid(0, i);
                posEnd = posStart + i;
                break;
            }
            else
            {
                lineStr = "";
                break;
            }
        }
        else if (lineStr.GetChar(i) == '&')
        {
            lineStr.SetChar(i, ' ');
            if (!spaceOnly && (m_CurrentSForm == fsfFree))
            {
                lineStr = lineStr.Mid(0, i+1);

                if (goForth && line+1 < control->GetLineCount())
                {
                    int posStart2;
                    bool tback, econ;
                    lineStr.Append(GetFortranLine2(control, control->PositionFromLine(line+1), posStart2, posEnd, tback, econ, true));
                }
                else
                    withContinuation = true;
                break;
            }
        }
        else if (lineStr.GetChar(i) == ';')
        {
            if (posStart + i >= posC)
            {
                lineStr = lineStr.Mid(0, i);
                posEnd = posStart + i;
                break;
            }
            else
            {
                posStart = posStart + i + 1;
                lineStr = lineStr.Mid(i+1);
                lsl = lineStr.length();
                i = -1;
                tryBack = false;
                spaceOnly = false;
            }
        }
        else if (spaceOnly)
            spaceOnly = false;
        i++;
    }

    if (m_CurrentSForm == fsfFixed)
    {
        if (goForth && line+1 < control->GetLineCount())
        {
            int posStart2;
            bool tback;
            bool wCon;
            wxString nextLine = GetFortranLine2(control, control->PositionFromLine(line+1), posStart2, posEnd, tback, wCon, true);
            if (wCon)
                lineStr.Append(nextLine);
        }
    }
    return lineStr;
}

wxString ConstrHighlighter::CutBracketsLevel2(const wxString& str)
{
    wxString retStr = str;
    int pos = str.Find('(');
    if (pos == wxNOT_FOUND)
        return retStr;

    std::vector<int> cutStart;
    std::vector<int> cutEnd;
    int level = 0;
    for (size_t i=pos; i<str.Len(); i++)
    {
        if (str.GetChar(i) == '(')
        {
            if (level == 1)
            {
                cutStart.push_back(i);
                cutEnd.push_back(wxString::npos);
            }
            level++;
        }
        else if (str.GetChar(i) == ')')
        {
            if (level == 2)
                cutEnd[cutEnd.size()-1] = i;
            level--;
        }
    }

    for (int i=int(cutStart.size())-1; i>=0; i--)
    {
        if (cutEnd[i] != int(wxString::npos))
            retStr = retStr.Mid(0,cutStart[i]) + " " + retStr.Mid(cutEnd[i]+1);
        else
            retStr = retStr.Mid(0,cutStart[i]) + " ";
    }
    return retStr;
}

int ConstrHighlighter::FindLimitPos(cbStyledTextCtrl* control, const FConstruct& fcon, int posStart, SearchDirection sdir)
{
    int pEnd = (sdir == sdirBack) ? 0 : control->GetLength();
    int rpEnd = pEnd;
    FConstruct::FConstructType ct = fcon.GetType();
    if (ct == FConstruct::ctIf || ct == FConstruct::ctDo || ct == FConstruct::ctSelectGroup ||
        ct == FConstruct::ctCritical || ct == FConstruct::ctForall || ct == FConstruct::ctEnum ||
        ct == FConstruct::ctType || ct == FConstruct::ctAssiciate || ct == FConstruct::ctBlock ||
        ct == FConstruct::ctTeam )
    {
        wxString str1, str2, str3;
        int str1Pos, str2Pos, str3Pos;

        if (sdir == sdirBack)
        {
            wxString word1 = "subroutine";
            wxString word2;
            wxString word3;
            FConstruct::FCLid flid1 = FConstruct::fclSub_sub;
            int cp1 = FindFKeyword(control, posStart, pEnd, flid1, word1, word2, word3,
                                          str1, str1Pos, str2, str2Pos, str3, str3Pos);
            if (cp1 == wxSCI_INVALID_POSITION)
                cp1 = rpEnd;
            else
                pEnd = cp1 + 10;

            word1 = "function";
            word2 = wxEmptyString;
            word3 = wxEmptyString;
            flid1 = FConstruct::fclFun_fun;
            int cp2 = FindFKeyword(control, posStart, pEnd, flid1, word1, word2, word3,
                                          str1, str1Pos, str2, str2Pos, str3, str3Pos);
            if (cp2 == wxSCI_INVALID_POSITION)
                cp2 = rpEnd;
            else
                pEnd = cp2 + 8;

            word1 = "program";
            word2 = wxEmptyString;
            word3 = wxEmptyString;
            flid1 = FConstruct::fclProg_prog;
            int cp3 = FindFKeyword(control, posStart, pEnd, flid1, word1, word2, word3,
                                          str1, str1Pos, str2, str2Pos, str3, str3Pos);
            if (cp3 == wxSCI_INVALID_POSITION)
                cp3 = rpEnd;

            return std::max(std::max(cp1,cp2),cp3);
        }
        else
        {
            wxString word1 = "end";
            wxString word2 = "subroutine";
            wxString word3;
            FConstruct::FCLid flid1 = FConstruct::fclSub_end_sub;
            int cp1 = FindFKeyword(control, posStart, pEnd, flid1, word1, word2, word3,
                                          str1, str1Pos, str2, str2Pos, str3, str3Pos);
            if (cp1 == wxSCI_INVALID_POSITION)
                cp1 = rpEnd;
            else
                pEnd = cp1;

            word1 = "end";
            word2 = "function";
            word3 = wxEmptyString;
            flid1 = FConstruct::fclFun_fun;
            int cp2 = FindFKeyword(control, posStart, pEnd, flid1, word1, word2, word3,
                                          str1, str1Pos, str2, str2Pos, str3, str3Pos);
            if (cp2 == wxSCI_INVALID_POSITION)
                cp2 = rpEnd;
            else
                pEnd = cp2;

            word1 = "end";
            word2 = "program";
            word3 = wxEmptyString;
            flid1 = FConstruct::fclProg_prog;
            int cp3 = FindFKeyword(control, posStart, pEnd, flid1, word1, word2, word3,
                                          str1, str1Pos, str2, str2Pos, str3, str3Pos);
            if (cp3 == wxSCI_INVALID_POSITION)
                cp3 = rpEnd;

            return std::min(std::min(cp1,cp2),cp3);
        }
    }
    else if (ct == FConstruct::ctProgramGroup || ct == FConstruct::ctInterface)
    {
        wxString str1, str2, str3;
        int str1Pos, str2Pos, str3Pos;

        if (sdir == sdirBack)
        {
            wxString word1 = "module";
            wxString word2;
            wxString word3;
            FConstruct::FCLid flid1 = FConstruct::fclMod_module;
            int cp1 = FindFKeyword(control, posStart, pEnd, flid1, word1, word2, word3,
                                          str1, str1Pos, str2, str2Pos, str3, str3Pos);
            if (cp1 == wxSCI_INVALID_POSITION)
                cp1 = rpEnd;
            else
                pEnd = cp1 + 6;

            word1 = "end";
            word2 = "module";
            word3 = wxEmptyString;
            flid1 = FConstruct::fclMod_end_module;
            int cp2 = FindFKeywordFull(control, posStart, pEnd, flid1, word1, word2, word3,
                                          str1, str1Pos, str2, str2Pos, str3, str3Pos);
            if (cp2 == wxSCI_INVALID_POSITION)
                cp2 = rpEnd;
            else
                pEnd = cp2 + 3;

            word1 = "submodule";
            word2 = wxEmptyString;
            word3 = wxEmptyString;
            flid1 = FConstruct::fclSubmod_submod;
            int cp3 = FindFKeyword(control, posStart, pEnd, flid1, word1, word2, word3,
                                          str1, str1Pos, str2, str2Pos, str3, str3Pos);
            if (cp3 == wxSCI_INVALID_POSITION)
                cp3 = rpEnd;

            return std::max(std::max(cp1,cp2),cp3);
        }
        else
        {
            wxString word1 = "end";
            wxString word2 = "module";
            wxString word3;
            FConstruct::FCLid flid1 = FConstruct::fclMod_end_module;

            int cp1 = FindFKeywordFull(control, posStart, pEnd, flid1, word1, word2, word3,
                                          str1, str1Pos, str2, str2Pos, str3, str3Pos);
            if (cp1 != wxSCI_INVALID_POSITION)
            {
                pEnd = cp1;
                cp1 = GetWordsEnd(str1, str1Pos, str2, str2Pos, str3, str3Pos);
            }

            word1 = "module";
            word2 = wxEmptyString;
            word3 = wxEmptyString;
            flid1 = FConstruct::fclMod_module;

            int cp2 = FindFKeyword(control, posStart, pEnd, flid1, word1, word2, word3,
                                          str1, str1Pos, str2, str2Pos, str3, str3Pos);
            if (cp2 != wxSCI_INVALID_POSITION)
                pEnd = cp2;
            word1 = "end";
            word2 = "submodule";
            word3 = wxEmptyString;
            flid1 = FConstruct::fclSubmod_end_submod;

            int cp3 = FindFKeywordFull(control, posStart, pEnd, flid1, word1, word2, word3,
                                          str1, str1Pos, str2, str2Pos, str3, str3Pos);
            if (cp3 != wxSCI_INVALID_POSITION)
                cp3 = GetWordsEnd(str1, str1Pos, str2, str2Pos, str3, str3Pos);
            if (cp1 == wxSCI_INVALID_POSITION)
                cp1 = rpEnd;
            if (cp2 == wxSCI_INVALID_POSITION)
                cp2 = rpEnd;
            if (cp3 == wxSCI_INVALID_POSITION)
                cp3 = rpEnd;

            return std::min(std::min(cp1,cp2),cp3);
        }
    }
    return rpEnd;
}

int ConstrHighlighter::GetConstructStartPos(cbEditor* editor, FortranSourceForm fsForm)
{
    //
    // Returns start position of a construct, which has end before the current position.
    // Function is assumed to be called after user pressed 'Enter'.
    //
    int constStartPos = -1;
    cbStyledTextCtrl* control = editor->GetControl();

    m_TimeMax = 50;
    m_Watch.Start();

    int nline = control->GetCurrentLine() - 1;
    if (nline < 2) return constStartPos;

    int lineStartPos = control->PositionFromLine(nline);
    int lineEndPos = control->GetLineEndPosition(nline);

    // Skip spaces
    int posWStart = -1;
    for (int i=lineStartPos; i<lineEndPos; i++)
    {
        const wxChar cch = control->GetCharAt(i);
        if (!isspace(cch))
        {
            posWStart = i;
            break;
        }
    }
    if (posWStart == -1) return constStartPos;
    int style = control->GetStyleAt(posWStart);
    if (style != wxSCI_F_WORD)
        return constStartPos;
    int posWEnd = control->WordEndPosition(posWStart, true);
    wxString cword = control->GetTextRange(posWStart,posWEnd).Lower();
    if (!cword.StartsWith("end") && !cword.StartsWith("else"))
        return constStartPos;
    else if (m_KeywordSet.count(cword) == 0)
        return constStartPos;

    Keyword word1;
    Keyword word2;
    Keyword word3;
    FConstruct::FCLid flid;
    bool foundFull;

    std::vector<FConstruct::FCLid> flidAll;
    flidAll = FConstruct::WordFCLidMap[cword];
    if (flidAll.empty())
        return constStartPos;
    bool containsK = false;
    int flineStartPos;
    int flineEndPos;
    wxString fLine;
    GetFortranLine(control, posWStart, fLine, flineStartPos, flineEndPos);
    for (size_t i=0; i<flidAll.size(); i++)
    {
        if (FConstruct::FCLReMap.count(flidAll[i]) == 0)
            continue;

        bool match = false;
        if (flidAll[i] == FConstruct::fclBlock_end_block)
        {
            if ( FConstruct::FCLReMap[FConstruct::fclBlock_end_block]->Matches(fLine) &&
                !FConstruct::FCLReMap[FConstruct::fclBlockdata_end_blockdata]->Matches(fLine))
                    match = true;
        }
        else if (FConstruct::FCLReMap[flidAll[i]]->Matches(fLine))
        {
            match = true;
        }

        if (match)
        {
            wxString wstr1, wstr2, wstr3;
            FConstruct::GetWordsFromFCLid(flidAll[i], wstr1, wstr2, wstr3);

            wxString str1, str2, str3;
            int str1Pos, str2Pos, str3Pos;
            int pos = FindFKeyword(control, flineStartPos, control->GetLength(), flidAll[i], wstr1, wstr2, wstr3,
                                   str1, str1Pos, str2, str2Pos, str3, str3Pos);
            if (m_Watch.Time() > m_TimeMax)
                return constStartPos;
            word1.word = str1;
            word1.posStart = str1Pos;
            word2.word = str2;
            word2.posStart = str2Pos;
            word3.word = str3;
            word3.posStart = str3Pos;

            if ((str1 + str2 + str3) == "end")
                flid = FConstruct::fclProgGroup_end;
            else
                flid = flidAll[i];

            if (pos == wxSCI_INVALID_POSITION)
                containsK = false;
            else
                containsK = true;
            break;
        }
    }
    if (!containsK)
        return constStartPos;

    KeywordList myPairs;
    FindMyPairs(control, word1, word2, word3, flid, myPairs, foundFull);

    if (!foundFull)
        return constStartPos;

    // get minimal position
    int minPos = -1;
    for (auto myP : myPairs)
	{
	    if (minPos == -1)
            minPos = myP.posStart;

	    minPos = (myP.posStart < minPos) ? myP.posStart : minPos;
	}
	if (minPos != -1)
        constStartPos = minPos;

    return constStartPos;
}



