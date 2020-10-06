
#include "fpimagelist.h"

#ifndef CB_PRECOMP
    #include <wx/string.h>
    #include <wx/bitmap.h>

    #include <configmanager.h>
#endif

FPImageList::FPImageList(int imSize)
{
    //ctor
    CreateImageList(imSize);
}

FPImageList::~FPImageList()
{
    //dtor
    delete m_pImlist;
}

void FPImageList::CreateImageList(int imSize)
{
    wxString prefix2;
    if (imSize <= 19)
    {
        m_pImlist = new wxImageList(16, 16);
        prefix2 = ConfigManager::GetDataFolder() + _T("/images/fortranproject/16x16/");
    }
    else if (imSize <= 23)
    {
        m_pImlist = new wxImageList(20, 20);
        prefix2 = ConfigManager::GetDataFolder() + _T("/images/fortranproject/20x20/");
    }
    else if (imSize <= 27)
    {
        m_pImlist = new wxImageList(24, 24);
        prefix2 = ConfigManager::GetDataFolder() + _T("/images/fortranproject/24x24/");
    }
    else if (imSize <= 31)
    {
        m_pImlist = new wxImageList(28, 28);
        prefix2 = ConfigManager::GetDataFolder() + _T("/images/fortranproject/28x28/");
    }
    else if (imSize <= 39)
    {
        m_pImlist = new wxImageList(32, 32);
        prefix2 = ConfigManager::GetDataFolder() + _T("/images/fortranproject/32x32/");
    }
    else if (imSize <= 47)
    {
        m_pImlist = new wxImageList(40, 40);
        prefix2 = ConfigManager::GetDataFolder() + _T("/images/fortranproject/40x40/");
    }
    else if (imSize <= 55)
    {
        m_pImlist = new wxImageList(48, 48);
        prefix2 = ConfigManager::GetDataFolder() + _T("/images/fortranproject/48x48/");
    }
    else if (imSize <= 63)
    {
        m_pImlist = new wxImageList(56, 56);
        prefix2 = ConfigManager::GetDataFolder() + _T("/images/fortranproject/56x56/");
    }
    else
    {
        m_pImlist = new wxImageList(64, 64);
        prefix2 = ConfigManager::GetDataFolder() + _T("/images/fortranproject/64x64/");
    }
    wxBitmap bmp;

    bmp = cbLoadBitmap(prefix2 + _T("ctor_public.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["use"] = 0;
    bmp = cbLoadBitmap(prefix2 + _T("class_public.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["module"] = 1;
    bmp = cbLoadBitmap(prefix2 + _T("method_public.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["subroutine"] = 2;
    bmp = cbLoadBitmap(prefix2 + _T("method_protected.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["function"] = 3;
    bmp = cbLoadBitmap(prefix2 + _T("method_private.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["program"] = 4;
    bmp = cbLoadBitmap(prefix2 + _T("typedef.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["type"] = 5;
    bmp = cbLoadBitmap(prefix2 + _T("interface.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["interface"] = 6;
    bmp = cbLoadBitmap(prefix2 + _T("funcs_folder.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["function_folder"] = 7;
    bmp = cbLoadBitmap(prefix2 + _T("others_folder.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["others_folder"] = 8;
    bmp = cbLoadBitmap(prefix2 + _T("symbols_folder.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["symbols_folder"] = 9;
    bmp = cbLoadBitmap(prefix2 + _T("preproc.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["preproc"] = 10;
    bmp = cbLoadBitmap(prefix2 + _T("var_public.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["variable"] = 11;
    bmp = cbLoadBitmap(prefix2 + _T("interface_function.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["interface_function"] = 12;
    bmp = cbLoadBitmap(prefix2 + _T("interface_subroutine.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["interface_subroutine"] = 13;
    bmp = cbLoadBitmap(prefix2 + _T("procedure.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["procedure"] = 14;

    bmp = cbLoadBitmap(prefix2 + _T("subroutine_private.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["subroutine_private"] = 15;
    bmp = cbLoadBitmap(prefix2 + _T("function_private.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["function_private"] = 16;
    bmp = cbLoadBitmap(prefix2 + _T("var_private.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["variable_private"] = 17;
    bmp = cbLoadBitmap(prefix2 + _T("var_protected.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["variable_protected"] = 18;
    bmp = cbLoadBitmap(prefix2 + _T("typedef_private.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["type_private"] = 19;
    bmp = cbLoadBitmap(prefix2 + _T("interface_private.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["interface_private"] = 20;
    bmp = cbLoadBitmap(prefix2 + _T("access_list_private.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["access_list_private"] = 21;
    bmp = cbLoadBitmap(prefix2 + _T("access_list_public.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["access_list_public"] = 22;
    bmp = cbLoadBitmap(prefix2 + _T("access_list_protected.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["access_list_protected"] = 23;
    bmp = cbLoadBitmap(prefix2 + _T("procedure_private.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["procedure_private"] = 24;
    bmp = cbLoadBitmap(prefix2 + _T("interface_function_private.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["interface_function_private"] = 25;
    bmp = cbLoadBitmap(prefix2 + _T("interface_subroutine_private.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["interface_subroutine_private"] = 26;
    bmp = cbLoadBitmap(prefix2 + _T("class.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["submodule"] = 27;
    bmp = cbLoadBitmap(prefix2 + _T("interface_subroutine_gen_private.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["interface_sub_gen_private"] = 28;
    bmp = cbLoadBitmap(prefix2 + _T("interface_subroutine_gen.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["interface_sub_gen"] = 29;
    bmp = cbLoadBitmap(prefix2 + _T("interface_function_gen_private.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["interface_fun_gen_private"] = 30;
    bmp = cbLoadBitmap(prefix2 + _T("interface_function_gen.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["interface_fun_gen"] = 31;
    bmp = cbLoadBitmap(prefix2 + _T("typedef_abstract.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["type_abstract"] = 32;
    bmp = cbLoadBitmap(prefix2 + _T("typedef_abstract_private.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["type_abstract_private"] = 33;
    bmp = cbLoadBitmap(prefix2 + _T("dtor_public.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["procedure_final"] = 34;
    bmp = cbLoadBitmap(prefix2 + _T("subroutine_call.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["subroutine_call"] = 35;
    bmp = cbLoadBitmap(prefix2 + _T("function_call.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["function_call"] = 36;
    bmp = cbLoadBitmap(prefix2 + _T("subroutine_calledby.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["subroutine_calledby"] = 37;
    bmp = cbLoadBitmap(prefix2 + _T("function_calledby.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["function_calledby"] = 38;
    bmp = cbLoadBitmap(prefix2 + _T("typedefs_folder.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["typedefs_folder"] = 39;

    bmp = cbLoadBitmap(prefix2 + _T("unknown.png"), wxBITMAP_TYPE_PNG);
    m_pImlist->Add(bmp);
    m_ImgNr["unknown"] = 40;

    m_ImgNr["none"] = -1;
}

int FPImageList::GetImageIdx(const std::string& name)
{
    int idx;
    if (m_ImgNr.count(name) == 0)
        idx = m_ImgNr["none"];
    else
        idx = m_ImgNr[name];
    return idx;
}

int FPImageList::GetTokenKindImageIdx(TokenF* token)
{
    if (!token)
        return m_ImgNr["none"];

    switch (token->m_TokenKind)
    {
        case tkUse: return m_ImgNr["use"];

        case tkModule: return m_ImgNr["module"];

        case tkSubroutine:
            {
                if (token->m_TokenAccess == taPrivate)
                    return m_ImgNr["subroutine_private"];
                else
                    return m_ImgNr["subroutine"];
            }
        case tkFunction:
            {
                if (token->m_TokenAccess == taPrivate)
                    return m_ImgNr["function_private"];
                else
                    return m_ImgNr["function"];
            }
        case tkProgram: return m_ImgNr["program"];

        case tkType:
            {
                if (token->m_TokenAccess == taPrivate)
                {
                    if (token->m_IsAbstract)
                        return m_ImgNr["type_abstract_private"];
                    else
                        return m_ImgNr["type_private"];
                }
                else
                {
                    if (token->m_IsAbstract)
                        return m_ImgNr["type_abstract"];
                    else
                        return m_ImgNr["type"];
                }
            }
        case tkInterface:
            {
                if (token->m_TypeDefinition.IsSameAs(_T("subroutine")))
                {
                    if (token->m_TokenAccess == taPrivate)
                        return m_ImgNr["interface_sub_gen_private"];
                    else
                        return m_ImgNr["interface_sub_gen"];
                }
                else if (token->m_TypeDefinition.IsSameAs(_T("function")))
                {
                    if (token->m_TokenAccess == taPrivate)
                        return m_ImgNr["interface_fun_gen_private"];
                    else
                        return m_ImgNr["interface_fun_gen"];
                }
                else
                {
                    if (token->m_TokenAccess == taPrivate)
                        return m_ImgNr["interface_private"];
                    else
                        return m_ImgNr["interface"];
                }
            }
        case tkInterfaceExplicit:
            {
                if (token->m_TokenAccess == taPrivate)
                    return m_ImgNr["interface_private"];
                else
                    return m_ImgNr["interface"];
            }

        case tkCommonblock: return m_ImgNr["none"];

        case tkPreprocessor: return m_ImgNr["preproc"];

        case tkFile: return m_ImgNr["none"];

        case tkVariable:
            {
                if (token->m_TokenAccess == taPrivate)
                    return m_ImgNr["variable_private"];
                else if (token->m_TokenAccess == taProtected)
                    return m_ImgNr["variable_protected"];
                else
                    return m_ImgNr["variable"];
            }

        //case tkInterfaceFunction: return m_ImgNr["interface_function"];

        //case tkInterfaceSubroutine: return m_ImgNr["interface_subroutine"];

        case tkProcedure:
            {
                if (token->m_TypeDefinition.IsSameAs(_T("subroutine")))
                {
                    if (token->m_TokenAccess == taPrivate)
                        return m_ImgNr["subroutine_private"];
                    else
                        return m_ImgNr["subroutine"];
                }
                else if (token->m_TypeDefinition.IsSameAs(_T("function")))
                {
                    if (token->m_TokenAccess == taPrivate)
                        return m_ImgNr["function_private"];
                    else
                        return m_ImgNr["function"];
                }
                else
                {
                    if (token->m_TokenAccess == taPrivate)
                        return m_ImgNr["procedure_private"];
                    else
                        return m_ImgNr["procedure"];
                }
            }

        case tkAccessList:
            {
                if (token->m_TokenAccess == taPrivate)
                    return m_ImgNr["access_list_private"];
                else if (token->m_TokenAccess == taProtected)
                    return m_ImgNr["access_list_protected"];
                else
                    return m_ImgNr["access_list_public"];
            }

        case tkSubmodule: return m_ImgNr["submodule"];

        case tkProcedureFinal: return m_ImgNr["procedure_final"];

        default: return m_ImgNr["unknown"];
    }
}

