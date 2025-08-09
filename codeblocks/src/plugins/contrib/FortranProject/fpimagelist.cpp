
#include "fpimagelist.h"

#include <sdk.h>
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
    m_ImSize = imSize;
    static const std::string images[41][2] =
    {
    {"ctor_public", "use"},
    {"class_public", "module"},
    {"method_public", "subroutine"},
    {"method_protected", "function"},
    {"method_private", "program"},
    {"typedef", "type"},
    {"interface", "interface"},
    {"funcs_folder", "function_folder"},
    {"others_folder", "others_folder"},
    {"symbols_folder", "symbols_folder"},
    {"preproc", "preproc"},
    {"var_public", "variable"},
    {"interface_function", "interface_function"},
    {"interface_subroutine", "interface_subroutine"},
    {"procedure", "procedure"},
    {"subroutine_private", "subroutine_private"},
    {"function_private", "function_private"},
    {"var_private", "variable_private"},
    {"var_protected", "variable_protected"},
    {"typedef_private", "type_private"},
    {"interface_private", "interface_private"},
    {"access_list_private", "access_list_private"},
    {"access_list_public", "access_list_public"},
    {"access_list_protected", "access_list_protected"},
    {"procedure_private", "procedure_private"},
    {"interface_function_private", "interface_function_private"},
    {"interface_subroutine_private", "interface_subroutine_private"},
    {"class", "submodule"},
    {"interface_subroutine_gen_private", "interface_sub_gen_private"},
    {"interface_subroutine_gen", "interface_sub_gen"},
    {"interface_function_gen_private", "interface_fun_gen_private"},
    {"interface_function_gen", "interface_fun_gen"},
    {"typedef_abstract", "type_abstract"},
    {"typedef_abstract_private", "type_abstract_private"},
    {"dtor_public", "procedure_final"},
    {"subroutine_call", "subroutine_call"},
    {"function_call", "function_call"},
    {"subroutine_calledby", "subroutine_calledby"},
    {"function_calledby", "function_calledby"},
    {"typedefs_folder", "typedefs_folder"},
    {"unknown", "unknown"}
    };

    imSize = cbFindMinSize16to64(imSize);
    m_pImlist = new wxImageList(imSize, imSize);

    wxString prefix(ConfigManager::GetDataFolder() + "/FortranProject.zip#zip:/images/fortranproject/");
#if wxCHECK_VERSION(3, 1, 6)
    prefix << "svg/";
#else
    prefix << wxString::Format("%dx%d/", imSize, imSize);
#endif

    for (int i = 0; i < 41; ++i)
    {
#if wxCHECK_VERSION(3, 1, 6)
        wxBitmap bmp = cbLoadBitmapBundleFromSVG(prefix + images[i][0] + ".svg", wxSize(imSize, imSize)).GetBitmap(wxDefaultSize);
#else
        wxBitmap bmp = cbLoadBitmap(prefix + images[i][0] + ".png");
#endif
        m_pImlist->Add(bmp);
        m_ImgNr[images[i][1]] = i;
    }

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
                if (token->m_TypeDefinition.IsSameAs("subroutine"))
                {
                    if (token->m_TokenAccess == taPrivate)
                        return m_ImgNr["interface_sub_gen_private"];
                    else
                        return m_ImgNr["interface_sub_gen"];
                }
                else if (token->m_TypeDefinition.IsSameAs("function"))
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

        case tkMacroDefine: return m_ImgNr["preproc"];

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
                if (token->m_TypeDefinition.IsSameAs("subroutine"))
                {
                    if (token->m_TokenAccess == taPrivate)
                        return m_ImgNr["subroutine_private"];
                    else
                        return m_ImgNr["subroutine"];
                }
                else if (token->m_TypeDefinition.IsSameAs("function"))
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
