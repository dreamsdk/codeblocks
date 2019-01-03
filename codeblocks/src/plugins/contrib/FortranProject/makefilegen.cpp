
#include "makefilegen.h"
#include "makefiledlg.h"
#include "nativeparserf.h"
#include "cbproject.h"
#include "compiler.h"
#include <compilerfactory.h>
#include <wx/file.h>

void MakefileGen::GenerateMakefile(cbProject* project, ProjectDependencies* projDep, NativeParserF* pNativeParser)
{
    if (!project || !projDep)
        return;

    ProjectBuildTarget* buildTarget = project->GetBuildTarget(project->GetActiveBuildTarget());
    if (!buildTarget)
        return;

    wxString projDir = project->GetBasePath();
    wxFileName mffn = wxFileName(projDir,_T("Makefile"));
    if (!mffn.IsOk())
        return;
    if (!SelectMikefileName(mffn))
        return;

    wxFile mfile;
    if(!mfile.Create(mffn.GetFullPath(), true))
    {
        cbMessageBox(_T("Makefile can't be created!"), _("Error"), wxICON_ERROR);
        return;
    }

    mfile.Write(_T("#\n# This Makefile was generated by Code::Blocks IDE.\n#\n"));


    FilesList& filesList = buildTarget->GetFilesList();

    wxArrayString src_dirs;
    wxArrayString src_ext;
    wxArrayString src_files;
    wxArrayString obj_files;
    wxString sfiles;
    wxString ofiles;



    FilesList::iterator it;
    for( it = filesList.begin(); it != filesList.end(); ++it )
    {
        ProjectFile* projFile = *it;
        const pfDetails& pfd = projFile->GetFileDetails(buildTarget);

        wxFileName sfn(pfd.source_file_absolute_native);
        sfn.MakeRelativeTo(mffn.GetPath(wxPATH_GET_SEPARATOR));
        wxString dir = sfn.GetPath(wxPATH_GET_VOLUME | wxPATH_GET_SEPARATOR, wxPATH_UNIX);
        int didx = src_dirs.Index(dir);
        if (didx == wxNOT_FOUND)
            didx = src_dirs.Add(dir);
        wxString ext = sfn.GetExt();
        ext << _T("d") << didx+1;
        int eidx = src_ext.Index(ext);
        if (eidx == wxNOT_FOUND)
        {
            eidx = src_ext.Add(ext);
            sfiles = wxEmptyString;
            src_files.Add(wxEmptyString);
            ofiles = wxEmptyString;
            obj_files.Add(wxEmptyString);
        }
        else
        {
            sfiles = src_files.Item(eidx);
            ofiles = obj_files.Item(eidx);
        }

        sfiles << sfn.GetFullName() << _T(" \\\n");
        src_files[eidx] = sfiles;

        if (projFile->compile)
        {
            ofiles << sfn.GetName() << _T(".o") << _T(" \\\n");
            obj_files[eidx] = ofiles;
        }
    }
    for (size_t i=0; i<src_ext.Count(); i++)
    {
        wxString str;
        str << _T("\nSRCS_") << src_ext.Item(i) << _T(" = \\\n");
        mfile.Write(str);
        mfile.Write(src_files[i].Mid(0,src_files[i].Find('\\', true)));
        mfile.Write(_T("\n"));
    }
    for (size_t i=0; i<src_ext.Count(); i++)
    {
        wxString str;
        str << _T("\nOBJS_") << src_ext.Item(i) << _T(" = \\\n");
        mfile.Write(str);
        mfile.Write(obj_files[i].Mid(0,obj_files[i].Find('\\', true)));
        mfile.Write(_T("\n"));
    }

    wxString objdir = _T("OBJS_DIR = ");

    for( it = filesList.begin(); it != filesList.end(); ++it )
    {
        ProjectFile* projFile = *it;
        const pfDetails& pfd = projFile->GetFileDetails(buildTarget);

        wxFileName sfn(pfd.source_file_absolute_native);
        wxString ffpath = sfn.GetFullName();
        if (pNativeParser->IsFileFortran(ffpath) && projFile->compile)
        {
            wxFileName ofn(pfd.object_file_absolute_native);
            ofn.MakeRelativeTo(mffn.GetPath(wxPATH_GET_SEPARATOR));
            ofn.SetExt(_T("o"));
            objdir << ofn.GetPath(wxPATH_GET_VOLUME | wxPATH_GET_SEPARATOR, wxPATH_UNIX) + _T("\n");
            break;
        }
    }

    bool containsNonFortranFiles = false;

    wxString depsFiles;
    for( it = filesList.begin(); it != filesList.end(); ++it )
    {
        ProjectFile* projFile = *it;
        const pfDetails& pfd = projFile->GetFileDetails(buildTarget);

        if (pNativeParser->IsFileFortran(pfd.source_file) && projFile->compile)
        {
            wxFileName sfn(pfd.source_file);
            sfn.SetExt(_T("o"));
            depsFiles << sfn.GetFullName();
            wxFileName sfn2(pfd.source_file);
            depsFiles << _T(": \\\n    ") << sfn2.GetFullName();
            wxArrayString use;
            projDep->GetUseFilesFile(pfd.source_file_absolute_native, use);
            for (size_t i=0; i<use.size(); i++)
            {
                wxFileName ufn(use.Item(i));
                ufn.SetExt(_T("o"));
                depsFiles << _T(" \\\n    ") << ufn.GetFullName();
            }

            wxArrayString extends;
            projDep->GetExtendsFilesFile(pfd.source_file_absolute_native, extends);
            for (size_t i=0; i<extends.size(); i++)
            {
                wxFileName ufn(extends.Item(i));
                ufn.SetExt(_T("o"));
                depsFiles << _T(" \\\n    ") << ufn.GetFullName();
            }

            wxArrayString incl;
            projDep->GetIncludeFilesFile(pfd.source_file_absolute_native, incl);
            for (size_t i=0; i<incl.size(); i++)
            {
                wxFileName ifn(incl.Item(i));
                depsFiles << _T(" \\\n    ") << ifn.GetFullName();
            }
            depsFiles << _T("\n");
        }
        else if (!pNativeParser->IsFileFortran(pfd.source_file) && projFile->compile)
        {
            containsNonFortranFiles = true;
        }
    }

    for (size_t i=0; i<src_ext.Count(); i++)
    {
        wxString sdir;
        wxString ext = src_ext.Item(i);
        sdir << _T("\nSRC_DIR_") << ext << _T(" = ");
        int dpos = ext.Find('d', true);
        if (dpos != wxNOT_FOUND)
        {
            wxString idxstr;
            idxstr = ext.Mid(dpos+1);
            long longint;
            if (idxstr.ToLong(&longint))
            {
                sdir << src_dirs.Item(longint-1);
            }
        }
        sdir << _T("\n");
        mfile.Write(sdir);
    }
    mfile.Write(objdir);

    TargetType tagTyp = buildTarget->GetTargetType();
    wxFileName exefile(buildTarget->GetOutputFilename());
    wxFileName basepath;
    basepath.Assign(buildTarget->GetBasePath(), wxEmptyString, wxEmptyString);
    wxArrayString bpdirs = basepath.GetDirs();
    if (bpdirs.GetCount() > 0)
    {
        for (size_t i=bpdirs.GetCount(); i>0; i--)
            exefile.PrependDir(bpdirs.Item(i-1));
        exefile.SetVolume(basepath.GetVolume()+wxFileName::GetVolumeSeparator());
    }

    wxString exeStr;
    if (basepath.HasVolume())
        exeStr << basepath.GetVolume() << wxFileName::GetVolumeSeparator() << wxFileName::GetPathSeparator();
    else
    {
        wxString sep = wxFileName::GetPathSeparator();
        if (!exefile.GetPath(wxPATH_GET_SEPARATOR).StartsWith(sep))
            exeStr << wxFileName::GetPathSeparator();
    }
    exeStr << exefile.GetPath(wxPATH_GET_SEPARATOR);
    exefile = wxFileName(exeStr, exefile.GetName(), exefile.GetExt());
    exefile.MakeRelativeTo(mffn.GetPath(wxPATH_GET_SEPARATOR));

    mfile.Write(_T("EXE_DIR = ") + exefile.GetPath(wxPATH_GET_VOLUME | wxPATH_GET_SEPARATOR, wxPATH_UNIX) + _T("\n"));
    mfile.Write(_T("\nEXE = ") + exefile.GetFullName() + _T("\n"));
    Compiler * compiler = CompilerFactory::GetCompiler(buildTarget->GetCompilerID());
    wxString compStr = _T("FC = ");
    wxString linkerStr = _T("LD = ");
    if (compiler)
    {
        compStr << compiler->GetPrograms().C;
        compStr << _T("\n");
        if (tagTyp == ttDynamicLib)
            linkerStr << compiler->GetPrograms().LD;
        else if (tagTyp == ttStaticLib)
            linkerStr << compiler->GetPrograms().LIB;
        else
            linkerStr << compiler->GetPrograms().LD;
        linkerStr << _T("\n");
    }
    else
        compStr << _T("\n");
    mfile.Write(compStr);
    mfile.Write(linkerStr);

    wxString idir = _T("IDIR = ");
    const wxArrayString& idirs = project->GetIncludeDirs();
    for(size_t i=0; i<idirs.size(); i++)
    {
        idir << _T("-I") << idirs.Item(i) << _T(" ");
    }
    const wxArrayString& idirst = buildTarget->GetIncludeDirs();
    for(size_t i=0; i<idirst.size(); i++)
    {
        idir << _T("-I") << idirst.Item(i) << _T(" ");
    }
    mfile.Write(idir + _T("\n"));

    wxString cflags = _T("CFLAGS = ");
    const wxArrayString& copt = project->GetCompilerOptions();
    for(size_t i=0; i<copt.size(); i++)
    {
        cflags << copt.Item(i) << _T(" ");
    }
    const wxArrayString& copt_t = buildTarget->GetCompilerOptions();
    for(size_t i=0; i<copt_t.size(); i++)
    {
        cflags << copt_t.Item(i) << _T(" ");
    }

    if(compiler)
    {
        const wxArrayString& copt2 = compiler->GetCompilerOptions();
        for(size_t i=0; i<copt2.size(); i++)
        {
            cflags << copt2.Item(i) << _T(" ");
        }
    }

    if (CompilerFactory::CompilerInheritsFrom(buildTarget->GetCompilerID(), _T("g95")))
        cflags << _T(" -fmod=$(OBJS_DIR) $(IDIR)");
    else if (CompilerFactory::CompilerInheritsFrom(buildTarget->GetCompilerID(), _T("ifclin")))
        cflags << _T(" -module $(OBJS_DIR) $(IDIR)");
    else if (CompilerFactory::CompilerInheritsFrom(buildTarget->GetCompilerID(), _T("ifcwin")))
        cflags << _T(" /nologo /module:$(OBJS_DIR) $(IDIR)");
    else if (CompilerFactory::CompilerInheritsFrom(buildTarget->GetCompilerID(), _T("pgfortran")))
        cflags << _T(" -module $(OBJS_DIR) $(IDIR)");
    else //gfortran
        cflags << _T(" -J$(OBJS_DIR) $(IDIR)");

    mfile.Write(cflags + _T("\n"));

    wxString lflags = _T("LFLAGS = ");
    const wxArrayString& lopt = project->GetLinkerOptions();
    for(size_t i=0; i<lopt.size(); i++)
    {
        wxString optstr = lopt.Item(i);
        optstr.Trim();
        int ipos = optstr.Find(_T("--rpath=\\\\$$$ORIGIN"));
        if (ipos != wxNOT_FOUND)
        {
            wxString optstr1 = optstr.Mid(0, ipos+8);
            wxString optstr2 = _T("'$$ORIGIN") + optstr.Mid(ipos+19) + _T("'");
            optstr = optstr1 + optstr2;
        }
        lflags << optstr << _T(" ");
    }
    const wxArrayString& lopt_t = buildTarget->GetLinkerOptions();
    for(size_t i=0; i<lopt_t.size(); i++)
    {
        wxString optstr = lopt_t.Item(i);
        optstr.Trim();
        int ipos = optstr.Find(_T("--rpath=\\\\$$$ORIGIN"));
        if (ipos != wxNOT_FOUND)
        {
            wxString optstr1 = optstr.Mid(0, ipos+8);
            wxString optstr2 = _T("'$$ORIGIN") + optstr.Mid(ipos+19) + _T("'");
            optstr = optstr1 + optstr2;
        }
        lflags << optstr << _T(" ");
    }
    mfile.Write(lflags + _T("\n"));

    wxString libs = _T("LIBS = ");
    const wxArrayString& ldirs = project->GetLibDirs();
    for(size_t i=0; i<ldirs.size(); i++)
    {
        libs << _T("-L") << ldirs.Item(i) << _T(" ");
    }
    const wxArrayString& ldirst = buildTarget->GetLibDirs();
    for(size_t i=0; i<ldirst.size(); i++)
    {
        libs << _T("-L") << ldirst.Item(i) << _T(" ");
    }
    const wxArrayString& lbsarr = project->GetLinkLibs();
    for(size_t i=0; i<lbsarr.size(); i++)
    {
        wxString lnam;
        if (lbsarr.Item(i).StartsWith(_T("lib")))
            lnam = lbsarr.Item(i).Mid(3);
        else
            lnam = lbsarr.Item(i);
        libs << _T("-l") << lnam << _T(" ");
    }
    const wxArrayString& lbsarrt = buildTarget->GetLinkLibs();
    for(size_t i=0; i<lbsarrt.size(); i++)
    {
        wxString lnam;
        if (lbsarrt.Item(i).StartsWith(_T("lib")))
            lnam = lbsarrt.Item(i).Mid(3);
        else
            lnam = lbsarrt.Item(i);
        libs << _T("-l") << lnam << _T(" ");
    }
    mfile.Write(libs + _T("\n"));


    wxString vpath;
    vpath << _T("\nVPATH = ");
    for (size_t i=0; i<src_ext.Count(); i++)
    {
        vpath << _T("$(SRC_DIR_") << src_ext.Item(i) << _T(")");
        vpath << _T(":$(OBJS_DIR)");
        if (i < src_ext.Count()-1)
        {
            vpath << _T(":");
        }
    }
//    vpath << _T("\nendif\n");
    vpath << _T("\n");
    mfile.Write(vpath);

    wxString objsstr = _T("OBJS = $(addprefix $(OBJS_DIR),");
    for (size_t i=0; i<src_ext.Count(); i++)
    {
        objsstr << _T(" $(OBJS_") << src_ext.Item(i) << _T(")");
    }
    objsstr << _T(")\n");
    mfile.Write(objsstr);

    mfile.Write(_T("\nall : $(EXE)\n"));

    wxString lstr = _T("\n$(EXE) :");

    for (size_t i=0; i<src_ext.Count(); i++)
    {
        lstr << _T(" $(OBJS_") << src_ext.Item(i) << _T(")");
    }
    lstr << _T("\n\t@mkdir -p $(EXE_DIR)");
    if (tagTyp == ttDynamicLib)
    {
        lstr << _T("\n\t$(LD)");
        lstr << _T(" -shared $(OBJS) -o $(EXE_DIR)$(EXE) $(LFLAGS) $(LIBS)\n");
    }
    else if (tagTyp == ttStaticLib)
    {
        lstr << _T("\n\trm -f $(EXE_DIR)$(EXE)");
        lstr << _T("\n\t$(LD)");
        lstr << _T(" -r -s $(EXE_DIR)$(EXE) $(OBJS)\n");
    }
    else
    {
        lstr << _T("\n\t$(LD)");
        lstr << _T(" -o $(EXE_DIR)$(EXE) $(OBJS) $(LFLAGS) $(LIBS)\n");
    }
    mfile.Write(lstr);

    for (size_t i=0; i<src_ext.Count(); i++)
    {
        wxString sdir;
        sdir << _T("$(SRC_DIR_") << src_ext.Item(i) << _T(")");

        wxString ext = src_ext.Item(i);
        int dpos = ext.Find('d',true);
        if (dpos != wxNOT_FOUND)
            ext = ext.Mid(0,dpos);

        wxString cstr;
        cstr << _T("\n$(OBJS_") << src_ext.Item(i) << _T("):\n");
        cstr << _T("\t@mkdir -p $(OBJS_DIR)\n");
        if (CompilerFactory::CompilerInheritsFrom(buildTarget->GetCompilerID(), _T("ifcwin")))
            cstr << _T("\t$(FC) $(CFLAGS) /c ") << sdir << _T("$(@:.o=.") << ext << _T(")") << _T(" /object: $(OBJS_DIR)$@\n");
        else
            cstr << _T("\t$(FC) $(CFLAGS) -c ") << sdir << _T("$(@:.o=.") << ext << _T(")") << _T(" -o $(OBJS_DIR)$@\n");

        mfile.Write(cstr);
    }

    wxString clean;
    clean << _T("\nclean :\n");
	clean << _T("\trm -f $(OBJS_DIR)*.*\n");
	clean << _T("\trm -f $(EXE_DIR)$(EXE)\n");
    mfile.Write(clean);

    mfile.Write(_T("\n# Dependencies of files\n"));
    mfile.Write(depsFiles);
    mfile.Write(_T("\n"));


    if (containsNonFortranFiles)
        cbMessageBox(_("The build target includes non Fortran files. They were added to the list of files, however the plugin doesn't know how to handle these files correctly."), _("Warning"), wxICON_WARNING);

    wxString msg = _("The make file \"");
    msg << mffn.GetFullPath();
    msg << _("\" was generated seccessfully.");
    cbMessageBox(msg);
}

bool MakefileGen::SelectMikefileName(wxFileName& mffn)
{
    MakefileDlg mfdlg(Manager::Get()->GetAppWindow());
    mfdlg.SetFilename(mffn.GetFullPath());
    int imax = 5;
    int i;
    for (i=0; i<imax; i++)
    {
        if (mfdlg.ShowModal() != wxID_OK)
            return false;
        mffn = mfdlg.GetFilename();
        if (!mffn.IsOk())
        {
            cbMessageBox(_("Error in the file name!"), _("Error"), wxICON_ERROR);
            continue;
        }

        if (mffn.FileExists())
        {
            int answ = cbMessageBox(_T("File \"")+mffn.GetFullPath()+_T("\" already exist.\nWould you like to overwrite it?"), _("Question"), wxYES_NO | wxICON_QUESTION);
            if (answ == wxID_YES)
                break;
        }
        else
            break;
    }

    if (i == imax)
    {
        cbMessageBox(_("I am tired. Maybe next time..."), _("Info"), wxICON_INFORMATION);
        return false;
    }
    return true;
}
