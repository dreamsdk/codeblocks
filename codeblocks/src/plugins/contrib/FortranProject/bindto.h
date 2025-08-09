#ifndef BINDTO_H
#define BINDTO_H

//(*Headers(Bindto)
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/dialog.h>
#include <wx/listctrl.h>
#include <wx/notebook.h>
#include <wx/panel.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
//*)

#include "parserf.h"
#include "bindtonewtype.h"
#include <map>

enum BindToIn
{
    bindToProject,
    bindToFile,
};

typedef std::map<wxString,wxArrayString> TypeMap;
typedef std::map<wxString,wxString> StrMap;
typedef std::set<wxString> StrSet;

typedef struct {
	TokenF* procToCall;
	wxString m_Name;
	bool m_Pass;
	wxString m_PassArg;
} TypeTBP;
typedef std::vector<TypeTBP> TypeTBPList;

class Bindto: public wxDialog
{
	public:

		Bindto(wxWindow* parent, ParserF* pParser);
		virtual ~Bindto();

		//(*Declarations(Bindto)
		wxButton* bt_Add;
		wxButton* bt_Defaults;
		wxButton* bt_Edit;
		wxButton* bt_OutputDir;
		wxButton* bt_Remove;
		wxButton* bt_copy;
		wxCheckBox* cb_ctorEnd;
		wxCheckBox* cb_ctorStart;
		wxCheckBox* cb_dtorEnd;
		wxCheckBox* cb_dtorStart;
		wxCheckBox* cb_genCython;
		wxCheckBox* cb_globalToOne;
		wxCheckBox* cb_incompleteWrapperProc;
		wxCheckBox* cb_pyFirstSelf;
		wxCheckBox* cb_pyGenClass;
		wxListView* lv_Types;
		wxPanel* Panel1;
		wxPanel* Panel2;
		wxPanel* Panel4;
		wxPanel* Panel5;
		wxPanel* pn_pyOpts;
		wxRadioButton* rb_ActiveProject;
		wxRadioButton* rb_CurrentFile;
		wxStaticText* StaticText16;
		wxStaticText* StaticText7;
		wxStaticText* st_globalFilename;
		wxTextCtrl* tc_OutputDir;
		wxTextCtrl* tc_bindCName;
		wxTextCtrl* tc_ctorEnd;
		wxTextCtrl* tc_ctorStart;
		wxTextCtrl* tc_dtorEnd;
		wxTextCtrl* tc_dtorStart;
		wxTextCtrl* tc_globalFilename;
		wxTextCtrl* tc_pyFunName;
		//*)

	protected:

		//(*Identifiers(Bindto)
		static const wxWindowID ID_BTOACTIVEPROJECT;
		static const wxWindowID ID_BTOCURRENTFILE;
		static const wxWindowID ID_TEXTCTRL1;
		static const wxWindowID ID_STATICTEXT5;
		static const wxWindowID ID_CHECKBOX9;
		static const wxWindowID ID_CHECKBOX3;
		static const wxWindowID ID_TEXTCTRL6;
		static const wxWindowID ID_STATICTEXT1;
		static const wxWindowID ID_TEXTCTRL7;
		static const wxWindowID ID_BUTTON1;
		static const wxWindowID ID_PANEL2;
		static const wxWindowID ID_LV_TYPES;
		static const wxWindowID ID_BUTTON_ADD;
		static const wxWindowID ID_BUTTON_COPY;
		static const wxWindowID ID_BUTTON_EDIT;
		static const wxWindowID ID_BUTTON_REMOVE;
		static const wxWindowID ID_BUTTON_DEFAULTS;
		static const wxWindowID ID_PANEL1;
		static const wxWindowID ID_CHECKBOX4;
		static const wxWindowID ID_TEXTCTRL4;
		static const wxWindowID ID_CHECKBOX5;
		static const wxWindowID ID_TEXTCTRL5;
		static const wxWindowID ID_PANEL3;
		static const wxWindowID ID_CHECKBOX1;
		static const wxWindowID ID_TEXTCTRL2;
		static const wxWindowID ID_CHECKBOX2;
		static const wxWindowID ID_TEXTCTRL3;
		static const wxWindowID ID_PANEL4;
		static const wxWindowID ID_CHECKBOX6;
		static const wxWindowID ID_TEXTCTRL8;
		static const wxWindowID ID_CHECKBOX7;
		static const wxWindowID ID_CHECKBOX8;
		static const wxWindowID ID_PANEL6;
		static const wxWindowID ID_PANEL5;
		static const wxWindowID ID_NOTEBOOK1;
		//*)

	private:

		//(*Handlers(Bindto)
		void OnAdd(wxCommandEvent& event);
		void OnEdit(wxCommandEvent& event);
		void OnRemove(wxCommandEvent& event);
		void OnDefaults(wxCommandEvent& event);
		void OnClick_cbCtorStart(wxCommandEvent& event);
		void OnClick_cbCtorEnd(wxCommandEvent& event);
		void OnClick_cbDtorStart(wxCommandEvent& event);
		void OnClick_cbDtorEnd(wxCommandEvent& event);
		void Onrb_ActiveProjectSelect(wxCommandEvent& event);
		void Onbt_OutputDirClick(wxCommandEvent& event);
		void Oncb_genCythonClick(wxCommandEvent& event);
		void OnCopy(wxCommandEvent& event);
		void Oncb_globalToOneClick(wxCommandEvent& event);
		//*)

		enum Language
		{
		    Fortran,
		    C,
		    Python,
		};

		struct TypeBind
		{
		    wxString fType;
		    wxString fTypeOnly;
		    wxString fDrvTypeName;
		    wxString bType;
		    wxString bDim;
		    wxString cType;
		    wxString cDim;
		    wxString info;
		    wxString errMsg;
		    bool wasFound;
		};

		struct TypePyx
		{
		    wxString declarPyxFirst;
		    wxString callCSecond;
		    wxString intent;
		    wxString fDrvTypeName;
		    wxString initStr;
		    bool hide;
		    bool copy;
		    int ndim;
		    wxArrayString addIntArg;
		};

		struct BintoDirective
		{
		    wxString      varName;
		    wxArrayString dim;
		    StrSet        intent;
		    wxString      initStr;
		};
		typedef std::map<wxString,BintoDirective> BTDirMap;

		ParserF* m_pParser;
        TokenF*  m_pTokenCurrent;
        int m_Indent;
        int m_TabSize;
        TypeMap m_TypeMap;
        TypeMap m_TypeDefinedInMap;
        TypeMap m_TypeDefinedInGlobMap;
        bool m_IsTypeMapDefault;
        bool m_WriteIncompleteWrapper;
        bool m_OneGProcFile;
        wxString m_OneGProcFileName;
        wxString m_BindCName;
        wxString m_CtorStartsWith;
        wxString m_CtorEndsWith;
        wxString m_DtorStartsWith;
        wxString m_DtorEndsWith;
        wxString m_InitialOutputDirFile;
        wxString m_InitialOutputDirProj;
        wxString m_OutputDir;
        bool m_LogToInt;
        TypeMap m_GlobLogFunMap;
        StrSet  m_LogTypeSet;
        wxString m_ProjectBinDir;
        wxString m_TargetLibraryName;
        bool m_IsTargetStaticLib;
        wxString m_TargetCompilerName;

        bool m_UseOneGlobalFile;
        wxString m_GlobProceduresFile;
        wxString m_GlobProceduresFileH;
        wxString m_GlobProcWarnMessages;
        StrSet m_GlobProceduresCInclude;
        bool m_GlobWriteStrFtoC;
        bool m_GlobWriteStrCtoF;
        bool m_GlobWriteStrLen;
        wxString m_WarnMessage;
        StrSet m_NotFoundTypes;
        wxString m_CStructs;
        wxArrayString m_CreatedMsg;
        StrSet m_CInclude;
        bool m_WriteStrFtoC;
        bool m_WriteStrCtoF;
        bool m_WriteStrLen;
        StrSet m_DefinedTypes;
        StrSet m_DefinedTypesBindC;
        StrSet m_NoArgConstructors;
        StrMap m_Deallocators;
        StrSet m_ModuleChildNames;
        wxString m_CurProcedure;
        wxString m_CurModule;
        wxString m_CurFile;
        bool m_InFortranModule;
        BTDirMap m_BTDirMap;
        bool m_FileWasCreated;
        wxArrayString m_PyxFileArr;

        bool m_PyGenCython;
        StrSet m_PyInclude;
        int m_PyIndent;
        wxString m_PyFuncName;
        bool m_PyCreateClass;
        bool m_PyFirstArgAsSelf;
        bool m_HasPyClassConstructor;
        StrMap m_C2NumpyTypes;
        StrMap m_C2CnpTypes;

        StrSet m_PyIncludeGlob;
        wxString m_TxtCythonFirstGlob;
        wxString m_TxtCythonGlob;

        void FillC2NumpyTypesMap();
        void FillC2CnpTypesMap();
        void FillTypeList();
        void LoadInitialValues();
        void FillTypeMapDefault();
        void LoadBindToConfig();
        void SaveBindToConfig();
        void OnOK(wxCommandEvent& event);
        void MakeBindTo(BindToIn btin);
        void FileBindTo(const wxString& filename);
        wxString GetIS(int nint = -1);
        wxString CreateBindFilename(const wxString& filename, bool header);
        wxString CheckOverwriteFilename(wxFileName &fname);
        void BindProcedure(wxString& txtBind, wxString& txtHeaders, wxString& txtCythonFirst, wxString& txtCythonSecond,
                           TokenF* token, const wxString& moduleName, bool isGlobal, wxString callName=wxEmptyString,
                           const TypeTBP* tbToken=NULL);
        void FillTypeBoundProcList(TokenF* modToken, TokenF* typeToken, TypeTBPList& typeTBPList);
        TypeBind GetBindType(TokenF* token, int& nDimVarAdd);
        TypeBind GetBindType(const wxString& declar, int& nDimVarAdd);
        wxString GetToken(const wxString& txt, int iPos);
        wxArrayString GetTypeAndKind(wxString decl);
        wxString GetFunctionDeclaration(TokenF* token);
        wxString GetCDims(wxString vdim);
        wxString SplitLines(const wxString& txt, Language lang);
        void GetSubStrFtoC(wxArrayString& strFtoC);
        void GetSubStrCtoF(wxArrayString& strCtoF);
        void GetFunStrLen(wxArrayString& strLen);
        void GetFunLogical(const wxString& logType, const wxString& nameLtoI, const wxString& nameItoL, wxArrayString& funLtoI, wxArrayString& funItoL);
        void GetHelperModule(bool useGlobal, bool getAll, std::map<wxString,wxString> &procMap, wxString& modHead);
        void PrepareAssumedShapeVariables(const wxArrayString& argArr, const wxArrayString& dimVarNames,
                                          wxArrayString& additionalDeclar, wxArrayString& addVarNames, wxArrayString& addVarNamesC,
                                          const wxArrayString& varNamesOfDim, const StrSet& argHideSetPy, wxArrayString& additionalDeclarPy,
                                          wxArrayString& addVarNamesPy, wxArrayString& addArgNamesPy);
        void AddDimVariables(const wxArrayString& argArr, wxArrayString& dimVarNames, int nDimVarAdd, wxString varFirstPart, const wxString& argName,
                             wxArrayString& varNamesOfDim, TypeBind& tys);
        void HideAssumedShape(const wxString& vdim, wxString& vdimHid, int& nAssumedDim);
        void AddDimVariablesFromDoc(wxArrayString& dimVarNames, int& nDimVarAdd, const wxString& argName,
                                    wxArrayString& varNamesOfDim, TypeBind& tys);
        wxString GetCName(const wxString& procName, const wxString& moduleName);
        wxString GetProcName(const wxString& procName, const wxString& moduleName, const wxString& nameFrame);
        void AddConstructors(wxString& txtBind, wxString& txtHeadersMod, wxString& txtCythonCtor, wxString& txtCythonFirst, const wxString& moduleName);
        void AddDestructors(wxString& txtBind, wxString& txtHeadersMod, wxString& txtCythonDtor, wxString& txtCythonFirst, const wxString& moduleName);
        wxString GetConstructorName(const wxString& type);
        bool IsConstructor(TokenF* token);
        bool IsDestructor(TokenF* token);
        wxString GetPyName(const wxString& procName, const wxString& moduleName);
        TypePyx GetBindTypePy(const TypeBind& tya, const wxString& varName);
        wxString CreateCythonFilename(const wxString& filename);
        void GetInitialOutputDir(wxString& initialOutputDirFile, wxString& initialOutputDirProj);
        bool MakeOutputDir();
        bool ValidatePyFuncName();
        void ShowNewTypeDlg(BindtoNewType& addNewType);
        void PrepateTypes(wxString& ft, wxString& bt, wxString& ct);
        wxArrayString GetLogFunNames(const wxString& fType);
        void ParseBindtoDirectives(const TokenF* parentToken);
        wxArrayString GetDimArr(const wxString& dimStr);
        void AddPyArgs(const wxArrayString& argArr, wxArrayString& morePyIntArgs, const wxArrayString& addIntArg);
        void WriteSetupPy(const wxArrayString& pyxFArr, const wxString& setupPyFn, const wxString& binDir);
        void AddToCStruct(TokenF* typeTok);
        void GetHeaderStartEnd(const wxString& hfname, wxString& hStart, wxString& hEnd);
        void WriteHelperModFile();
        void AddToLogFile(const wxString& msg);

		DECLARE_EVENT_TABLE()
};

#endif
