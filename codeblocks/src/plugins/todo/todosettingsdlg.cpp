/*
 * This file is part of the Code::Blocks IDE and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 * $Revision: 7109 $
 * $Id: todosettingsdlg.cpp 7109 2011-04-15 11:53:16Z mortenmacfly $
 * $HeadURL: svn://svn.code.sf.net/p/codeblocks/code/branches/release-20.xx/src/plugins/todo/todosettingsdlg.cpp $
 */

#include "sdk.h"
#ifndef CB_PRECOMP
  #include <wx/checkbox.h>
  #include <wx/intl.h>
  #include <wx/string.h>
  #include <wx/xrc/xmlres.h>
  #include "configmanager.h"
  #include "manager.h"
#endif
#include "todosettingsdlg.h"

ToDoSettingsDlg::ToDoSettingsDlg(wxWindow* parent)
{
    //ctor
    wxXmlResource::Get()->LoadPanel(this, parent, _T("ToDoSettingsDlg"));
    bool checked = Manager::Get()->GetConfigManager(_T("todo_list"))->ReadBool(_T("auto_refresh"), true);
    bool standalone = Manager::Get()->GetConfigManager(_T("todo_list"))->ReadBool(_T("stand_alone"), true);
    XRCCTRL(*this, "chkAutoRefresh", wxCheckBox)->SetValue(checked);
    XRCCTRL(*this, "chkMessagesPane", wxCheckBox)->SetValue(!standalone);
}

ToDoSettingsDlg::~ToDoSettingsDlg()
{
    //dtor
}

void ToDoSettingsDlg::OnApply()
{
    bool checked = XRCCTRL(*this, "chkAutoRefresh", wxCheckBox)->GetValue();
    bool standalone = !(XRCCTRL(*this, "chkMessagesPane", wxCheckBox)->GetValue());
    Manager::Get()->GetConfigManager(_T("todo_list"))->Write(_T("auto_refresh"), checked);
    Manager::Get()->GetConfigManager(_T("todo_list"))->Write(_T("stand_alone"), standalone);
}
