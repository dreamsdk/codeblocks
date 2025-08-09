/*
 * This file is part of the Code::Blocks IDE and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 * $Revision: 13091 $
 * $Id: todosettingsdlg.cpp 13091 2022-12-03 08:40:20Z wh11204 $
 * $HeadURL: https://svn.code.sf.net/p/codeblocks/code/branches/release-25.03/src/plugins/todo/todosettingsdlg.cpp $
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
    wxXmlResource::Get()->LoadPanel(this, parent, "ToDoSettingsDlg");
    bool checked = Manager::Get()->GetConfigManager("todo_list")->ReadBool("auto_refresh", true);
    bool standalone = Manager::Get()->GetConfigManager("todo_list")->ReadBool("stand_alone", true);
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
    Manager::Get()->GetConfigManager("todo_list")->Write("auto_refresh", checked);
    Manager::Get()->GetConfigManager("todo_list")->Write("stand_alone", standalone);
}
