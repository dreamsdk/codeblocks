/*
 * This file is part of the Code::Blocks IDE and licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 * $Revision: 11535 $
 * $Id: headerfixup.cpp 11535 2018-12-20 11:16:02Z fuscated $
 * $HeadURL: svn://svn.code.sf.net/p/codeblocks/code/branches/release-20.xx/src/plugins/contrib/headerfixup/headerfixup.cpp $
 */

#include <wx/window.h>

#include <sdk.h>
#include <cbproject.h>
#include <globals.h>
#include <manager.h>
#include <projectmanager.h>

#include "headerfixup.h"
#include "configuration.h"
#include "execution.h"

// ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

namespace
{
  PluginRegistrant<HeaderFixup> reg(_T("HeaderFixup"));
}

// ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

HeaderFixup::HeaderFixup()
{
}// HeaderFixup

// ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

HeaderFixup::~HeaderFixup()
{
}// ~HeaderFixup

// ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

void HeaderFixup::OnAttach()
{
}// OnAttach

// ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

void HeaderFixup::OnRelease(bool /*appShutDown*/)
{
}// OnRelease

// ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

int HeaderFixup::Execute()
{
  // if not attached, exit
  if ( !IsAttached() )
    return -1;

  // if no project is opened -> inform the user and do not operate
  const cbProject* prj = Manager::Get()->GetProjectManager()->GetActiveProject();
  if (!prj)
  {
    cbMessageBox(_("You need to open a project/workspace before using this plugin!"),
                 _T("Header Fixup"), wxICON_ERROR | wxOK);
    return -1;
  }

  Execution Dlg(NULL);
  Dlg.ShowModal();
  return 0;
}// Execute

// ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

cbConfigurationPanel* HeaderFixup::GetConfigurationPanel(wxWindow* parent)
{
  return new Configuration(parent);
}// GetConfigurationPanel
