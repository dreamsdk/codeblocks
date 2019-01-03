/*
 * This file is licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 * Author: Darius Markauskas
 *
 */

#include "lineaddress.h"

LineAddress::LineAddress()
{
    m_IsFinish = false;
}

LineAddress::~LineAddress()
{
}

void LineAddress::Init(const wxString& fileName, int lineNumber, bool isFinish)
{
    m_Filename = fileName;
    m_LineNumber = lineNumber;
    m_IsFinish = isFinish;
};

bool LineAddress::IsSameAs(LineAddress &other)
{
    if (m_Filename.IsSameAs(other.m_Filename) && (m_LineNumber == other.m_LineNumber))
        return true;
    else
        return false;
};

