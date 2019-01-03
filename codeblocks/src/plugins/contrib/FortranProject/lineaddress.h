/*
 * This file is licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 * Author: Darius Markauskas
 *
 */

#ifndef LINEADDRESS_H_INCLUDED
#define LINEADDRESS_H_INCLUDED

#include <wx/string.h>
#include <list>


class LineAddress;
typedef std::list<LineAddress> JumpAddressList;

class LineAddress
{
    public:
        LineAddress();
        ~LineAddress();

        void Init(const wxString& fileName, int lineNumber, bool isFinish);
        bool IsSameAs(LineAddress &other);
        wxString GetFilename()const {return m_Filename;};
        int GetLineNumber()const {return m_LineNumber;};
        bool IsFinish()const {return m_IsFinish;};

    private:
        wxString    m_Filename;
        int         m_LineNumber;
        bool        m_IsFinish;
};

#endif //LINEADDRESS_H_INCLUDED
