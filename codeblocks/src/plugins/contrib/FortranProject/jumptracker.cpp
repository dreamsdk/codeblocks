/*
 * This file is licensed under the GNU General Public License, version 3
 * http://www.gnu.org/licenses/gpl-3.0.html
 *
 * Author: Darius Markauskas
 *
 */

#include "jumptracker.h"

JumpTracker::JumpTracker()
{
    m_HomeIsStart = true;
}

JumpTracker::~JumpTracker()
{
}

void JumpTracker::TakeJump(LineAddress& jumpStart, LineAddress& jumpFinish)
{
    if (jumpStart.IsSameAs(jumpFinish) && jumpStart.IsSameAs(m_JumpHome))
    {
        //do nothing
    }
    else if (jumpStart.IsSameAs(jumpFinish) && !m_JumpHome.GetFilename().IsEmpty())
    {
        if (m_JumpHome.IsFinish())
            m_JumpBack.push_front(m_JumpHome);
        m_JumpHome = jumpFinish;
    }
    else if ( m_JumpHome.IsSameAs(jumpStart) && !m_JumpForward.empty() && jumpFinish.IsSameAs(m_JumpForward.front()) )
    {
        m_JumpBack.push_front(m_JumpHome);
        m_JumpHome = m_JumpForward.front();
        m_JumpForward.pop_front();
    }
    else if (m_JumpHome.IsSameAs(jumpFinish) && !m_JumpBack.empty() && m_JumpBack.front().IsSameAs(jumpStart))
    {
        // jump was repeated. Do nothing.
    }
    else
    {
        if (!m_JumpHome.GetFilename().IsEmpty() && m_JumpHome.IsFinish() && !m_JumpHome.IsSameAs(jumpStart))
        {
            m_JumpBack.push_front(m_JumpHome);
        }
        m_JumpBack.push_front(jumpStart);
        m_JumpHome = jumpFinish;
        m_JumpForward.clear();
    }
    while (m_JumpBack.size() > 50)
    {
        m_JumpBack.pop_back();
    }
}

bool JumpTracker::IsJumpBackEmpty()
{
    return m_JumpBack.empty();
}

bool JumpTracker::IsJumpHomeEmpty()
{
    return m_JumpHome.GetFilename().IsEmpty();
}

bool JumpTracker::IsJumpForwardEmpty()
{
    return m_JumpForward.empty();
}

void JumpTracker::MakeJumpBack()
{
    if (!IsJumpBackEmpty())
    {
        m_JumpForward.push_front(m_JumpHome);
        m_JumpHome = m_JumpBack.front();
        m_JumpBack.pop_front();
    }
}

void JumpTracker::MakeJumpForward()
{
    if (!IsJumpForwardEmpty())
    {
        m_JumpBack.push_front(m_JumpHome);
        m_JumpHome = m_JumpForward.front();
        m_JumpForward.pop_front();
    }
}

const LineAddress& JumpTracker::GetHomeAddress()
{
    return m_JumpHome;
}
