#ifndef JUMPTRACKER_H
#define JUMPTRACKER_H

#include "lineaddress.h"

class JumpTracker
{
    public:
        JumpTracker();
        ~JumpTracker();
        void TakeJump(LineAddress& jumpStart, LineAddress& jumpFinish);
        bool IsJumpBackEmpty();
        bool IsJumpHomeEmpty();
        bool IsJumpForwardEmpty();
        void MakeJumpBack();
        void MakeJumpForward();
        const LineAddress& GetHomeAddress();
    protected:
    private:
        JumpAddressList                    m_JumpForward;
        LineAddress                        m_JumpHome;
        JumpAddressList                    m_JumpBack;
        bool                               m_HomeIsStart;
};

#endif // JUMPTRACKER_H

