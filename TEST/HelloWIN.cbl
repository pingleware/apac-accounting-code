       $set ans85 noosvs mf case defaultbyte"00"
      ****************************************************************
      * Copyright Micro Focus International Limited 1991-2000. All   *
      * Rights Reserved.                                             *
      * This demonstration program is provided for use by users of   *
      * Micro Focus products and may be used, modified and           *
      * distributed as part of your application provided that you    *
      * properly acknowledge the copyright of Micro Focus in this    *
      * material                                                     *
      ****************************************************************
       identification division.
       program-id. Win32Hlo.
      ****************************************************************
      *                                                              *
      *                      WIN32HLO.CBL                            *
      *                                                              *
      * Example program: Win32 'Hello World'                         *
      *                                                              *
      ****************************************************************
      *
      ****************************************************************
      *                                                              *
      * About WIN32HLO                                               *
      *                                                              *
      * A number of extensions to the COBOL language are used in     *
      * this program, and are noted in comments where they occur.    *
      * For full descriptions, look up the appropriate syntax in the *
      * help Index.                                                  *
      *                                                              *
      ****************************************************************
      ****************************************************************
      *                                                              *
      * COBOL Extension: Special-names.                              *
      *                                                              *
      *     call-conventions are supported as below.                 *
      *                                                              *
      *     No bits specified means that the standard COBOL calling  *
      *     conventions are employed.  This means parameters are     *
      *     passed on a stack, last named is first pushed on the     *
      *     stack.  The parameters are removed from the stack by the *
      *     CALLer. Use this for compatibility with existing COBOL   *
      *     programs.                                                *
      *                                                              *
      *     The meaning of the numbers is derived from decomposing   *
      *     the number into binary components, with bits having      *
      *     the following meanings:                                  *
      *                                                              *
      *     0   -   parameters are passed on a stack, first named    *
      *             is first pushed.  So you could call this         *
      *             convention 'REVERSED'                            *
      *     1   -   The parameters are removed from the stack        *
      *             by the called routine                            *
      *     2   -   Reserved for future use                          *
      *     3   -   Ensure that the call name is resolved at link    *
      *             time rather than run time.                       *
      *     4   -   Reserved for use in the OS/2 product             *
      *     5   -   Reserved for use in the OS/2 product             *
      *     6   -   Use Win32 'stdcall' naming convention if the     *
      *             call is resolved at link time                    *
      *                                                              *
      *     The 'WINAPI' convention used by Win32 is obtained        *
      *     from a combination of bits 6, 3 and 1 giving a call      *
      *     convention value of 74                                   *
      *                                                              *
      ****************************************************************
       special-names.
       call-convention 74 is WINAPI.
       data division.
       working-storage section.
      ****************************************************************
      *                                                              *
      * The Microsoft Platform SDK supplies a header file, WINDOWS.H,*                                     *
      * containing data types and constants for Windows programming. *
      * In COBOL we have to scan the C header files and create our   *
      * own constants with the appropriate values.                   *
      * This can be done automatically if required using the program *
      * H2CPY.EXE provided with this COBOL system. To view help on   *
      * the Header-to-copy utility, from the help Contents tab       *
      * double-click Creating User Interfaces, Graphical User        *
      * Interfaces, Win32 API, Header-to-copy utility.               *
      *                                                              *
      * Mini versions of the Windows header files are also supplied  *
      * as copyfiles in the Win32api demonstration application, but  *
      * we cannot guarantee that these are fully up-to-date.         *
      *                                                              *
      * In this program, the WM-PAINT and WM-DESTROY messages are    *
      * used.                                                        *
      * To translate values from C constants to COBOL constants,     *
      * use the following rules:                                     *
      *                                                              *
      *                     C           COBOL                        *
      *     Hexadecimal   0xnn          h"nn"                        *
      *     Decimal         nn            nn                         *
      *                                                              *
      ****************************************************************
       78  WM-PAINT            value h"000F".
       78  WM-DESTROY          value h"0002".
      ****************************************************************
      *                                                              *
      * The supplied C header file defines data types for all the    *
      * Windows data items.  In COBOL, we have to use the COBOL data *
      * types.                                                       *
      *                                                              *
      * As a general conversion rule:                                *
      *                                                              *
      *     'C'         COBOL                                        *
      *     SHORT       PIC S9(4) COMP-5                             *
      *    1 USHORT      PIC 9(4)  COMP-5                             *
      *     LONG        PIC S9(9) COMP-5                             *
      *     ULONG       PIC 9(9)  COMP-5                             *
      *     PVOID       POINTER             (similarly for other     *
      *                                     pointer types)           *
      *     LHANDLE     PIC 9(9)  COMP-5 )  (These are equivalent    *
      *     LHANDLE     PPOINTER         )  for Windows working      *
      *                         LHANDLE is used for any 32bit        *
      *                         handle, eg HAB, HMQ, HPS etc.        *
      *                                                              *
      *     NB  PIC 9(4) COMP-5 is identical to PIC X(2) COMP-5      *
      *     NB  PIC 9(9) COMP-5 is identical to PIC X(4) COMP-5      *
      *                                                              *
      ****************************************************************
      ****************************************************************
      *                                                              *
      * COBOL Extension: Procedure-pointers                          *
      *                                                              *
      *     Data pointers are now complemented by procedure pointers *
      *                                                              *
      ****************************************************************
       01  MyWndProc    procedure-pointer.
      ****************************************************************
      *                                                              *
      * ASCIIZ (null-terminated) strings are not natural with COBOL, *
      * and in particular are not suitable for use as literals.      *
      * Where ASCIIZ strings are used, they must be declared in      *
      * Working-Storage and followed by a x"00" NULL terminator.     *
      *                                                              *
      ****************************************************************
       01  MyClassName          pic x(20) value "Welcome1" & x"00".
       01  MyData.
           03  loop-flag        pic x     value 'C'.
               88  loop-end               value 'E'.
           03  bool             pic s9(9) comp-5.
               88  boolTRUE               value 1.
               88  boolFALSE              value 0.
       01  WndClass.
           03  style            pic  9(9) comp-5.
           03  lpfnWndProc      procedure-pointer.
           03  cbClsExtra       pic s9(9) comp-5.
           03  cbWndExtra       pic s9(9) comp-5.
           03  hInstance        pic  s9(9) comp-5.
           03  hIcon            pic  s9(9) comp-5.
           03  hCursor          pic  s9(9) comp-5.
           03  hbrBackground    pic  s9(9) comp-5.
           03  lpszMenuName     pointer.
           03  lpszClassName    pointer.
      ****************************************************************
      *                                                              *
      * Structures are supplied in C header files, and must be       *
      * converted to COBOL format to be used.                        *
      * Below is a MSG structure, and in LOCAL-STORAGE section       *
      * is an example of an LPPAINTSTRUCT structure (ppaint)         *
      *                                                              *
      ****************************************************************
       01  msg.
           03  msg-hwnd         pic  s9(9) comp-5.
           03  msg-message      pic   9(9) comp-5.
           03  msg-wParam       pic  s9(9) comp-5.
           03  msg-lParam       pic  s9(9) comp-5.
           03  msg-time         pic   9(9) comp-5.
           03  msg-pt.
               05  msg-pt-x     pic  s9(9) comp-5.
               05  msg-pt-y     pic  s9(9) comp-5.
      ****************************************************************
      *                                                              *
      * COBOL Extension: Local-Storage Section.                      *
      * COBOL Extension: Recursion                                   *
      *                                                              *
      *     Any data declared in the LOCAL-STORAGE SECTION is        *
      *     created freshly for each instance of the program.        *
      *     This data cannot be initialized.                         *
      *                                                              *
      ****************************************************************
       local-storage section.
       01  MyData.
           03  mResult          pic  s9(9) comp-5.
           03  tmpFlag          pic  s9(9) comp-5.
           03  hWindow          pic  s9(9) comp-5.
       01  hps                  pic  s9(9) comp-5.
       01  ppaint.
           03  hdc              pic  s9(9) comp-5.
           03  fErase           pic  s9(9) comp-5.
           03  rcl.
               05  xLeft        pic s9(9) comp-5.
               05  yTop         pic s9(9) comp-5.
               05  xRight       pic s9(9) comp-5.
               05  yBottom      pic s9(9) comp-5.
           03  fRestore         pic s9(9) comp-5.
           03  fUpdate          pic s9(9) comp-5.
           03  rgbdata          pic x(32).
       01  hInst                pic s9(9) comp-5.
       01  hPrevInstance        pic s9(9) comp-5.
       01  lpszCmdLine          POINTER.
       01  nCmdShow             pic s9(9) comp-5.
       linkage section.
       01  hWnd                 pic s9(9) comp-5.
       01  iMessage             pic s9(9) comp-5.
       01  wParam               pic s9(9) comp-5.
       01  lParam               pic s9(9) comp-5.
      ****************************************************************
      *                                                              *
      * COBOL Extension: Call-conventions                            *
      *                                                              *
      *     This use of the call-convention WINAPI (declared above   *
      *     in special-names) means that all the entry points in     *
      *     this program follow the Win32 calling convention.        *
      *                                                              *

     ****************************************************************
       procedure division WINAPI.
       MyWinMain section.
           call "PC_WIN_INIT"
                using hInst
                      hPrevInstance
                      lpszCmdLine
                      nCmdShow.
      * check whether the class exists. If not, create it.
           call WINAPI "FindWindowA"
                using by reference    MyClassName
                      by value        0           size 4
                returning             hWindow
           if hWindow = 0
               move 3 to style
               set lpfnWndProc to entry "MyWndProc"
               move 0 to cbClsExtra
               move 0 to cbWndExtra
               move hInst to hInstance
               call WINAPI "LoadIconA"
                    using by value    0           size 4
                          by value    h"00007f00" size 4
                    returning         hIcon
               call WINAPI "LoadCursorA"
                    using by value    0           size 4
                          by value    h"00007f00" size 4
                    returning         hCursor
               call WINAPI "GetStockObject"
                    using by value    0           size 4

                        returning      hbrBackground
               set lpszMenuName to null
               set lpszClassName to address of MyClassName
               call WINAPI "RegisterClassA"
                    using             WndClass
                    returning         tmpFlag
               if tmpFlag = 0

                     exit program returning 0
               end-if
           end-if
       call WINAPI "CreateWindowExA"
            using by value        0              size 4
                  by reference    MyClassName
                  by reference    "COBOL & Windows" & x"00"
                  by value        h"00CF0000"    size 4
                  by value        h"8000"        size 4
                  by value        0              size 4
                  by value        h"8000"        size 4
                  by value        0              size 4
                  by value        0              size 4
                  by value        0              size 4
                  by value        hInst
                  by value        0              size 4
            returning             hWindow
       call WINAPI "ShowWindow"
            using by value        hWindow
                  by value        nCmdShow
       call WINAPI "UpdateWindow"
            using    by value     hWindow

  
      ****************************************************************
      *                                                              *
      * This in-line PERFORM implements the message loop.            *
      *                                                              *
      ****************************************************************
       perform until loop-end
           call WINAPI "GetMessageA"
                using by reference    msg
                      by value        0    size 4
                      by value        0    size 4
                      by value        0    size 4
                returning             bool
           if boolFALSE
               set loop-end to true
           else
               call WINAPI "TranslateMessage"
                    using by reference msg
               call WINAPI "DispatchMessageA"
                    using by reference msg
           end-if
       end-perform
       exit program returning msg-wParam
       stop run.

  
       MyWindowProcedure section.
      ****************************************************************
      *                                                              *
      * COBOL Extension: ENTRY USING BY VALUE                        *
      * COBOL Extension: Recursion                                   *
      *                                                              *
      *     To complement the CALL USING BY VALUE syntax, there is   *
      *     now also ENTRY USING BY VALUE.                           *
      *                                                              *
      *     COBOL being recursive means that the call to             *
      *     CreateWindow (above) can lead to control being           *
      *     passed to this entry point.                              *
      *     In fact, any of the calls in this section could lead     *
      *     to control being passed to a new instance of this        *
      *     entry point (hence the need for LOCAL-STORAGE SECTION.)  *
      *                                                              *
      ****************************************************************
       entry "MyWndProc"
             using by value hWnd
                   by value iMessage
                   by value wParam
                   by value lParam.
       move 0 to mResult
       evaluate iMessage
      ****************************************************************
      *                                                              *
      * The only message we are interested in is the PAINT message   *
      * The sequence of actions is:                                  *
      *                                                              *
      *     Get Handle-To-Presentation-Space (HPS) for painting      *
      *                         in the client window                 *
      *     Fill the window with the System Background colour        *
      *     Write the words 'Hello COBOL World' at position (70,70)  *
      *     Release the HPS.                                         *
      *                                                              *
      ****************************************************************
           when WM-PAINT
               call WINAPI "BeginPaint"
                    using      by value     hwnd
                               by reference ppaint
                    returning hps
               call WINAPI "FillRect"
                    using      by value     hps
                               by reference rcl
                               by value     hbrBackground
               call WINAPI "GetClientRect"
                    using   by value     hwnd
                            by reference rcl
               call WINAPI "DrawTextA"
                    using by value       hps
                          by reference 'Hello COBOL World'
                          by value       17    size 4
                          by reference rcl
                          by value       h"25" size 4
               call WINAPI "EndPaint"
                    using by value       hwnd

                          by reference   ppaint

           when WM-DESTROY
               call WINAPI "PostQuitMessage"
                    using by value       0     size 4
      ****************************************************************
      *                                                              *
      *     All other messages are despatched to the default         *
      *     window procedure according to the Windows rules          *
      *                                                              *
      ****************************************************************
           when other
               call WINAPI "DefWindowProcA"
                    using by value     hWnd
                          by value     iMessage
                          by value     wParam
                          by value     lParam
                    returning          mResult
       end-evaluate
      ****************************************************************
      *                                                              *
      * COBOL Extension: RETURNING phrase                            *
      *                                                              *
      *     To complement the RETURNING phrase on the CALL, you      *
      *     can also use the RETURNING phrase on the EXIT.           *
      *                                                              *
      ****************************************************************
       exit program returning mResult.
