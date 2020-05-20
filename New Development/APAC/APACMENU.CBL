      $SET LINKCOUNT"256" GNT"APACMENU.GNT"
      ******************************************************************
      *                                                                *
      *    ****    *******    ******   **     **  ********  **     **  *
      *   **  **   **    **  **    **  ***   ***  **        ***    **  *
      *  **    **  **    **  **        ** * * **  **        ** *   **  *
      *  ********  *******   **        **  *  **  *****     **  *  **  *
      *  **    **  **        **        **     **  **        **   * **  *
      *  **    **  **        **    **  **     **  **        **    ***  *
      *  **    **  **         ******   **     **  ********  **     **  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     B U S I N E S S   S Y S T E M   M E N U   P R O G R A M    *
      *                                                                *
      *       Version 9.04.04 - February/March 2018                    *
      *                                                                *
      ******************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                VERSIONS OF THE APAC SYSTEM                Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё While developing this system the requirements and budgets Ё
      *    Ё (originally this was a propriety package) for various     Ё
      *    Ё businesses necessitated that at least 4 separate systems  Ё
      *    Ё be produced. As the maintenance and amendments to the 4   Ё
      *    Ё different systems would require man hours that would not  Ё
      *    Ё enable me to do much development, it was decided to use   Ё
      *    Ё one system with the various requirements controlled by    Ё
      *    Ё the various menu programs using a version indicator. This Ё
      *    Ё indicator is located in the second record of the SYSTEM   Ё
      *    Ё file SEC-LEV, which is in the SEC-MODULES. This indicator Ё
      *    Ё together with all the other indicators in SEC-MODULES, is Ё
      *    Ё moved to WS0-MODULES in BUP000 or BUP001 and then passed  Ё
      *    Ё via the LINKAGE SECTION from program to program in  the   Ё
      *    Ё LS0-MODULES group. The other indicators in this group are Ё
      *    Ё used to show which modules of the system are installed.   Ё
      *    фмммммммммммммммммммямммммммммммммммммммяммммммммммммммммммм╣
      *    Ё    SYSTEM FILE    Ё      CHAIN.WS     Ё      CHAIN.LS     Ё
      *    цдддддддддддддддддддедддддддддддддддддддеддддддддддддддддддд╢
      *    Ё  03  SEC-MODULES. Ё  03  WS0-MODULES. Ё  03  LS0-MODULES. Ё
      *    Ё      05  SEC-DTP  Ё      05  WS0-DTP  Ё      05  LS0-DTP  Ё
      *    Ё      05  SEC-STP  Ё      05  WS0-STP  Ё      05  LS0-STP  Ё
      *    Ё      05  SEC-HPD  Ё      05  WS0-HPD  Ё      05  LS0-HPD  Ё
      *    Ё      05  SEC-CRP  Ё      05  WS0-CRP  Ё      05  LS0-CRP  Ё
      *    Ё      05  SEC-GLP  Ё      05  WS0-GLP  Ё      05  LS0-GLP  Ё
      *    Ё      05  SEC-POS  Ё      05  WS0-POS  Ё      05  LS0-POS  Ё
      *    Ё      05  SEC-JCP  Ё      05  WS0-JCP  Ё      05  LS0-JCP  Ё
      *    Ё      05  SEC-VHP  Ё      05  WS0-VHP  Ё      05  LS0-VHP  Ё
      *    Ё      05  SEC-WAG  Ё      05  WS0-WAG  Ё      05  LS0-WAG  Ё
      *    Ё      05  SEC-SAL  Ё      05  WS0-SAL  Ё      05  LS0-SAL  Ё
      *    Ё      05  SEC-LEV  Ё      05  WS0-LEV  Ё      05  LS0-LEV  Ё
      *    Ё      05  SEC-WHS  Ё      05  WS0-WHS  Ё      05  LS0-WHS  Ё
      *    Ё      05  SEC-RTE  Ё      05  WS0-RTE  Ё      05  LS0-RTE  Ё
      *    Ё      05  FILLER   Ё                   Ё                   Ё
      *    юдддддддддддддддддддадддддддддддддддддддаддддддддддддддддддды
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     APACMENU.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   OCTOBER 1982.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1993 - 2018
                                  by James William Lemmon.
                                    (Id No. 4412165050082).

                All rights reserved.

                e-mail jwlemmon@gmail.com.

       SECURITY.
                This program is free software; you can redistribute
                it and/or modify it under the terms of the GNU General
                Public License as published by the Free Software
                Foundation; either version 3 of the License, or (at
                your option) any later version.

                This program is distributed in the hope that it will
                be useful, but WITHOUT ANY WARRANTY; without even the
                implied warranty of MERCHANTABILITY or FITNESS FOR A
                PARTICULAR PURPOSE. See the GNU General Public License
                for more details.

                You should have received a copy of the GNU General
                Public License along with this program. If not, see
                <http://www.gnu.org/licenses/>.

       INSTALLATION.          APAC.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         CURSOR IS CSTART
                         CONSOLE IS CRT
                         CRT STATUS IS KEY-STATUS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY "APACFIDS.SL".

       COPY "CONTROL.SL".

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "SHARED.SL".

       COPY "TXTRAN.SL".

       DATA DIVISION.
      *
      *          ********  ********  **        ******** 
      *          **           **     **        **
      *          **           **     **        ** 
      *          *****        **     **        *****
      *          **           **     **        **  
      *          **           **     **        **
      *          **        ********  ********  ********
      *
       FILE SECTION.

       COPY "APACFIDS.FDE".

       COPY "CONTROL.FDE".

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "SHARED.FDE".

       COPY "TXTRAN.FDE".

      *
      *         **        **    *****    ******    **   **
      *         **        **   **   **   **   **   **  **
      *         **        **   **   **   **  **    ** **
      *         **   **   **   **   **   *****     ****
      *          ** **** **    **   **   **  **    ** **
      *           ***  ***     **   **   **   **   **  **
      *            *    *       *****    **   **   **   **
      *
       WORKING-STORAGE SECTION.
       77  WS-CHECK                  PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-S1                     PIC  9(04)    COMP-5.
       77  WS-S2                     PIC  9(04)    COMP-5.
       77  WS-S3                     PIC  9(04)    COMP-5.
       77  WS-S4                     PIC  9(04)    COMP-5.
       77  WS-SHARED                 PIC  9(04)    COMP-5 VALUE 1.
       77  WS-NETKEY                 PIC  9(04)    COMP-5.
       77  WS-PARKEY                 PIC  9(04)    COMP-5.
       77  WS-TEMP-LIN               PIC  9(02)    COMP-5.
       77  WS-REG                    PIC  9(02)    COMP-5.
       77  WS-OPTION                 PIC  X(01).
       77  PRG-NAME                  PIC  X(18)    VALUE SPACES.
       77  WS-NUM                    PIC  Z9.
       77  WS-CONV                   PIC  9(01).
       77  APAC-SYSTEM               PIC  X(12).
       77  APAC-VERSION              PIC  X(04).

      *    здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Used by calling program (WORKING-STORAGE) and called program Ё
      *    Ё (LINKAGE SECTION) for Main screen layout and headings.       Ё
      *    юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *         C = Clear comment line (50)
      *         E = Clear lines (any line or lines between 5 and 46)
      *         H = Change heading
      *         S = Clear the screen and display headings
      *
           03  CRT-TYPE              PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START             PIC  9(02).
           03  CRT-END               PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM           PIC  X(15)  VALUE "APACMENU".
      *
      *    Screen Heading
      *
           03  CRT-HEADING           PIC  X(40)  VALUE "APAC SYSTEM MAIN MENU".
      *
      *    Company Name
      *
           03  CRT-COMPANY           PIC  X(40)  VALUE "WAITING FOR COMPANY/BUSINESS NAME".
      *
      *    WorkStation / Supervisor
      *
           03  CRT-TERMINAL.
               05  CRT-WRKHD         PIC  X(11)  VALUE "Supervisor ".
               05  CRT-WRKST         PIC  X(03)  VALUE "   ".

       COPY "WS.WS".

       01  WS-HELP.
           03  WS-MODULE             PIC  X(03) VALUE "APC".
           03  WS-PROG               PIC  X(03) VALUE "MEN".

       COPY "CHAIN.WS".

       COPY "FILE-IDX.IDS".

       COPY "FILE.IDS".

       01  W05-COMMAND.
           03  W05-CHAR              PIC  X(128).

       COPY "W12.WS".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

      * 
      *   *****    *****   ******   ******  ******  **    **
      *  **   **  **   **  **   **  **      **      ***   **
      *  **       **       **  **   **      **      ****  **
      *   *****   **       *****    *****   *****   ** ** **
      *       **  **       **  **   **      **      **  ****
      *  **   **  **   **  **   **  **      **      **   ***
      *   *****    *****   **   **  ******  ******  **    **
      *
       SCREEN SECTION.

       COPY "MENUINST.CRT".

       01  S-MENU.
           03  BACKGROUND-COLOR Magenta FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE  6 COLUMN 30 VALUE "                     ".
               05  LINE  7 COLUMN 30 VALUE "  PROGRAM  CONTROL   ".
               05  LINE  8 COLUMN 30 VALUE " 1".
               05          COLUMN 32 VALUE   " Stock             " FOREGROUND-COLOR Cyan  HIGHLIGHT.
               05  LINE  9 COLUMN 30 VALUE " 2".
               05          COLUMN 32 VALUE   " Creditors         " FOREGROUND-COLOR Cyan  HIGHLIGHT.
               05  LINE 10 COLUMN 30 VALUE " 3".
               05          COLUMN 32 VALUE   " Debtors           " FOREGROUND-COLOR Cyan  HIGHLIGHT.
               05  LINE 11 COLUMN 30 VALUE " 4".
               05          COLUMN 32 VALUE   " General Ledger    " FOREGROUND-COLOR Cyan  HIGHLIGHT.
               05  LINE 12 COLUMN 30 VALUE " 5".
               05          COLUMN 32 VALUE   " Sales / Orders    " FOREGROUND-COLOR Cyan  HIGHLIGHT.
               05  LINE 13 COLUMN 30 VALUE " 6".
               05          COLUMN 32 VALUE   " Point-of-Sale     " FOREGROUND-COLOR Cyan  HIGHLIGHT.
               05  LINE 14 COLUMN 30 VALUE " 7".
               05          COLUMN 32 VALUE   " Job Costing       " FOREGROUND-COLOR Cyan  HIGHLIGHT.
               05  LINE 15 COLUMN 30 VALUE " 8".
               05          COLUMN 32 VALUE   " Period End/Dates  " FOREGROUND-COLOR Cyan  HIGHLIGHT.
               05  LINE 16 COLUMN 30 VALUE " A".
               05          COLUMN 32 VALUE   " Utilities         " FOREGROUND-COLOR Cyan  HIGHLIGHT.
               05  LINE 17 COLUMN 30 VALUE " B".
               05          COLUMN 32 VALUE   " Add Bus/Company   " FOREGROUND-COLOR Cyan  HIGHLIGHT.
               05  LINE 18 COLUMN 30 VALUE " C".
               05          COLUMN 32 VALUE   " Switch Companies  " FOREGROUND-COLOR Cyan  HIGHLIGHT.
               05  LINE 19 COLUMN 30 VALUE " D".
               05          COLUMN 32 VALUE   " Print Spool File  " FOREGROUND-COLOR Cyan  HIGHLIGHT.
               05  LINE 20 COLUMN 30 VALUE " E".
               05          COLUMN 32 VALUE   " Cheque Processing " FOREGROUND-COLOR Cyan  HIGHLIGHT.
               05  LINE 21 COLUMN 30 VALUE " F".
               05          COLUMN 32 VALUE   " Orders Spec.      " FOREGROUND-COLOR Cyan  HIGHLIGHT.
               05  LINE 22 COLUMN 30 VALUE " L".
               05          COLUMN 32 VALUE   " Login/Logout      " FOREGROUND-COLOR Cyan  HIGHLIGHT.
               05  LINE 23 COLUMN 30 VALUE " 9".
               05          COLUMN 32 VALUE   " End System        " FOREGROUND-COLOR Cyan  HIGHLIGHT.
               05  LINE 24 COLUMN 30 VALUE "                     ".

       01  SD-CONF.
           03  BACKGROUND-COLOR Red FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE  8 COLUMN 14 VALUE "                                                       ".
               05  LINE  9 COLUMN 14 VALUE " The System parameters, Company details, Departments,  ".        
               05  LINE 10 COLUMN 14 VALUE "     Divisions, Hardware configurations, Integration,  ".        
               05  LINE 11 COLUMN 14 VALUE "     Rate structures & Stationery choices need to be   ".        
               05  LINE 12 COLUMN 14 VALUE "     set up!                                           ".        
               05  LINE 13 COLUMN 14 VALUE " These details include:                                ".        
               05  LINE 14 COLUMN 14 VALUE "     Address, Telephone, VAT registration number,      ".        
               05  LINE 15 COLUMN 14 VALUE "     Sales staff, Technicians, Debtor Categories,      ".        
               05  LINE 16 COLUMN 14 VALUE "     Sales Ledgers, Printers, Cash Drawer/s, Interest  ".        
               05  LINE 17 COLUMN 14 VALUE "     rates, Price rounding, Stationery choices, User   ".        
               05  LINE 18 COLUMN 14 VALUE "     codes, Passwords and Integration account numbers. ".        
               05  LINE 19 COLUMN 14 VALUE " If you are not sure of the above, please read pages   ".        
               05  LINE 20 COLUMN 14 VALUE " 30 to 36 of the manual and then select the Demonstra- ".        
               05  LINE 21 COLUMN 14 VALUE " tion system and selecting option 8 from the PROGRAM   ".        
               05  LINE 22 COLUMN 14 VALUE " CONTROL menu study the various PARAMETER options to   ".        
               05  LINE 23 COLUMN 14 VALUE " familiarise yourself with the requirements.           ".        
               05  LINE 24 COLUMN 14 VALUE "                                                       ".        
               05  LINE 25 COLUMN 14 VALUE " DO THE SETUP NOW ?                                    ".        
               05  LINE 26 COLUMN 14 VALUE "                                                       ".
                                   
       01  S98.
           03  BACKGROUND-COLOR Red FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 27 COLUMN 14 VALUE "                                                     ".         
               05  LINE 28 COLUMN 14 VALUE " Do not forget to make security copies of your files ".          
               05  LINE 29 COLUMN 14 VALUE "                                                     ".
               05  LINE 49 COLUMN 79 VALUE "д"                                                     BACKGROUND-COLOR Blue  FOREGROUND-COLOR Cyan.

       01  S98T.
           03  BACKGROUND-COLOR Red FOREGROUND-COLOR Grey HIGHLIGHT.
               05  LINE 27 COLUMN 22 VALUE "                                      ".
               05  LINE 28 COLUMN 22 VALUE " The terminal may now be switched off ".
               05  LINE 29 COLUMN 22 VALUE "                                      ".
               05  LINE 49 COLUMN 79 VALUE "д"                                     BACKGROUND-COLOR Blue  FOREGROUND-COLOR Cyan.

       01  S-LEGAL.
           03  BACKGROUND-COLOR Cyan FOREGROUND-COLOR Red HIGHLIGHT.
               05  LINE 50 COLUMN  2 VALUE "This package must be registered within ".
               05          COLUMN 42 PIC Z9 USING WS-REG FOREGROUND-COLOR Grey HIGHLIGHT BLINK.
               05          COLUMN 44 VALUE " days".
               
      *
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION.
       AA000        SECTION.
       AA00.
             ACCEPT W05-COMMAND FROM COMMAND-LINE.
             PERFORM ZA000-INIT.
           IF WS0-POS > 0
               IF "N" = WS0-CRED AND WS0-STOCK AND WS0-DEBT AND WS0-JOBS AND WS0-PARM AND WS0-WAGES AND WS0-GL
      *        IF WS0-CRED = "N" AND WS0-STOCK = "N" AND WS0-DEBT = "N" AND WS0-JOBS = "N" AND WS0-PARM = "N" AND WS0-WAGES = "N" AND WS0-GL = "N"
                   GO TO AA10.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 1                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
             PERFORM BA000.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 0                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
           IF WS-USER NUMERIC
               MOVE 27               TO SHADE-ROW
               MOVE 22               TO SHADE-COL
               MOVE 38               TO SHADE-WIDTH
               MOVE 3                TO SHADE-LINES
               DISPLAY S98T
               PERFORM SCREEN-SHADOW
               CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
               STOP RUN.
       AA05.
      *    *************************************************************
      *    ****    P A R A M E T E R   F I L E
      *    *************************************************************
             PERFORM ZA00AA THRU ZA00A-CONTINUE.
             OPEN I-O PARAM.
           IF RUNTIME-ERROR
               IF FLE-LOCKED OR FLE-LIMIT
                   DISPLAY "WAITING" AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                   GO TO AA05.
           IF WS-STATUS NOT = "00" AND WS-STAT1 NOT = "4"
               MOVE 15               TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
             MOVE ZERO               TO PAR-LOGON.
             REWRITE PAR-RECORD1.
             MOVE 3                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
             UNLOCK PARAM.
             CLOSE PARAM.
             MOVE 27                 TO SHADE-ROW.
             MOVE 14                 TO SHADE-COL.
             MOVE 53                 TO SHADE-WIDTH.
             MOVE 3                  TO SHADE-LINES.
             DISPLAY S98.
             PERFORM SCREEN-SHADOW.
             CLOSE APACFIDS.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             STOP RUN.

       AA10.
             MOVE "MNU\POSMENU"      TO PRG-NAME.
             PERFORM AA100.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 0                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
             PERFORM ERASE-SCREEN.
             MOVE 27                 TO SHADE-ROW.
             MOVE 14                 TO SHADE-COL.
             MOVE 53                 TO SHADE-WIDTH.
             MOVE 3                  TO SHADE-LINES.
             DISPLAY S98.
             PERFORM SCREEN-SHADOW.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
             STOP RUN.

       AA100        SECTION.
       AA101.
             PERFORM ZA00AA THRU ZA999.
           IF WS-OPTION = "Y"
               PERFORM CA000
               IF WS-OPTION = "8"
                   PERFORM CLEAR-BLOCK
                   MOVE 11           TO SCREEN-LIN
                   PERFORM SAVE-ATTR
                   PERFORM MARK-BLOCK
                   MOVE "Z"          TO WS-OK
                   MOVE "MNU\UTILMENU" TO PRG-NAME.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 0                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
             PERFORM SAVE-SCREEN-3.

       AA102.
             MOVE SPACES             TO WS-NEXT-PRG.
             CALL PRG-NAME USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY DATA-FILE-IDS
             ON EXCEPTION GO TO AA110
             END-CALL.
            CANCEL PRG-NAME.
          IF NOT(WS-NEXT-PRG = SPACES)
              MOVE WS-NEXT-PRG       TO PRG-NAME
              GO TO AA102.

       AA105.
           IF WS-DEM = 1 AND WS-DAYS > 80
               CALL "UTP\SYS001" USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY
               CANCEL "UTP\SYS001".
             PERFORM RESTORE-SCREEN-3.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 1                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
             CLOSE NETWORK DEPART PARAM SHARED TXTRAN.
             GO TO AA199.

       AA110.
             MOVE SPACE              TO WS-ERR-MES.
             STRING "Program- " DELIMITED SIZE PRG-NAME DELIMITED " " " not on disk, press ANY key" DELIMITED SIZE INTO WS-ERR-MES.
             MOVE 27                 TO WS-MES-LINE.
             PERFORM ERROR-MESSAGE.
             GO TO AA105.

       AA199.
             EXIT.

       COPY "AA900.ALM".

      *    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *           S C R E E N ,   K E Y B O A R D   &   M O U S E
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё      SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3       Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё                      SCREEN-SHADOW                        Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To routine is used to display a shadow down the right and Ё
      *    Ё along the bottom of a pop-up box. The parameters that are Ё
      *    Ё required:                                                 Ё
      *    Ё          SHADE-ROW   - Top line of the box.               Ё
      *    Ё          SHADE-COL   - Left line of box.                  Ё
      *    Ё          SHADE-WIDTH - Width of the box.                  Ё
      *    Ё          SHADE-LINES - Height of box.                     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "FUNCTION.SCR".

      *    *************************************************************
      *    ****   T H I S   R O U T I N E   I S   U S E D   T O
      *           D I S P L A Y   E R R O R   M E S S A G E S
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё                      ERROR-MESSAGE                        Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To display the error message higher or lower (default is  Ё
      *    Ё line 20) Move the line number which must be used as the   Ё
      *    Ё top line to WS-MES-LINE. The message to be displayed must Ё
      *    Ё be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "ERRORW.SCR".

       COPY "LOCKED.REC".

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё           ERASE SCREEN FROM SPECIFIED LOCATION            Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё Uses CRT-START as starting line and increases and CRT-END Ё
      *    Ё as the ending line. The screen is cleared with Column 1   Ё
      *    Ё and 80 containing "Ё" and columns 2 to 79 containing      Ё
      *    Ё spaces.                                                   Ё
      *    Ё                                                           Ё
      *    Ё  eg.                                                      Ё
      *    Ё   MOVE 8         TO CRT-START.                            Ё
      *    Ё   MOVE 28        TO CRT-END.                              Ё
      *    Ё   PERFORM ERASE-SCREEN.                                   Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY "CLEARW.CRT".

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё           R E A D   F I L E S   R O U T I N E S           Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       AC000              SECTION.

       COPY "PARAM.RD".

       COPY "APAC.HLP".

       BA000        SECTION 1.
       BA00.
             MOVE 6                  TO SHADE-ROW.
             MOVE 30                 TO SHADE-COL.
             MOVE 21                 TO SHADE-WIDTH.
             MOVE 19                 TO SHADE-LINES.
             DISPLAY S-MENU.
           IF WS0-POS < 2
               DISPLAY "(N/A)" AT 1346 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
           IF (WS0-LEV < 4) AND (WS0-JCP < 2)
               DISPLAY "(N/A)" AT 1446 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
           IF WS0-LEV < 3
              DISPLAY " Add Bus/Com (N/A)" AT 1733 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta
              DISPLAY " Switch Comp (N/A)" AT 1833 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta
              DISPLAY ".(N/A)" AT 2045 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta
              DISPLAY "(N/A)" AT 2146 WITH FOREGROUND-COLOR Cyan HIGHLIGHT BACKGROUND-COLOR Magenta.
      *    IF (DEB-ORD-SYS = "Y")
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.

       BA02.
             MOVE 7                  TO TOP-ROW.
             MOVE 22                 TO BOTTOM-ROW.
             MOVE 6                  TO SCREEN-LIN.
             MOVE 31                 TO SCREEN-COL.
             MOVE 18                 TO STRING-LENGTH.
             PERFORM SAVE-ATTR.

       BA05.
      *
      *    ****    W A I T   A N D   R E A D   O N E   K E Y
      *                       D E P R E S S I O N
      *
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BA10.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN F1-KEY       PERFORM HELP-ROUTINE
                      
                 WHEN OTHER        PERFORM AA900-ALARM
               END-EVALUATE
               GO TO BA05
           ELSE
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN UP-KEY       PERFORM MOVE-BLOCK-UP
                 
                 WHEN DOWN-KEY     PERFORM MOVE-BLOCK-DOWN
                 
                 WHEN MOUSE-KEY    PERFORM POSITION-MOUSE
                                   IF MOUSE-ENTER = "Y"
                                       MOVE "N"   TO MOUSE-ENTER
                                       GO TO BA15
                                   END-IF
                                   IF MOUSE-Y = 49
                                       IF MOUSE-X = 5
                                           MOVE 5 TO KEY-CODE-1
                                           GO TO BA10
                                       ELSE
                                       IF MOUSE-X = 10
                                           MOVE 6 TO KEY-CODE-1
                                           GO TO BA10
                                       ELSE
                                       IF MOUSE-X > 36 AND < 44
                                           MOVE 0 TO KEY-CODE-1
                                           GO TO BA10
                                       ELSE
                                       IF MOUSE-X > 54 AND < 60
                                           MOVE 0 TO KEY-CODE-1
                                           MOVE 1 TO KEY-TYPE
                                           GO TO BA10
                                       END-IF
                                   END-IF
                                   
                 WHEN ENTER-KEY    GO TO BA15
                 
                 WHEN OTHER        PERFORM AA900-ALARM
               END-EVALUATE
               GO TO BA05
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X  TO WS-OPTION
               CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS
               IF (WS-OPTION < 1 OR > 9) AND (WS-OPTION < "A" OR > "F") AND (WS-OPTION NOT = "L")
                   PERFORM AA900-ALARM
                   GO TO BA05
               END-IF
               GO TO BA16
           ELSE
               PERFORM AA900-ALARM
               GO TO BA05.

       BA15.
             EVALUATE SCREEN-LIN
               WHEN 7      MOVE "1"    TO WS-OPTION
               
               WHEN 8      MOVE "2"    TO WS-OPTION
               
               WHEN 9      MOVE "3"    TO WS-OPTION
               
               WHEN 10     MOVE "4"    TO WS-OPTION
               
               WHEN 11     MOVE "5"    TO WS-OPTION
               
               WHEN 12     MOVE "6"    TO WS-OPTION
               
               WHEN 13     MOVE "7"    TO WS-OPTION
               
               WHEN 14     MOVE "8"    TO WS-OPTION
               
               WHEN 15     MOVE "A"    TO WS-OPTION
               
               WHEN 16     MOVE "B"    TO WS-OPTION
               
               WHEN 17     MOVE "C"    TO WS-OPTION
               
               WHEN 18     MOVE "D"    TO WS-OPTION
               
               WHEN 19     MOVE "E"    TO WS-OPTION
               
               WHEN 20     MOVE "F"    TO WS-OPTION
               
               WHEN 21     MOVE "L"    TO WS-OPTION
               
               WHEN 22     MOVE "9"    TO WS-OPTION
               
               WHEN OTHER  PERFORM AA900-ALARM
                           GO TO BA05
             END-EVALUATE.
             GO TO BA18.

       BA16.
             PERFORM CLEAR-BLOCK.
             EVALUATE WS-OPTION
               WHEN "9"    MOVE 22     TO SCREEN-LIN
               
               WHEN "1"    MOVE 7      TO SCREEN-LIN
               
               WHEN "2"    MOVE 8      TO SCREEN-LIN
               
               WHEN "3"    MOVE 9      TO SCREEN-LIN
               
               WHEN "4"    MOVE 10     TO SCREEN-LIN
               
               WHEN "5"    MOVE 11     TO SCREEN-LIN
               
               WHEN "6"    MOVE 12     TO SCREEN-LIN
               
               WHEN "7"    MOVE 13     TO SCREEN-LIN
               
               WHEN "8"    MOVE 14     TO SCREEN-LIN
               
               WHEN "A"    MOVE 15     TO SCREEN-LIN
               
               WHEN "B"    MOVE 16     TO SCREEN-LIN
               
               WHEN "C"    MOVE 17     TO SCREEN-LIN
               
               WHEN "D"    MOVE 18     TO SCREEN-LIN
               
               WHEN "E"    MOVE 19     TO SCREEN-LIN
               
               WHEN "F"    MOVE 20     TO SCREEN-LIN
               
               WHEN "L"    MOVE 21     TO SCREEN-LIN
             END-EVALUATE.
             PERFORM SAVE-ATTR.
             PERFORM MARK-BLOCK.

       BA18.
           IF WS-DAYS > 91
               DISPLAY "This package must be legalised - PHONE JW Lemmon - 082-566-3095" AT 2302 WITH FOREGROUND-COLOR Red HIGHLIGHT
               GO TO BA05.
             EVALUATE WS-OPTION
               WHEN "9"    GO TO BA999
               
               WHEN "1"    MOVE "MNU\STPMENU"    TO PRG-NAME
               
               WHEN "2"    MOVE "MNU\CRPMENU"    TO PRG-NAME
               
               WHEN "3"    MOVE "MNU\DTPMENU"    TO PRG-NAME
               
               WHEN "4"    MOVE "MNU\GLPMENU"    TO PRG-NAME
               
               WHEN "5"    MOVE "MNU\SLPMENU"    TO PRG-NAME
               
               WHEN "6"    IF WS0-POS = 0
                               MOVE "MNU\NOPMENU"  TO PRG-NAME
                           ELSE
                               MOVE "MNU\SALMENU"  TO PRG-NAME
                               
               WHEN "7"    IF (WS0-LEV < 4) AND (WS0-JCP < 2)
                               MOVE "MNU\NOPMENU"  TO PRG-NAME
                           ELSE
                               MOVE "MNU\JCPMENU"  TO PRG-NAME
                               
               WHEN "8"    MOVE "MNU\REPMENU"      TO PRG-NAME
               
               WHEN "A"    MOVE "MNU\UTILMENU"     TO PRG-NAME
               
               WHEN "B"    IF WS0-LEV < 3
                               MOVE "MNU\NOPMENU"  TO PRG-NAME
                           ELSE
                               MOVE "UTP\SYSNEW"   TO PRG-NAME

               WHEN "C"    IF WS0-LEV < 3
                               MOVE "MNU\NOPMENU"  TO PRG-NAME
                           ELSE
                           IF (WS0-DBLEV < 3) AND (WS0-STLEV < 3) AND (WS0-GLLEV < 3) AND (WS0-CRLEV < 3)
                               MOVE "MNU\NOPMENU"  TO PRG-NAME
                           ELSE
                               GO TO BA20
                               
               WHEN "D"    MOVE "UTP\SPOOLPRN"     TO PRG-NAME
               
               WHEN "E"    IF WS0-LEV < 3
                               MOVE "MNU\NOPMENU"  TO PRG-NAME
                           ELSE
                               MOVE "UTP\PRNCHQ"   TO PRG-NAME
                               
               WHEN "F"    IF WS0-LEV < 3
                               MOVE "MNU\NOPMENU"  TO PRG-NAME
                           ELSE
                               MOVE "SLP\ORDSPC"   TO PRG-NAME
                               
               WHEN "L"    MOVE "UTP\USR001"       TO PRG-NAME
             END-EVALUATE.
             PERFORM AA100.
             DISPLAY MENU-INSTRUCT.
             GO TO BA05.
      *    *************************************************************
      *    ****           S W I T C H   C O M P A N I E S
      *    *************************************************************
       BA20.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 0                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
             PERFORM ERASE-SCREEN.

       BA25.
      *    *************************************************************
      *    ****    U P D A T E   P A R A M E T E R   F I L E
      *          Su
      *    *************************************************************
             OPEN I-O PARAM.
           IF RUNTIME-ERROR
               IF FLE-LOCKED OR FLE-LIMIT
                   DISPLAY "WAITING" AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                   GO TO AA05.
           IF WS-STATUS NOT = "00"
               MOVE 15               TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
             MOVE ZERO               TO PAR-LOGON.
             REWRITE PAR-RECORD1.
             MOVE 3                  TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
             UNLOCK PARAM.
             CLOSE PARAM APACFIDS.
             MOVE "ANYBODY"          TO WS0-NAME.
             MOVE "X"                TO WS-INSTR.
             PERFORM ZA000-INIT.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
             GO TO BA00.

       BA999.
             EXIT.

       CA000        SECTION 2.
       CA00.
             PERFORM SAVE-SCREEN-3.
             PERFORM ERASE-SCREEN.
             DISPLAY SD-CONF.
             MOVE 8                  TO SHADE-ROW.
             MOVE 14                 TO SHADE-COL.
             MOVE 56                 TO SHADE-WIDTH.
             MOVE 19                 TO SHADE-LINES.
             PERFORM SCREEN-SHADOW.
             ACCEPT WS-OPTION AT 2534 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Red AUTO.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF WS-OPTION = "Y"
               GO TO CA05.
             DISPLAY " SWITCH THIS MESSAGE OFF ?                            " AT 5014 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Red.
             ACCEPT WS-OPTION AT 5041 WITH FOREGROUND-COLOR Brown HIGHLIGHT BACKGROUND-COLOR Red AUTO.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
           IF NOT(WS-OPTION = "Y")
               PERFORM RESTORE-SCREEN-3
               GO TO CA999.
             GO TO CA10.

       CA05.
             MOVE "8"                TO WS-OPTION.

       CA10.
             MOVE 3                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE "N"                TO PAR-NEW-SYS.
             REWRITE PAR-RECORD1.
             PERFORM RESTORE-SCREEN-3.

       CA999.
             EXIT.
      *    *************************************************************
      *             I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 9.
       ZA000-OPEN.
             ACCEPT W12-T-YMD FROM DATE YYYYMMDD.
             MOVE W12-CC             TO W12-CENT.
             MOVE W12-YY             TO W12-YEAR.
             MOVE W12-MM             TO W12-MONTH.
             MOVE W12-DD             TO W12-DAY.
             MOVE W12-TODAY          TO TODAY-DDMMYY.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
             CALL "MNU\BUP000" USING WS-PARID WS-USER-ID WS0-PROGRAMS W05-COMMAND DATA-FILE-IDS.
             CANCEL "MNU\BUP000".
           IF APAC-END
               STOP RUN.
             MOVE WS-APAC-VERSION    TO APAC-VERSION.
             MOVE WS-APAC-SYSTEM     TO APAC-SYSTEM.
             MOVE WS-COMPANY         TO CRT-COMPANY.
             MOVE WS-L-OR-N          TO W02-L-OR-N.
             MOVE WS-SYS-ID          TO W02-SYSID.
             MOVE TODAY-DDMMYY       TO W12-TODAY.
             MOVE WS-TERMINAL        TO CRT-TERMINAL.
             OPEN I-O APACFIDS.
           IF APAC-INIT
               PERFORM ZA100
               GO TO ZA000-OPEN.
             PERFORM ZA60.
           IF (WS-DEM = 1 AND WS-DAYS < 66) OR (WS-DEM > 1)
               CALL "UTP\SYS001" USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY
               CANCEL "UTP\SYS001".
             CALL "UTP\USR001" USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY DATA-FILE-IDS.
             CANCEL "UTP\USR001".
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё D o   n o t   d i s p l a y   E R R O R   m e s s a g e s Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
             MOVE 2                  TO USER-ACTION.
             MOVE 44                 TO USER-NUMBER.
             MOVE 1                  TO USER-SETTING.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Setup various keys and the mouse for use with menus and  Ё
      *    Ё  other required user interface functions and procedures.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

             COPY "FUNCTION.PRO".

       ZA00.
             PERFORM ERASE-SCREEN.
           IF (WS-DEM = 1) AND (WS-DAYS > 85)
               COMPUTE WS-REG = 15 - (WS-DAYS - 65)
               DISPLAY S-LEGAL
               GO TO ZA999.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё         Setup the file ID's including path names          Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *    *************************************************************
      *    ****             P A R A M E T E R   F I L E             ****
      *    *************************************************************
       ZA00AA.
             MOVE "PARAM"            TO AFID-KEY.

       ZA00-READ-APACFIDS.
             READ APACFIDS WITH IGNORE LOCK KEY IS AFID-KEY.
           IF WS-STATUS = "00"
               GO TO ZA00-READ-APACFIDS-EXIT.
             STRING "Missing " DELIMITED SIZE AFID-KEY DELIMITED BY " " " file ID - Status " DELIMITED SIZE WS-STATUS DELIMITED SIZE INTO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.
             STOP RUN.

       ZA00-READ-APACFIDS-EXIT.
             EXIT.

       ZA00A-CONTINUE.
             MOVE AFID-PATH          TO W02-PARAM.

       ZA00B.
             OPEN I-O PARAM.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
               MOVE 15               TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             MOVE 3                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      *    *************************************************************
      *    ****               C O N T R O L   F I L E               ****
      *    *************************************************************
            MOVE "NETWORK"  TO AFID-KEY.
            PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
            MOVE AFID-PATH  TO W02-NETWORK.
      *    *************************************************************
      *    ****            D E P A R T M E N T   F I L E            ****
      *    *************************************************************
            MOVE "DEPART"  TO AFID-KEY.
            PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
            MOVE AFID-PATH  TO W02-DEPART.
      *    *************************************************************
      *    ****       N E T W O R K   (S H A R E D)   F I L E       ****
      *    *************************************************************
            MOVE "SHARED"  TO AFID-KEY.
            PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
            MOVE AFID-PATH  TO W02-SHARED.
      *    *************************************************************
      *    ****       V A L U E   A D D E D   T A X   F I L E       ****
      *    *************************************************************
            MOVE "TXTRAN"  TO AFID-KEY.
            PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
            MOVE AFID-PATH  TO W02-TXTRAN.
            MOVE PAR-CRDX  TO WS-CARDEX.
      *      MOVE "X"   TO WS-CRDMTH.
            MOVE PAR-NEW-SYS  TO WS-OPTION.
      *    *************************************************************
      *    ****    C O N T R O L   F I L E
      *    *************************************************************
            OPEN I-O NETWORK.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
               MOVE 2                TO WS-F-ERROR
               PERFORM OPEN-ERROR.
      *    *************************************************************
      *    ****    O P E N   T H E   S H A R E D   F I L E
      *    *************************************************************
            OPEN I-O SHARED.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS = "05"
               PERFORM ZA05 THRU ZA15.
           IF WS-STATUS NOT = "00"
               MOVE 37               TO WS-F-ERROR
               PERFORM OPEN-ERROR.
             MOVE 5                  TO WS-SHARED.
             READ SHARED WITH LOCK.
           IF NOT(SHR-RUN-DATE = W12-TODAY)
               MOVE W12-TODAY  TO SHR-RUN-DATE
               MOVE ZERO  TO SHR-UPDATE-SEQ
               REWRITE SHR-RECORD.
             UNLOCK SHARED.
      *    *************************************************************
      *    ****    D E P A R T M E N T  /   S A L E S  L E D G E R
      *                            F I L E
      *    *************************************************************
       ZA01.
             OPEN I-O DEPART.
           IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR (WS-STATUS = "39")
               CLOSE PARAM
               PERFORM SAVE-SCREEN-3
               CALL "UTP\DPTREB" USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY DATA-FILE-IDS
               CANCEL "UTP\DPTREB"
               PERFORM RESTORE-SCREEN-3
               CLOSE DEPART
               OPEN I-O PARAM NETWORK
               GO TO ZA01.
           IF RUNTIME-ERROR IF FLE-LOCKED
                                GO TO ZA200
                            ELSE
                            IF FLE-LIMIT
                                GO TO ZA49.
           IF NOT(WS-STATUS = "00")
               MOVE 7                TO WS-F-ERROR
               PERFORM OPEN-ERROR.

       ZA02.
             OPEN I-O TXTRAN.
           IF (WS-STAT1 = "9" AND IDX-CORRUPT) OR (WS-STATUS = "39")
               CLOSE PARAM
               PERFORM SAVE-SCREEN-3
               CALL "UTP\TAXREB" USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY DATA-FILE-IDS
               CANCEL "UTP\TAXREB"
               PERFORM RESTORE-SCREEN-3
               OPEN I-O PARAM NETWORK
               GO TO ZA02.
           IF RUNTIME-ERROR IF FLE-LOCKED
                                GO TO ZA200
                            ELSE
                            IF FLE-LIMIT
                                GO TO ZA49
                            ELSE
                       IF IDX-CORRUPT
                                MOVE 32  TO WS-F-ERROR
                                GO TO ZA50.
           IF WS-STATUS NOT = "00"
              MOVE 32   TO WS-F-ERROR
              IF WS-STATUS = "39"
                  GO TO ZA50
              ELSE
                  PERFORM OPEN-ERROR.
            MOVE 251   TO WS-PARKEY.
            PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      *    *************************************************************
      *    ****    C H E C K   I F   C O N V E R S I O N S
      *                   R E Q U I R E D
      *     As screen layouts and CALL parameters have changed as from
      *     Version 9, all previous convertions are now remarks.
      *    *************************************************************
             MOVE 1                  TO WS-NETKEY.
             READ NETWORK.
      *    IF DEB-VER < "8.09"
      *        PERFORM SAVE-SCREEN-3
      *        CALL "CNV\DEBCON" USING WS-PARID WS-USER-ID
      *        CANCEL "CNV\DEBCON"
      *        PERFORM RESTORE-SCREEN-3.
      *    IF DEB-VER < "8.11"
      *        PERFORM SAVE-SCREEN-3
      *        CALL "UTP\JOBREB" USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY
      *        CALL "CNV\DEBJOB" USING WS-PARID WS-USER-ID
      *        CANCEL "CNV\DEBJOB"
      *        PERFORM RESTORE-SCREEN-3.
      *    IF DEB-VER < "8.12"
      *        PERFORM SAVE-SCREEN-3
      *        CALL "CNV\DEBAGE" USING WS-PARID WS-USER-ID
      *        CANCEL "CNV\DEBAGE"
      *        PERFORM RESTORE-SCREEN-3.
      *    IF DEB-VER < "8.13"
      *        PERFORM SAVE-SCREEN-3
      *        CALL "CNV\DEBREST" USING WS-PARID WS-USER-ID
      *        CANCEL "CNV\DEBREST"
      *        PERFORM RESTORE-SCREEN-3.
           IF DEB-VER < "9.04"
               PERFORM SAVE-SCREEN-3
               CALL "CNV\DEBCONV" USING WS-PARID WS-USER-ID DATA-FILE-IDS
               CANCEL "CNV\DEBCONV"
               PERFORM RESTORE-SCREEN-3.
             MOVE 2                  TO WS-NETKEY.
             READ NETWORK.
      *    IF P-VER < "8.12"
      *        PERFORM SAVE-SCREEN-3
      *        CALL "CNV\PURCON" USING WS-PARID WS-USER-ID
      *        CANCEL "CNV\PURCON"
      *        PERFORM RESTORE-SCREEN-3.
           IF P-VER < "9.04"
               PERFORM SAVE-SCREEN-3
               CALL "CNV\CREDCONV" USING WS-PARID WS-USER-ID DATA-FILE-IDS
               CANCEL "CNV\CREDCONV"
               PERFORM RESTORE-SCREEN-3.
      *      MOVE 3   TO WS-NETKEY.
      *      READ NETWORK.
      *    IF STK-VER < "8.09"
      *        PERFORM SAVE-SCREEN-3
      *        CALL "CNV\SRDCON" USING WS-PARID WS-USER-ID
      *        CANCEL "CNV\SRDCON"
      *        CALL "UTP\CARREB" USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY
      *        PERFORM RESTORE-SCREEN-3.
      *    IF STK-VER < "8.14"
      *        PERFORM SAVE-SCREEN-3
      *        CALL "CNV\CARCON" USING WS-PARID WS-USER-ID
      *        CANCEL "CNV\CARCON"
      *        CALL "UTP\CARREB" USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY
      *        PERFORM RESTORE-SCREEN-3.
             GO TO ZA999.

       ZA05.
             MOVE 1                  TO WS-SHARED WS-NETKEY.
       ZA10.
             READ NETWORK IGNORE LOCK.
             MOVE NET-RECORD  TO SHR-RECORD.
           IF WS-NETKEY = 5
               MOVE "N"              TO SHR-DEBT SHR-CRED SHR-STOCK SHR-GLED SHR-LAY.
             WRITE SHR-RECORD.
           IF WS-NETKEY < 5
               ADD 1                 TO WS-SHARED WS-NETKEY
               GO TO ZA10.

       ZA15.
             EXIT.

       COPY "ZA49.PRO".

       ZA50.
             MOVE "Rebuild TAX file" TO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       ZA51.
             EXIT PROGRAM.
      *    *************************************************************
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *    *************************************************************
       ZA60.
             MOVE WS-COMPANY         TO CRT-COMPANY.
      *    *************************************************************
      *    ****    I N I T I A L I Z E  F I L E S
      *    *************************************************************
       ZA100.
             MOVE WS-NEXT-PRG        TO PRG-NAME.
             MOVE SPACES             TO WS-NEXT-PRG.
             CALL PRG-NAME USING WS-PARID WS-USER-ID WS0-PROGRAMS WS0-SECURITY
                  ON EXCEPTION STOP RUN.
             CANCEL PRG-NAME.
           IF WS-NEXT-PRG NOT = SPACES
               GO TO ZA100.

       ZA200.

       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.

       ZA999.
             EXIT.

       I-O-ERRORS      SECTION  10.

       COPY "ERRORS.PRO".
      
       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK  TO WS-FILE
                               MOVE WS-NETKEY    TO WS-KEY
                               
               WHEN 7          MOVE W02-DEPART   TO WS-FILE
                               MOVE DPT-CODE     TO WS-KEYX
                               
               WHEN 15         MOVE W02-PARAM    TO WS-FILE
                               MOVE WS-PARKEY    TO WS-KEY
                               
               WHEN 32         MOVE W02-TXTRAN   TO WS-FILE
                               MOVE TAX-KEY      TO WS-KEYX
                               
               WHEN 37         MOVE W02-SHARED   TO WS-FILE
                               MOVE WS-SHARED    TO WS-KEY
                               
               WHEN OTHER      MOVE "ERROR"      TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".
