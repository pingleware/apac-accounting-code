      $SET LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   ********  **     **  *******    ****    ********   *
      *   **	**     **     ***   ***  **	   **  **	 **    *
      *   **	       **     ** * * **  **	  **	**	**     *
      *    ******      **     **  *  **  *****	  **	**     **      *
      * 	**     **     **     **  **	  **	**     **      *
      *   **	**     **     **     **  **	   **  **      **      *
      *    ******      **     **     **  **	    ****       **      *
      *                                                                *
      *     ENGLISH						       *
      * 							       *
      * 	A C C O U N T S	  R E C E I V A B L E	/	       *
      *                                                                *
      *     D E B T O R	  S T A T E M E N T S	P R O G R A M	       *
      * 							       *
      * 	       ( C O N S O L - U S B)			       *
      *                                                                *
      *     VERSION 8.15.02 - November 2011			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  May 2006	- New field included in Accounts Receivable    *
      * 		  for recording number of outstanding Jobs     *
      * 		  linked to the account. Additional filler     *
      * 		  added for future expansion.		       *
      * 							       *
      *  October 2006	- Amended Accounts receivable ageing from a    *
      * 		  specific date to the actual date on which    *
      * 		  statements and month-end is run.	       *
      * 							       *
      *  Feb 2007	- New file (DEBDEP/AR_REST.DAT) - Debtor sales *
      * 		  restrictions file has been included which    *
      * 		  will allow for departments to de excluded or *
      * 		  only selected departments allowed. This will *
      * 		  allow the user the abillity to allow or bar  *
      * 		  sales of selected departments. An indicator  *
      * 		  03  DEB-RESTRICT    PIC  X(01).	       *
      *			      88  SALES-EXCLUDED	VALUE "E".     *
      *			      88  SALES-ALLOWED 	VALUE "A".     *
      *			      88  NO-SALES-RESTRICTIONS VALUE "N".     *
      * 		  has been included in the Debtor Record.      *
      * 							       *
      *  Dec 2007	- Program changed to allow the user to print   *
      * 		  statements for a range of Debtor accounts.   *
      * 		  The main objective is to allow statements to *
      * 		  printed on more than one work station simil- *
      * 		  taneously, allowing a shared work load to be *
      * 		  used to print large debtor files.	       *
      * 		  See SECTION'S XB300, XB400 AND XB500.        *
      * 							       *
      *  October 2008	- Amended Accounts receivable ageing to create *
      * 		  duplicate files of the accounts and trans-   *
      * 		  actions for printing of copy satements when  *
      * 		  and if required. The file names are AR_Ayymm *
      * 		  and AR_Tyymm, where yymm is replaced with    *
      * 		  the year and month.			       *
      * 							       *
      *  March 2010	- Include facility to print to a USB printer   *
      * 		  via the Windows print using the relavent     *
      * 		  print API's supplied with MF COBOL. An entry *
      * 		  in the APACFIDS file has been included for   *
      *			  the system to access the bitmap file that is *
      * 		  used for the statement and is accessed using *
      * 		  "STMBMP" as the key.			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STMF07.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   DECEMBER 1994.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1994 - 2012
				     by James William Lemmon.
				       (Id No. 4412165050082).

		   All rights reserved.

		   e-mail jwlemmon@gmail.com.

000050 SECURITY.
		   This program is free software; you can redistribute
		   it and/or modify it under the terms of the GNU General
		   Public License as published by the Free Software
		   Foundation; either version 2 of the License, or (at
		   your option) any later version.

		   This program is distributed in the hope that it will
		   be useful, but WITHOUT ANY WARRANTY; without even the
		   implied warranty of MERCHANTABILITY or FITNESS FOR A
		   PARTICULAR PURPOSE.	See the GNU General Public License
		   for more details.

		   You should have received a copy of the GNU General
		   Public License along with this program; if not, write
		   to the Free Software Foundation, Inc., 59 Temple Place
		   - Suite 330, Boston, MA 02111-1307, USA.

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080                 CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

       COPY DBHIST.SL.

000160 COPY DBTCON.SL.

000160 COPY DBTRAN.SL.

000140 COPY DEBMEM.SL.

000140 COPY DEBTOR.ISL.

       COPY DTHIST.SL.

000120 COPY PARAM.SL.

       COPY SHARED.SL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

       COPY DBHIST.FDE.

000290 COPY DBTCON.FDE.

000290 COPY DBTRAN.FDE.

000140 COPY DEBMEM.FDE.

000270 COPY DEBTOR.FDE.

       COPY DTHIST.FDE.

000250 COPY PARAM.FDE.

       COPY SHARED.FDE.

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   ** 
      *         **    *    **   **    **   **   **    **  **  
      *         **   ***   **   **    **   ******     *****   
      *          ** ** ** **    **    **   **   **    **  **  
      *           ***   ***     **    **   **    **   **   ** 
      *            *     *       ******    **    **   **    **
      *
002780 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
002790 77  WS-SUB-1	   PIC	9(04)	 COMP-5.
002800 77  WS-SUB-2	   PIC	9(04)	 COMP-5.
002810 77  WS-SUB-3	   PIC	9(04)	 COMP-5.
002820 77  WS-SUB-4	   PIC	9(04)	 COMP-5.
002830 77  WS-SUB-5	   PIC	9(04)	 COMP-5.
002840 77  WS-SUB-6	   PIC	9(04)	 COMP-5.
002850 77  WS-IXS1	   PIC	9(04)	 COMP-5.
002860 77  WS-IXS2	   PIC	9(04)	 COMP-5.
002870 77  WS-IXS3	   PIC	9(04)	 COMP-5.
002880 77  WS-IXS4	   PIC	9(04)	 COMP-5.
002890 77  WS-MSUB	   PIC	9(04)	 COMP-5.
002900 77  WS-PARKEY	   PIC	9(04)	 COMP-5.
002910 77  WS-DEPKEY	   PIC	9(04)	 COMP-5.
002930 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002940 77  WS-WRKEY	   PIC	9(04)	 COMP-5.
002960 77  WS-NTP	   PIC	9(01).
002970 77  WS-LINES	   PIC	9(02)	 VALUE 34.
002980 77  WS-PAGE	   PIC	9(04)	 COMP-5.
002990 77  WS-OPTION	   PIC	X(01).
002990 77  WS-AGE	   PIC	X(01).
002990 77  WS-SEQ	   PIC	X(01).
003000 77  WS-CREDIT	   PIC	X(01).
002990 77  WS-ZERO	   PIC	X(01).
002990 77  WS-RANGE	   PIC	X(01).
       77  WS-NUM	   PIC	Z9.
003020 77  WS-ADDR	   PIC	X(01).
       77  WS-END	   PIC	X(01)	 VALUE "N".
       77  WS-REF	   PIC	X(01).
       77  WS-STMT	   PIC	X(01).
       88  PREVIOUS-STATEMENT		 VALUE "P".
003030 77  WS-ERROR	   PIC	9(01).
003040 77  WS-ERROR1	   PIC	9(01).
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
003110 77  WS-EXPAND	   PIC	X(01)	 VALUE " ".
003180 77  WS-COMP	   PIC	9(01).
003180 77  WS-STM	   PIC	9(01).
003190 77  WS-TYPE	   PIC	X(01).
003200 77  WS-HEAD	   PIC	X(01).
003240 77  WS-MESS	   PIC	X(02).
003250 77  WS-ACNO	   PIC	X(06).
003250 77  WS-ASTART	   PIC	X(06).
003250 77  WS-AEND	   PIC	X(06).
003260 77  WS-S1	   PIC	9(04)	 COMP-5.
003270 77  WS-S2	   PIC	9(04)	 COMP-5.
003280 77  WS-S3	   PIC	9(04)	 COMP-5.
003290 77  WS-S4	   PIC	9(04)	 COMP-5.
003300 77  WS-S5	   PIC	9(04)	 COMP-5.
003310 77  WS-S6	   PIC	9(04)	 COMP-5.
003320 77  WS-S7	   PIC	9(04)	 COMP-5.
003330 77  WS-S8	   PIC	9(04)	 COMP-5.
003340 77  WS-CHK	   PIC	X(33)	 VALUE
003350                     "ENTER if correct - N if incorrect".
003360 77  WS-ERR1	   PIC	X(22)	 VALUE
003370                     "Invalid account number".
003380 77  WS-ERR2	   PIC	X(09)	 VALUE "No record".
003420 77  WS-AC	   PIC	X(07)	 VALUE "Account".
       77  TODAY-DDMMYY	   PIC	9(08)	 COMP-5.
       77  WS-USUB	   PIC	9(04)	 COMP-5.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Banking details for Clients to use for payments via EFT.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       01  WS-BANK-DETAILS.
	   03  WS-PRINT-BNK	 PIC  X(01)    VALUE "N".
	   03  WS-BNK-NME	 PIC  X(30).
	   03  WS-BRANCH.
	       05  FILLER	 PIC  X(13)    VALUE "Branch Code: ".
	       05  WS-BR-CODE	 PIC  9(06).
	   03  WS-ACT-TYP	 PIC  X(01).
	   03  WS-BANK-ACCOUNT.
	       05  WS-ACT-TYPE	 PIC  X(13)   VALUE SPACES.
	       05  WS-ACT-NO	 PIC  X(12).
      /
002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002430     03  WS-TOP-LNE2.
002440         05  WS-TCH  PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002450	   03  WS-MID-LNE.
002460         05  WS-MCHR PIC X(01) OCCURS 80.
	   03  WS-MDDT-LNE REDEFINES WS-MID-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
002450	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "Ё".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "╟".
	       05  FILLER      PIC  X(01) VALUE "Ё".
002470	   03  WS-BOT-LNE.
002480         05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC X(01) OCCURS 80.
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
           03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

003440 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "STM".
	   03  WS-PROG	       PIC  X(03) VALUE "F06".

003460 01  W00-TRAN-CODES.
003470     03  W00-TRAN-TYPES  OCCURS 100.
003480         05  W00-TRAN-CODE PIC 9(02)   COMP-5.
003490         05  W00-ACTION  PIC 9(02)     COMP-5.
003500         05  W00-E-DESC  PIC X(12).
003510         05  W00-A-DESC  PIC X(12).

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003520 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DBHIST.ID.

       COPY DEBTOR.ID.

       COPY DBTCON.ID.

       COPY DBTRAN.ID.

000140 COPY DEBMEM.ID.

       COPY DTHIST.ID.

       COPY PARAM.ID.

       COPY SHARED.ID.

      *  зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *  Ё  PRINTER ROUTINES FOR USB & WINDOWS PRINTERS THAT DO NOT  Ё
      *  Ё  USE STANDARD RAW CODE FOR FORMATING 		     Ё
      *  юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      ******************************************************************
      * KEY							       *
      * 							       *
      * Descriptions for all of the call-by-name routines appear       *
      * alphabetically. Each description contains the routine name and *
      * function followed by these sections (as appropriate):	       *
      * 							       *
      * SYNTAX:     Shows the CALL statement you could use to call     *
      * 	    the	routine.				       *
      * 							       *
      * 	    The optional RETURNING clause is also shown. Every *
      * 	    routine returns a value showing the result of the  *
      * 	    operation. Unless otherwise indicated, zero        *
      * 	    indicates success, nonzero indicates failure. This *
      * 	    value is left in the data item specified in the    *
      * 	    RETURNING clause, in this reference, status-code.  *
      * 	    If this clause is omitted, the value is left in    *
      * 	    the special register RETURN-CODE. (If call-        *
      * 	    convention bit two is set, RETURN-CODE is not      *
      * 	    changed.)					       *
      * 							       *
      * 	    status-code must be a numeric data item capable of *
      * 			holding positive values from 0 to      *
      * 			65535; for example, PIC X(2) COMP-5.   *
      * 							       *
      * 	    The name of the routine must be coded in upper     *
      * 	    case.					       *
      * 							       *
      * PARAMETERS: Describes any parameters shown in the RETURNING    *
      * 	    and USING clause. A parameter enclosed in brackets,*
      * 	    for example, [parameter1] is optional and might    *
      * 	    not be needed for all forms of the routine.        *
      * 							       *
      * ON ENTRY:   Indicates which of the parameters shown are passed *
      * 	    on entry.					       *
      * 							       *
      * ON EXIT:    Indicates which of the parameters shown are        *
      * 	    returned on exit.				       *
      * 							       *
      * 	    Where bits of one or more bytes are referenced,    *
      * 	    bit 0 is the least significant (rightmost) bit.    *
      * 							       *
      * COMMENTS:   Provides any additional information necessary for  *
      * 	    the successful use of the routine.		       *
      *								       *
      *    PC_PRINT_FILE	Print a file			       *
      *    PC_PRINTER_CLOSE	Close a printer channel 	       *
      *    PC_PRINTER_CONTROL	Send a printer command to a printer    *
      *    PC_PRINTER_FREE_BMP	Free bitmap from memory 	       *
      *    PC_PRINTER_INFO	Get printer information 	       *
      *    PC_PRINTER_LOAD_BMP	Load bitmap into memory 	       *
      *    PC_PRINTER_OPEN	Open a printer channel		       *
      *    PC_PRINTER_SET_COLOR	Set printer color		       *
      *    PC_PRINTER_SET_FONT	Set printer font		       *
      *    PC_PRINTER_WRITE	Write text to a printer 	       *
      *    PC_PRINTER_WRITE_BMP	Write bitmap to a printer	       *
      * 							       *
      *================================================================*
      * 							       *
      *    PC_WIN_CLOSE_PRINTER	   Close a printer channel	       *
      *    PC_WIN_FREE_PBMP	   Free bitmap from memory	       *
      *    PC_WIN_LOAD_PBMP	   Load bitmap into memory	       *
      *    PC_WIN_OPEN_PRINTER	   Open a printer channel	       *
      *    PC_WIN_OPEN_PRINTER_EXT Extended open a printer channel     *
      *    PC_WIN_PRINT_FILE	   Print a file 		       *
      *    PC_WIN_PRINT_FILE_EXT   Extended print a file	       *
      *    PC_WIN_PRINTER_CONTROL  Send a printer control	       *
      *    PC_WIN_PRINTER_INFO	   Get printer information	       *
      *    PC_WIN_SET_PCOLOR	   Set printer color		       *
      *    PC_WIN_SET_PDEFAULT	   Set or reset default printer        *
      *    PC_WIN_SET_PFONT	   Set printer font		       *
      *    PC_WIN_WRITE_PBMP	   Write bitmap to a printer	       *
      *    PC_WIN_WRITE_PRINTER	   Write to a printer		       *
      * 							       *
      * ______________________________________________________________ *

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	      Parameters for PC_PRINTER_DEFAULT_NAME	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       01  Default-Printer-Flags           pic x(04) comp-5  Value 1.
       01  Default-Printer-Name.
	   03  Default-Printer-Name-Len    pic x(02) comp-5.
	   03  Default-Printer-Name-Text   pic x(80).

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	      Parameters for PC_PRINTER_DEFAULT_FONT	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       01  Default-Font-Family-Name.
	   03  Default-Font-Name-Len	   pic x(02) comp-5.
	   03  Default-Font-Name-Text	   pic x(80).
       01  Default-Font-Size               pic x(04) comp-5.
       01  Default-Font-Style              pic x(04) comp-5.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	    Parameters for PC_PRINTER_DEFAULT_PROPERTIES       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       01  Default-Properties-Flags      pic x(4) comp-5.
       01  Default-Properties.
	   03 pr-len	       pic  x(2) comp-5.
	   03 pr-papersize     pic s9(4) comp-5.
	   03 pr-paperlength   pic s9(4) comp-5.
	   03 pr-paperwidth    pic s9(4) comp-5.
	   03 pr-scale	       pic s9(4) comp-5.
	   03 pr-copies        pic s9(4) comp-5.
	   03 pr-papertray     pic s9(4) comp-5.
	   03 pr-printquality  pic s9(4) comp-5.
	   03 pr-color	       pic s9(4) comp-5.
	   03 pr-duplex        pic s9(4) comp-5.
	   03 pr-orientation   pic s9(4) comp-5.
	   03 pr-yresolution   pic s9(4) comp-5.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  Parameters for PC_PRINTER_OPEN	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       01  Printer-Handle     pic x(04) comp-5.
       01  Document-Title.
	   03  Title-Len      pic x(02) comp-5.
	   03  Title-Text     pic x(80).
       01  Printer-Flags      pic x(04) comp-5.
       01  Window-Handle      pic x(04) comp-5.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  Parameters for PC_PRINTER_WRITE	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

      * ---Printer-Handle (same as above)
       01  Print-Buffer       pic x(136).
       01  Print-Buffer-Len   pic x(04) comp-5.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	       Parameters for PC_PRINTER_LOAD_BMP	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      * ---Printer-Handle (same as above)
       01  BmpFilename	      pic x(64).
       01  BmpID	      pic x(04) comp-5.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		Parameters for PC_PRINTER_WRITE_BMP	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      * ---Printer-Handle (same as above)
      * ---BmpID	  (same as above)
       01  WriteBmpReserved   pic x(04) comp-5 value 0.
       01  BmpRow	      pic x(04) comp-5 value 1.
       01  BmpCol	      pic x(04) comp-5 value 6.
       01  BmpWidth	      pic x(04) comp-5 value 82.
       01  BmpHeigth	      pic x(04) comp-5 value 13.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		Parameters for PC_PRINTER_WRITE_BMP	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      * ---Printer-Handle (same as above)
      * ---BmpID	  (same as above)

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		Parameters for PC_PRINTER_SET_COLOR	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

      * ---Printer-Handle (same as above)
       01  fore-or-back       pic x(2) comp-5.
       01  color-red	      pic x(2) comp-5.
       01  color-green	      pic x(2) comp-5.
       01  color-blue	      pic x(2) comp-5.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		 Parameters for PC_PRINTER_CONTROL	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

      * ---Printer-Handle (same as above)
      * ---print-command:  1 - Abort printing and close printer
      * 		   2 - Throw a page
      * 		   3 - Flush the print buffers
      * 		   4 - Start a new line
      *

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  Parameters for PC_PRINTER_CLOSE	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      * ---Printer-Handle (same as above)
      *

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		    Parameters for PC_PRINT_FILE	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       01  Filename.
	   03  Filename-Len		       pic xx comp-5.
	   03  Filename-Text		       pic x(80).

       01  DocumentTitle.
	   03  DocumentTitle-Len	       pic xx comp-5.
	   03  DocumentTitle-Text	       pic x(15).
      * --- Printer-Flags (same as above)
      * --- Window-Handle (Same as above)

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  Parameters for PC_PRINTER_INFO	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       78 PrinterNameBufferBegin	value next.
       01 PrinterNameBuffer         pic x(255).
       78 PrinterNameBufferLength       value next
          - PrinterNameBufferBegin.
       78 PrinterTypeBufferBegin        value next.
       01 PrinterTypeBuffer         pic x(255).
       78 PrinterTypeBufferLength       value next
          - PrinterTypeBufferBegin.
       78 PrinterDeviceBufferBegin      value next.
       01 PrinterDeviceBuffer       pic x(255).
       78 PrinterDeviceBufferLength     value next
          - PrinterDeviceBufferBegin.
       78 PrinterLocationBufferBegin    value next.
       01 PrinterLocationBuffer     pic x(255).
       78 PrinterLocationBufferLength   value next
          - PrinterLocationBufferBegin.
       78 PrinterCommentBufferBegin     value next.
       01 PrinterCommentBuffer      pic x(255).
       78 PrinterCommentBufferLength    value next
          - PrinterCommentBufferBegin.
       78 PrinterPaperSizeBufferBegin   value next.
       01 PrinterPaperSizeBuffer    pic x(255).
       78 PrinterPaperSizeBufferLength  value next
          - PrinterPaperSizeBufferBegin.

       01  PrinterInformation.
	   03  PrinterInformationLength  pic xxxx comp-5.
	   03  DeviceContextHandle	 pic xxxx comp-5.
	   03  reserved 		 pic xxxx comp-5.
	   03  Orientation		 pic xxxx comp-5.
	   03  PageRowsCurrentFont	 pic xxxx comp-5.
	   03  PageColsCurrentFont	 pic xxxx comp-5.
	   03  RowsRemainingCurrentFont  pic xxxx comp-5.
	   03  MaxGraphicsHorizontalAxis pic xxxx comp-5.
	   03  MaxGraphicsVerticalAxis	 pic xxxx comp-5.
	   03  MinGraphicsHorizontalAxis pic xxxx comp-5.
	   03  MinGraphicsVerticalAxis	 pic xxxx comp-5.
      *
      *    -------  Horizontal
      *
	   03  CurrentCooridinateX	 pic xxxx comp-5.
      *
      *    -------  Vertical
      *
	   03  CurrentCooridinateY	 pic xxxx comp-5.
	   03  NumberOfCopies		 pic xx   comp-5.
      *
      *    -------  Draft, Low,Medium, High, Default or DPI
      *
	   03  PrintQuality		 pic xx   comp-5.
      *
      *    -------  Monochrome or Color
      *
	   03  Color			 pic x	  comp-5.
	   03  reserved 		 pic x	  comp-5.
	   03  DriverVersionNumber	 pic xx   comp-5.
	   03  PrinterName.
	       05  PrinterNameLength	     pic xxxx comp-5
                   value PrinterNameBufferLength.
	       05  p-PrinterNameBuffer			   pointer.
	   03  PrinterType.
	       05  PrinterTypeLength	     pic xxxx comp-5
                   value PrinterTypeBufferLength.
	       05  p-PrinterTypeBuffer			   pointer.
	   03  PrinterDevice.
	       05 PrinterDeviceLength	    pic xxxx comp-5
                  value PrinterDeviceBufferLength.
	       05 p-PrinterDeviceBuffer 		  pointer.
	   03  PrinterLocation.
	       05 PrinterLocationLength     pic xxxx comp-5
                  value PrinterLocationBufferLength.
	       05 p-PrinterLocationBuffer		  pointer.
	   03  PrinterComment.
	       05 PrinterCommentLength	    pic xxxx comp-5
                  value PrinterCommentBufferLength.
	       05 p-PrinterCommentBuffer		  pointer.
	   03  PrinterPaperSize.
	       05 PrinterPaperSizeLength    pic xxxx comp-5
                 value PrinterPaperSizeBufferLength.
	       05 p-PrinterPaperSizeBuffer		  pointer.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Used to display error messages for the Print Routines.   Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       01  Operation			   pic x(30).

       01   printer-control.

	   03  Default-Printer-Name.
	       05  Default-Printer-Name-Len  pic x(02) comp-5.
	       05  Default-Printer-Name-Text pic x(80).

	   03  Font-family.
	       05  Font-family-namelen	pic x(2) comp-5 value 80.
	       05  Font-family-name	pic x(80).

	   03  FontTypeFace    pic x(13) value "Courier New" & x"00".
	   03  font-style      pic x(4) comp-5 value 0.
	   03  font-size       pic x(4) comp-5 value 0.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		Set to print Я 5 characters per inch	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   03  font-5-CPI      pic x(4) comp-5 value 24.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		Set to print Я 8 characters per inch	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   03  font-8-CPI      pic x(4) comp-5 value 14.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		Set to print Я 12 characters per inch	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   03  font-12-CPI     pic x(4) comp-5 value 10.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		Set to print Я 15 characters per inch	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   03  font-15-CPI     pic x(4) comp-5 value 8.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		Set to print Я 16 characters per inch	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   03  font-16-CPI     pic x(4) comp-5 value 7.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		Set to print Я 19 characters per inch	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   03  font-19-CPI     pic x(4) comp-5 value 5.

           03  abort           pic x(4) comp-5 value 1.
           03  prn-control     pic x(4) comp-5 value 2.
	   03  prn-flags       pic x(4) comp-5 value 1.
           03  prn-command     pic x(4) comp-5 value 4.
	   03  prn-handle      pic x(4) comp-5.

	   03  prn-open        pic x(01) VALUE "C".

	   03  W02-PRINTER.
               05  W02-PRN     PIC  X(06).

           03  W02-SPOOLER.
               05  W02-WS      PIC  X(02).
               05  W02-US      PIC  X(01).
               05  W02-TERM    PIC  X(03).
               05  W02-US2     PIC  X(01).
               05  W02-REP     PIC  X(01).
               05  W02-P       PIC  X(01).
               05  W02-USER    PIC  X(03).

002140 01  STM-LINE.
002150	   03  STM-DET	       PIC X(96).

002140 01  ST7-LINE1 REDEFINES STM-LINE.
002150	   03  ST7-DET	       PIC X(96).

002160 01  ST7-LINE2 REDEFINES STM-LINE.
002170	   03  ST7-COMP.
002180	       05  ST7-EXCOMP  PIC X(36).
002190         05  FILLER      PIC X(36).
002200     03  FILLER          PIC X(24).

002160 01  ST7-LINE2A REDEFINES STM-LINE.
002170	   03  FILLER	       PIC  X(24).
002180	   03  ST7-HEADER      PIC  X(17).
002200	   03  FILLER	       PIC  X(55).

002230 01  ST7-LINE3 REDEFINES STM-LINE.
002240	   03  ST7-ADD	       PIC  X(30).
002250	   03  FILLER	       PIC  X(12).
002750	   03  ST7-TEL	       PIC  X(19).
002760	   03  ST7-TNO	       PIC  X(14).
002220	   03  FILLER	       PIC  X(21).

002290 01  ST7-LINE4 REDEFINES STM-LINE.
002350	   03  ST7-PC1	       PIC  X(08).
	   03  ST7-AH1	       PIC  X(12).
	   03  ST7-AC1	       PIC  X(06).
	   03  FILLER	       PIC  X(17).
	   03  ST7-STMNT.
	       05  ST7-AH2     PIC  X(06).
	       05  ST7-DATE    PIC  Z9/99/9999.
	       05  FILLER      PIC  X(16).
	       05  ST7-AH3     PIC  X(09).
	       05  ST7-PAGE    PIC  ZZZ9.
	   03  ST7-REMIT REDEFINES ST7-STMNT.
	       05  FILLER      PIC  X(13).
	       05  ST7-AH2R    PIC  X(06).
	       05  ST7-DATER   PIC  Z9/99/9999.
	       05  FILLER      PIC  X(16).
	   03  FILLER	       PIC  X(08).

002290 01  ST7-LINE5 REDEFINES STM-LINE.
	   03  FILLER	       PIC  X(04).
002300	   03  ST7-NAME.
002350	       05  ST7-PC2     PIC  X(08).
	       05  FILLER      PIC  X(32).
002310	   03  FILLER	       PIC  X(08).
002320	   03  ST7-MESS        PIC  X(42).
	   03  FILLER	       PIC  X(02).

002340 01  ST7-LINE6 REDEFINES STM-LINE.
	   03  FILLER	       PIC  X(01).
	   03  ST7-HD1.
	     05	 FILLER	       PIC  X(02).
002420	     05	 ST7-TDTE      PIC  Z9/99/9999.
002430	     05	 FILLER	       PIC  X(04).
	     05	 ST7-BFWD.
002440	       07  ST7-TDESC   PIC  X(12).
	       07  FILLER      PIC  X(04).
002470	       07  ST7-TREF    PIC  X(08).
002490	     05	 FILLER	       PIC  X(04).
002500	     05	 ST7-V1.
002510	       07  ST7-TDBT    PIC  Z(08)9.99 BLANK WHEN ZERO.
	     05  FILLER        PIC  X(04).
002520	     05	 ST7-PD	       PIC  X(03).
002500	     05	 ST7-V2.
002510	       07  ST7-TCRD    PIC  Z(08)9.99 BLANK WHEN ZERO.
	     05	ST7-CB	       PIC  X(01).
002520	     05	FILLER	       PIC  X(04).
002530	     05	ST7-V3.
002540	       07 ST7-TBAL     PIC  Z(08)9.99-.
002550	     05	FILLER	       PIC  X(02).

       01  ST7-LINE6-CLR REDEFINES STM-LINE.
	   03  FILLER	       PIC  X(01).
	   03  ST7-CLR-DT.
	       05  FILLER      PIC  X(14).
	   03  ST7-CLR-DSC.
	       05  FILLER      PIC  X(16).
	   03  ST7-CLR-RF.
	       05  FILLER      PIC  X(12).
	   03  ST7-CLR-DB.
	       05  FILLER      PIC  X(16).
	   03  ST7-CLR-CR.
	       07  FILLER      PIC  X(20).
	   03  ST7-CLR-BL.
	       05  FILLER      PIC  X(17).

002690 01  ST7-LINE7 REDEFINES STM-LINE.
	   03  FILLER	       PIC  X(15).
002700	   03  ST7-A1	       PIC  X(12).
002710	   03  ST7-A2	       PIC  X(10).
002720	   03  ST7-A3	       PIC  X(12).
002720	   03  ST7-A4	       PIC  X(12).
002720	   03  ST7-A5	       PIC  X(12).
002730	   03  FILLER	       PIC  X(23).

002600 01  ST7-LINE8 REDEFINES STM-LINE.
002610	   03  FILLER	       PIC  X(13).
	   03  ST7-INT	       PIC  Z(06)9.99-.
002610     03  FILLER          PIC  X(01).
002620	   03  ST7-3MTHS       PIC  Z(06)9.99-.
002610     03  FILLER          PIC  X(01).
002620	   03  ST7-2MTHS       PIC  Z(06)9.99-.
002630     03  FILLER          PIC  X(01).
002640	   03  ST7-1MTH        PIC  Z(06)9.99-.
002650     03  FILLER          PIC  X(01).
002660	   03  ST7-CUR	       PIC  Z(06)9.99-.
002670     03  FILLER          PIC  X(01).
002680	   03  ST7-DUE	       PIC  Z(06)9.99-.
002670	   03  FILLER	       PIC  X(01).
002680	   03  ST7-DUE2        PIC  Z(06)9.99-.


       01  W05-DEB-HISTORY.
	   03  W05-DBT-HIS.
	       05  W05-DBT  PIC  X(04) VALUE "AR_T".
	       05  W05-TYY  PIC  9(02).
	       05  W05-TMM  PIC  9(02).
	       05  FILLER   PIC  X(04) VALUE ".DAT".
	   03  W05-DEB-HIS.
	       05  W05-DEB  PIC  X(04) VALUE "AR_A".
	       05  W05-DEBH-ID.
		   07  W05-DYY PIC  9(02).
		   07  W05-DMM PIC  9(02).
	       05  W05-HIST-ID REDEFINES W05-DEBH-ID PIC  9(04).
	       05  FILLER   PIC  X(04) VALUE ".DAT".

003650 01  W05-LIST.
003660     03  W05-OPT         PIC 9(03).
003670     03  W05-TYPES REDEFINES W05-OPT.
003680         05  W05-COMP    PIC 9(01).
003690         05  W05-INDV    PIC 9(01).
003700         05  W05-INT     PIC 9(01).
      *    **** Consolodated Statements Y or N
	   03  W05-CON-STM     PIC  X(01).
      *    **** Consolodation Type A=All transactions, S=Summary
	   03  W05-CON-TYPE    PIC  X(01).
      *    **** Statements to be printed Y or N
	   03  W05-PRINT-CS    PIC  X(01).

003710*01  W10-SPARES.
004300*    03  W10-DATE	       PIC  X(10).
      *    03  W10-RDATE REDEFINES W10-DATE.
      *        05  W10-DTE     PIC  99/99/9999.
       01  W10-REF.
           03  W10-NUM         PIC  9(08).

       COPY W12.WS.

002730 01  W22-DATES.
002740	   03  W22-DTE1        PIC  9(08).
002750	   03  W22-DTE2        PIC  9(08).
002760     03  W22-DATE2 REDEFINES W22-DTE2.
	       05  W22-CC2     PIC  9(02).
002770	       05  W22-YY2     PIC  9(02).
002780	       05  W22-MM2     PIC  9(02).
002790	       05  W22-DD2     PIC  9(02).
002760	   03  W22-DATE3 REDEFINES W22-DTE2.
	       05  W22-CY2     PIC  9(04).
	       05  FILLER      PIC  9(04).
002800	   03  W22-DTE3        PIC  9(08).
002810     03  W22-DATE3 REDEFINES W22-DTE3.
002820	       05  W22-DD3     PIC  9(02).
002830	       05  W22-MM3     PIC  9(02).
	       05  W22-CC3     PIC  9(02).
002840	       05  W22-YY3     PIC  9(02).

003930 01  W25-CALCS.
002860     03  W25-RESULT      PIC 9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880         05  W25-WHOLE   PIC 9(05).
002890         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
003980     03  W25-TOTAL       PIC S9(07)V99 COMP-3.
003990     03  W25-VALUE       PIC S9(07)V99 COMP-3.

004000 01  W30-MESSAGES.
004010	   03  W30-MESS6.
	       05  W30-MESS	 OCCURS 2.
004020		   07  W30-SMESS PIC X(42) OCCURS 4.
	   03  W30-MESS2 REDEFINES W30-MESS6.
	       05  W30-MESS-A	 OCCURS 2.
		   07  W30-SMES  PIC X(42) OCCURS 4.
004030	   03  W30-AMESSAGES.
004040	       05  FILLER  PIC X(42) VALUE
004050			   "Ons het nog nie u betaling vir VERLEDE".
004060	       05  FILLER  PIC X(42) VALUE
004070			   "MAAND ontvang nie. Indien daar 'n rede is,".
004080	       05  FILLER  PIC X(42) VALUE
004090			   "word u vriendelik versoek om ons in kennis".
004100	       05  FILLER  PIC X(42) VALUE
			   "te stel.".
004160	       05  FILLER  PIC X(42) VALUE
004170			   "U rekening is nou TWEE MAANDE agterstallig".
004180	       05  FILLER  PIC X(42) VALUE
004190			   "en u word versoek om dringend aandag daar-".
004200	       05  FILLER  PIC X(42) VALUE
004230			   "aan te skenk".
004240	       05  FILLER  PIC X(42) VALUE SPACES.
004250	       05  FILLER  PIC X(42) VALUE
004260			   "U rekening is nou MEER AS TWEE MAANDE".
004270	       05  FILLER  PIC X(42) VALUE
004280			   "agterstallig! Tensy u rekening nie op".
004290	       05  FILLER  PIC X(42) VALUE
004320			   "datum gebring word nie, sal GEREGTILIKE".
004330	       05  FILLER  PIC X(42) VALUE
004340			   " STAPPE teen u geneem word.".
004370	       05  FILLER  PIC X(42) VALUE
004380			   "We do not appear to have received your".
004390	       05  FILLER  PIC X(42) VALUE
004400			   "payment for LAST MONTH. You are kindly".
004410	       05  FILLER  PIC X(42) VALUE
004440			   "requested to contact us if there is a".
004450	       05  FILLER  PIC X(42) VALUE
004460			   "problem with your account.".
004490	       05  FILLER  PIC X(42) VALUE
004500			   "Your account is now TWO MONTHS in arrear".
004510	       05  FILLER  PIC X(42) VALUE
004520			   "and we request that you give this matter".
004550	       05  FILLER  PIC X(42) VALUE
004560			   "your urgent attention".
004570	       05  FILLER  PIC X(42) VALUE SPACES.
004590	       05  FILLER  PIC X(42) VALUE
004600			   "Your account is now MORE THAN TWO MONTHS".
004610	       05  FILLER  PIC X(42) VALUE
004620			   "in arrear! Unless your account is brought".
004650	       05  FILLER  PIC X(42) VALUE
004660			   "up-to-date, LEGAL ACTION will be".
004650	       05  FILLER  PIC X(42) VALUE
			   "instituted.".
004690     03  W30-AMESS       REDEFINES W30-AMESSAGES.
004700         05  W30-ARRM    OCCURS 2.
004710		   07  W30-30D PIC X(168).
004720		   07  W30-60D PIC X(168).
004730		   07  W30-90D PIC X(168).
004740	   03  W30-STM.
             04  W30-STM6.
004750	       05  W30-STML    PIC X(42)  OCCURS 4.
             04  W30-STM2 REDEFINES W30-STM6.
	       05  W30-STL     PIC X(42)  OCCURS 4.

       01  W30-AGE-DATES.
	   03  W30-120	    PIC  9(08) COMP-3.
	   03  W30-90	    PIC  9(08) COMP-3.
	   03  W30-60	    PIC  9(08) COMP-3.
	   03  W30-30	    PIC  9(08) COMP-3.
	   03  W30-CUR	    PIC  9(08) COMP-3.

       01  W35-AGE-VALUES.
000230	   03  W35-CUR	    PIC S9(09)V99 COMP-3.
000240	   03  W35-30	    PIC S9(09)V99 COMP-3.
000250	   03  W35-60	    PIC S9(09)V99 COMP-3.
000260	   03  W35-90	    PIC S9(09)V99 COMP-3.
000270	   03  W35-120	    PIC S9(09)V99 COMP-3.
000270	   03  W35-150	    PIC S9(09)V99 COMP-3.
000270	   03  W35-INT	    PIC S9(09)V99 COMP-3.

004760 01  W40-COMPANY.
004770     02  W40-NAME1.
004780         03  W40-NAME.
004790             05  W40-ENTRY PIC X(07).
004800             05  FILLER  PIC X(33).
004810         03  FILLER      PIC X(32).
004820     02  W40-NAME2 REDEFINES W40-NAME1.
004830         03  W40-SURNAME PIC X(24).
004840         03  W40-FNAME   PIC X(24).
004850         03  W40-SNAME   PIC X(24).
004860     02  W40-NAME3 REDEFINES W40-NAME1.
004870         03  W40-NCHAR OCCURS 72 PIC X(01).
004880     02  W40-ADDRESS1.
004890         03  W40-ADD1    PIC X(30).
004900         03  W40-ADD2    PIC X(30).
004910         03  W40-ADD3    PIC X(30).
004920         03  W40-ADD4    PIC X(30).
004930     02  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
004940         03  W40-ACHAR OCCURS 120 PIC X(01).
004950     02  W40-POST        PIC 9(04).
004960     02  W40-TEL         PIC X(14).
004970     02  W40-HTEL        PIC X(14).
004980     02  W40-TAX REDEFINES W40-HTEL.
004990         03  W40-TAXNO   PIC 9(14)     COMP-3.
005000     02  W40-TITLE       PIC 9(04)     COMP-5.
005010     02  W40-LANG        PIC X(01).

       COPY W40.WS.

005020 01  W45-TRAN.
005030     03  W45-CODE        PIC 9(02).
005040     03  W45-ENG         PIC X(12).
005050     03  W45-AFR         PIC X(12).
005060     03  W45-ACT         PIC X(01).

       COPY FUNCTION.WS.

005170 COPY W50.WS.

       01  W55-STATS.
           03  W55-H1         PIC  X(09).
           03  W55-H2         PIC  X(09).
           03  W55-H3         PIC  X(09).
           03  W55-H4         PIC  X(09).
           03  W55-H5         PIC  X(09).
           03  W55-H6         PIC  X(09).
           03  W55-MONTHS.
               05  W55-MTH1   PIC  9(02) COMP-5.
               05  W55-MTH2   PIC  9(02) COMP-5.
               05  W55-MTH3   PIC  9(02) COMP-5.
               05  W55-MTH4   PIC  9(02) COMP-5.
               05  W55-MTH5   PIC  9(02) COMP-5.
               05  W55-MTH6   PIC  9(02) COMP-5.
           03  W55-VALUES.
               05  W55-VAL1   PIC S9(09)V99 COMP-3.
               05  W55-VAL2   PIC S9(09)V99 COMP-3.
               05  W55-VAL3   PIC S9(09)V99 COMP-3.
               05  W55-VAL4   PIC S9(09)V99 COMP-3.
               05  W55-VAL5   PIC S9(09)V99 COMP-3.
               05  W55-VAL6   PIC S9(09)V99 COMP-3.

005190 01  W60-NME-ADD.
005200     03  W60-NAME.
005210         05  W60-NCHAR OCCURS 40 PIC X(01).
005220     03  W60-ADDRESS.
005230         05  W60-ACHAR OCCURS 60 PIC X(01).

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-DNO        PIC X(06) OCCURS 18.

005330 01  W75-TITLES.
005340     03  W75-DESCRIP.
005350         05  FILLER      PIC X(28)       VALUE
005360                     "Mr     Mnr    Mrs    Mev    ".
005370         05  FILLER      PIC X(28)       VALUE
005380                     "Miss   Mej    Ms     Ms     ".
005390         05  FILLER      PIC X(28)       VALUE
005400                     "Dr     Dr     Rev    Ds     ".
005410         05  FILLER      PIC X(28)       VALUE
005420                     "Prof   Prof   The HonSy Edel".
005430         05  FILLER      PIC X(28)       VALUE
005440                     "                            ".
005450     03  W75-CODES REDEFINES W75-DESCRIP.
005460         05  W75-TITLE  OCCURS 10.
005470             07  W75-ETITLE  PIC X(07).
005480             07  W75-ATITLE  PIC X(07).

005490 01  W80-ACCOUNT.
005500	   03  W80-ACNO        PIC  X(06).
005510     03  W80-NUMD REDEFINES W80-ACNO.
005530	       05  W80-SERIAL  PIC  9(06).
005500	   03  W80-ACNO2       PIC  X(06).
005520	   03  W80-TYPE        PIC  9(01).

       COPY W90.DBT.

       COPY W95.STM.

004490 01  W100-EDIT.
004500     03  W100-LBAL       PIC Z(06)9.99-.
004500     03  W100-LARR       PIC Z(06)9.99-.
004500     03  W100-BAL        PIC Z(08)9.99-.
004560     03  W100-CRL        PIC Z(06)9.  

       01  W200-MEMO.
           03  W200-MEM-FLDS. 
               05  W200-MEM1   PIC  X(60).
               05  W200-MEM2   PIC  X(60).
               05  W200-MEM3   PIC  X(60).
               05  W200-MEM4   PIC  X(60).
               05  W200-MEM5   PIC  X(60).
               05  W200-MEM6   PIC  X(60).
               05  W200-MEM7   PIC  X(60).
               05  W200-MEM8   PIC  X(60).
               05  W200-MEM9   PIC  X(60).
               05  W200-MEM10  PIC  X(60).
               05  W200-MEM11  PIC  X(60).
               05  W200-MEM12  PIC  X(60).
           03  W200-MEM-FOC REDEFINES W200-MEM-FLDS.
               05  W200-MEM    PIC  X(60) OCCURS 12.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
005990 SCREEN SECTION.

       COPY BLANK.CRT.

009340 01  S05D.
	   03  LINE  4 COLUMN 13 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	     "зддддддддддддддддддддддддддддддддддддддддддддддддддддд©".
	   03  LINE  5 COLUMN 13 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	     "Ё       Select the Range of accounts to print         Ё".
	   03  LINE  6 COLUMN 13 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	     "цддддддддддддддддддддддддддддддддддддддддддддддддддддд╢".
	   03  LINE  7 COLUMN 13 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	     "Ё Start A/C:                                          Ё".
	   03  LINE  8 COLUMN 13 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	     "Ё                                                     Ё".
	   03  LINE  9 COLUMN 13 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	     "Ё End A/C:                                            Ё".
	   03  LINE 10 COLUMN 13 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	     "Ё                                                     Ё".
	   03  LINE 11 COLUMN 13 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	     "цддддддддддддддддддддддддддддддддддддддддддддддддддддд╢".
	   03  LINE 12 COLUMN 13 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	     "Ё    Statements will be printed in name sequence.     Ё".
	   03  LINE 13 COLUMN 13 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	     "юддддддддддддддддддддддддддддддддддддддддддддддддддддды".

006640 01  S15B.
006650	   03  LINE  2 COLUMN 31 FOREGROUND-COLOR 7 HIGHLIGHT
006660                           VALUE "PRINTING STATEMENTS".
006670     03  LINE  5 COLUMN 12 VALUE "Are you sure (Y/N)?".

       COPY DEBLUP.CRT.

       01  S-DEB-MEM.
           03  LINE  7 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE 
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E    
      -    "".
       01  S-MEM.
           03  LINE  6 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G9.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G10.
           03  LINE  7 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE 
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E    
      -    "".
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03  LINE  8 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G14.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-HME-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G13.
           03  LINE  9 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 10 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 11 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 12 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 13 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 14 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 15 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 16 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 17 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 18 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 19 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 20 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 21 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G11.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G12.
       01  SA-MEM.
           03  LINE  9 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM1 AUTO.
           03  LINE 10 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM2 AUTO.
           03  LINE 11 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM3 AUTO.
           03  LINE 12 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM4 AUTO.
           03  LINE 13 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM5 AUTO.
           03  LINE 14 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM6 AUTO.
           03  LINE 15 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM7 AUTO.
           03  LINE 16 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM8 AUTO.
           03  LINE 17 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM9 AUTO.
           03  LINE 18 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM10 AUTO.
           03  LINE 19 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM11 AUTO.
           03  LINE 20 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM12 AUTO.

       01  S80-YEAR.
	   03  LINE 08 COLUMN 40 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  Z9 USING W05-DYY AUTO.
       01  S80-MONTH.
	   03  LINE 09 COLUMN 40 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  Z9 USING W05-DMM AUTO.

       COPY ERROR.CRT.

       COPY OPT.CRT.

004870 COPY S99.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006900 AA00-MAIN	      SECTION.
006910 AA00-INIT.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
003240	   IF LS0-DBLEV < 2
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006920	     PERFORM ZA00-INIT.
	     PERFORM YA000.
	     PERFORM XB000.
	   IF PREVIOUS-STATEMENT
	       CLOSE DBHIST
		     DTHIST.
	   IF prn-open = "O"
	       perform close-down-printer.

       AA49.
006950	     EXIT PROGRAM.
      * здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      * Ё The following routines are used for printing via the WINDOWS Ё
      * Ё printing API's and are used for LASER and INK-JET printers   Ё
      * Ё that do not allow for embedded control characters.	       Ё
      * юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	PRINT-DETAILS  SECTION.
	     perform print-line.
      * здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      * Ё  Check if paper must be advanced? (WS-ADVANCE will be > 0)   Ё
      * юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   if WS-ADVANCE = 99
	       perform Throw-a-page
	   else
	   if WS-ADVANCE > 0
	       perform advance-printer WS-ADVANCE times.
      * ______________________________________________________________ *
      * PC_PRINTER_WRITE					       *
      * 							       *
      * Writes a text string to the printer.			       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_WRITE" using	 printer-handle        *
      * 					 print-buffer	       *
      * 			       by value  print-buff-len        *
      * 			       returning status-code	       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	print-buffer	pic x(n).			       *
      * 	print-buff-len	Numeric literal or pic x(4) comp-5.    *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 	printer-buffer	The buffer from which the bytes are to *
      * 			be printed			       *
      * 	printer-buff-len  The number of bytes to print	       *
      * 							       *
      * ON EXIT:						       *
      * 							       *
      * 	None						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You can check the success of the call by examining     *
      * 	RETURN-CODE.					       *
      * ______________________________________________________________ *
       print-line	  section.
             call "PC_PRINTER_WRITE" using    prn-handle
					      Print-Buffer
				     by value Print-Buffer-Len
             end-call.

      * ______________________________________________________________ *
      * PC_PRINTER_SET_FONT					       *
      * 							       *
      * Sets the font on the printer.				       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_SET_FONT" using     printer-handle    *
      * 					     font-family-name  *
      * 				   by value  font-size	       *
      * 				   by value  font-style        *
      * 			   returning status-code	       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	font-family-name	Group item defined as:	       *
      * 	    name-len	pic x(2) comp-5.		       *
      * 	    name-text	pic x().			       *
      * 	font-size	Numeric literal or pic x(4) comp-5.    *
      * 	font-style	Numeric literal or pic x(4) comp-5.    *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 	name-len	The length of the font family name.    *
      * 	name-text	The family name of the font to be used,*
      * 			such as Courier, Times and Symbol      *
      * 	font-size	Point size of the font		       *
      * 	font-style	Set of bits defining the required font *
      * 			style:				       *
      * 	  bit 3	 Bold					       *
      * 	  bit 2	 Strikeout				       *
      * 	  bit 1	 Underline				       *
      * 	  bit 0	 Italic 				       *
      * 							       *
      * 	All remaining bits are reserved for future use and     *
      * 	should not be set.				       *
      * 							       *
      * ON EXIT:						       *
      * 							       *
      * 	None						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You can check the success of the call by examining     *
      * 	RETURN-CODE.					       *
      * ______________________________________________________________ *
	set-printer-font   section.
	     call "PC_PRINTER_SET_FONT"
				     using prn-handle
					   font-family
				     by value font-size
				     by value font-style
             end-call.

      * ______________________________________________________________ *
      * PC_PRINTER_CONTROL					       *
      * 							       *
      * Sends a print command to a printer.			       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_CONTROL" using      printer-handle    *
      * 				  by value   print-command     *
      * 				  returning  status-code       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	print-command	Numeric literal or pic x(4) comp-5.    *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 	print-command	The command to send to the printer:    *
      * 							       *
      * 	  1   Abort printing the document and close the        *
      * 	      printer					       *
      * 	  2   Throw a page				       *
      * 	  3   Flush the print buffers			       *
      * 	  4   Advance one line				       *
      * 							       *
      * ON EXIT:						       *
      * 							       *
      *		None						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You can check the success of the call by examining     *
      * 	RETURN-CODE.					       *
      * ______________________________________________________________ *
      * здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      * Ё	     A D V A N C E   P A P E R	 1   L I N E	       Ё
      * юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	advance-printer    section.
	     move 4		 to prn-command.
             call "PC_PRINTER_CONTROL" using    prn-handle
                                       by value prn-command
	     end-call.
      * здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      * Ё	     A D V A N C E   P A P E R	 1   P A G E	       Ё
      * юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	Throw-a-page	   section.
	     move 2		 to prn-command.
	     call "PC_PRINTER_CONTROL" using	prn-handle
                                       by value prn-command
	     end-call.
      * ______________________________________________________________ *
      * PC_PRINTER_SET_COLOR					       *
      * 							       *
      * Sets the color on the printer.				       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_SET_COLOR" using     printer-handle   *
      * 				    by value  fore-or-back     *
      * 				    by value  color-red        *
      * 				    by value  color-green      *
      * 				    by value  color-blue       *
      * 				    returning status-code      *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	fore-or-back	pic x(2) comp-5.		       *
      * 	color-red	pic x(2) comp-5.		       *
      * 	color-green	pic x(2) comp-5.		       *
      * 	color-blue	pic x(2) comp-5.		       *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle The printer handle returned when the    *
      * 		       printer was opened.		       *
      * 	fore-or-back   Whether to set foreground or background:*
      * 	    1	       foreground			       *
      * 	    2	       background			       *
      * 	color-red      Value between 0 and 255; intensity of   *
      * 						red	       *
      * 	color-green    Value between 0 and 255; intensity of   *
      * 						green	       *
      * 	color-blue     Value between 0 and 255; intensity of   *
      * 						blue	       *
      * 							       *
      * ON EXIT:						       *
      * 							       *
      * 	None						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You must open the printer before using this routine.   *
      * 							       *
      * 	If your printer does not support color, it is up to    *
      * 	the printer device driver to interpret these values.   *
      * 	For example, some perform gray-shading; others choose  *
      * 	either black or white for each color.		       *
      * 							       *
      * 	You specify the color you want by combinations of      *
      * 	values showing the intensity of the three primary      *
      * 	colors. For example, to get the colors in the	       *
      * 	following table, you set the RGB values as shown:      *
      * 							       *
      * 	      Color	   Red	   Green   Blue 	       *
      * 	      -----	   ---	   -----   ---- 	       *
      * 	      Red	   255	   0	   0		       *
      * 	      Yellow	   255	   255	   0		       *
      * 	      Green	   0	   255	   0		       *
      * 	      Cyan	   0	   255	   255		       *
      * 	      Blue	   0	   0	   255		       *
      * 	      Magenta	   255	   0	   255		       *
      * 	      White	   255	   255	   255		       *
      * 	      Black	   0	   0	   0		       *
      * 							       *
      * 	    Some other, more unusual, colors are:	       *
      * 							       *
      * 	      Dark orange  255	   140	   0		       *
      * 	      Navy blue	   0	   0	   128		       *
      * 	      Violet	   238	   120	   238		       *
      * 	      Beige	   245	   245	   220		       *
      * 							       *
      * ______________________________________________________________ *
	printer-colour	  section.
	     call "PC_PRINTER_SET_COLOR" using	  prn-handle
					 by value fore-or-back
					 by value color-red
					 by value color-green
					 by value color-blue
	     end-call.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *	   Ё This routine prints the transaction details and uses the  Ё
      *    Ё background colour to show columns. This helps with read-  Ё
      *    Ё ability and gives the document a more professional look.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	PRINT-TRANSACTION-LINE SECTION.
	     MOVE 245		 TO color-red.
	     MOVE 245		 TO color-green.
	     MOVE 220		 TO color-blue.
	     perform printer-colour.
	     MOVE ST7-CLR-DT	 TO Print-Buffer.
	     MOVE 14		 to Print-Buffer-Len.
	     perform print-line.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  W H I T E   B A C K G R O U N D	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 255		 TO color-red.
	     MOVE 255		 TO color-green.
	     MOVE 255		 TO color-blue.
	     perform printer-colour.
	     MOVE ST7-CLR-DSC	 TO Print-Buffer.
	     MOVE 16		 to Print-Buffer-Len.
	     perform print-line.
	     MOVE 245		 TO color-red.
	     MOVE 245		 TO color-green.
	     MOVE 220		 TO color-blue.
	     perform printer-colour.
	     MOVE ST7-CLR-RF	 TO Print-Buffer.
	     MOVE 12		 to Print-Buffer-Len.
	     perform print-line.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  W H I T E   B A C K G R O U N D	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 255		 TO color-red.
	     MOVE 255		 TO color-green.
	     MOVE 255		 TO color-blue.
	     perform printer-colour.
	     MOVE ST7-CLR-DB	 TO Print-Buffer.
	     MOVE 16		 to Print-Buffer-Len.
	     perform print-line.
	     MOVE 245		 TO color-red.
	     MOVE 245		 TO color-green.
	     MOVE 220		 TO color-blue.
	     perform printer-colour.
	     MOVE ST7-CLR-CR	 TO Print-Buffer.
	     MOVE 20		 to Print-Buffer-Len.
	     perform print-line.
	     MOVE 245		 TO color-red.
	     MOVE 220		 TO color-green.
	     MOVE 245		 TO color-blue.
	     perform printer-colour.
	     MOVE ST7-CLR-BL	 TO Print-Buffer.
	     MOVE 17		 to Print-Buffer-Len.
	     perform print-line.
	     perform advance-printer.
      *
      *************  End PRINT-TRANSACTION-LINE  *************
      * ______________________________________________________________ *
      * PC_PRINTER_CLOSE					       *
      * 							       *
      * Closes a previously opened printer channel.		       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_CLOSE" using printer-handle	       *
      * 				returning status-code	       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 							       *
      * ON EXIT:						       *
      * 	None						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You can check the success of the call by examining     *
      * 	RETURN-CODE.					       *
      * ______________________________________________________________ *
	close-down-printer section.
	     call "PC_PRINTER_CLOSE" using by reference prn-handle
	     end-call.

      * ______________________________________________________________ *
      * PC_PRINTER_LOAD_BMP					       *
      * 							       *
      * Loads a bitmap into memory ready for the PC_PRINTER_WRITE_BMP  *
      * function.						       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_LOAD_BMP" using	printer-handle *
      * 				   by reference bmpfilename    *
      * 				   by reference bmp-id	       *
      * 				   returning	status-code    *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	bmpfilename	pic x(n).			       *
      * 	bmp-id		pic x(4) comp-5.		       *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 	bmpfilename	The name of the file to be printed     *
      * 			(null or space terminated).	       *
      * 							       *
      * ON EXIT:						       *
      * 	bmp-id		A unique identifier for the loaded     *
      * 			bitmap. 			       *
      * 	status-code	Printer status code:		       *
      * 							       *
      * 	    0		Successful			       *
      * 	    3		Printer device not open 	       *
      * 	   18		Failed to load bitmap		       *
      * 	   19		Invalid bitmap id given 	       *
      * 							       *
      * EXAMPLE:						       *
      * 							       *
      *     call "PC_PRINTER_LOAD_BMP" using	    printer-handle     *
      * 			       by reference "mflogo.bmp" & x"0"*
      * 			       by reference bmp-id	       *
      * 			       returning    printer-retcode    *
      *     end-call						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 	See also:					       *
      * 							       *
      * 	PC_PRINTER_FREE_BMP				       *
      * 	PC_PRINTER_WRITE_BMP				       *
      * ______________________________________________________________ *
       print-header-BMP    section.

	     call "PC_PRINTER_LOAD_BMP"	using prn-handle
					by reference BmpFilename
					by reference BmpID
	     end-call.

           if Return-Code not = 0
               move "PC_PRINTER_LOAD_BMP" to Operation
               perform 9999-Display-Error
	   end-if.
      * ______________________________________________________________ *
      * PC_PRINTER_WRITE_BMP					       *
      * 							       *
      * Write a loaded bitmap to a printer at a specified position with*
      * a given size.						       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_WRITE_BMP" using     printer-handle   *
      * 				    by value  bmp-id	       *
      * 				    by value  reserved	       *
      * 				    by value  bmp-row	       *
      * 				    by value  bmp-col	       *
      * 				    by value  bmp-width        *
      * 				    by value  bmp-height       *
      * 				    returning status-code      *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	reserved	pic x(4) comp-5 value 0.	       *
      * 	bmp-id		pic x(4) comp-5.		       *
      * 	bmp-row 	pic x(4) comp-5.		       *
      * 	bmp-col 	pic x(4) comp-5.		       *
      * 	bmp-width	pic x(4) comp-5.		       *
      * 	bmp-height	pic x(4) comp-5.		       *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 	bmp-id	The unique identifier of the bitmap to print   *
      * 	bmp-row 					       *
      * 	bmp-col The position of the bitmap to print.	       *
      * 	bmp-width					       *
      * 	bmp-height  The size of the bitmap to print.	       *
      * 							       *
      * ON EXIT:						       *
      * 	status-code  Printer status code:		       *
      * 	    0	     Successful 			       *
      * 	    3	     Printer device not open		       *
      * 	    4	     Out of memory while printing	       *
      * 	    5	     Disk full while spooling file	       *
      * 	    7	     Print job aborted, no file spooled to     *
      * 		     Print Manager			       *
      * 	   11	     Write failure			       *
      * 	   21	     Failed to print bitmap		       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	This routine works with PostScript and HP PCL printers.*
      * 	It is not supported on HP Deskjet or dot matrix        *
      * 	printers.					       *
      * 	See also:					       *
      * 							       *
      * 	PC_PRINTER_FREE_BMP				       *
      * 	PC_PRINTER_LOAD_BMP				       *
      * 							       *
      * ______________________________________________________________ *
	     call "PC_PRINTER_WRITE_BMP" using prn-handle
					 by value BmpID
					 by value WriteBmpReserved
					 by value BmpRow
					 by value BmpCol
					 by value BmpWidth
					 by value BmpHeigth
	     end-call.

           if Return-Code not = 0
               move "PC_PRINTER_WRITE_BMP" to Operation
               perform 9999-Display-Error
	   end-if.
      * ______________________________________________________________ *
      * PC_PRINTER_FREE_BMP					       *
      * 							       *
      * Frees a bitmap from memory, releasing its image and palette    *
      * back to the operating system.				       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_FREE_BMP" using     printer-handle    *
      * 				   by value  bmp-id	       *
      * 				   returning status-code       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	bmp-id	pic x(4) comp-5.			       *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      *				printer was opened.		       *
      * 	bmp-id	The unique id of the loaded bitmap, returned   *
      * 		when the bitmap was loaded.		       *
      * 							       *
      * ON EXIT:						       *
      * 	status-code Printer status code:		       *
      * 	    0	    Successful				       *
      * 	    3	    Printer device not open		       *
      * 	   20	    Failed to free bitmap		       *
      * 							       *
      * EXAMPLE:						       *
      * 							       *
      * 	call "PC_PRINTER_FREE_BMP" using printer-handle,       *
      * 				   by value  bmp-idlogo,       *
      * 				   returning printer-retcode   *
      * 	end-call					       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 	See also:					       *
      * 							       *
      * 	PC_PRINTER_LOAD_BMP				       *
      * 	PC_PRINTER_WRITE_BMP				       *
      * ______________________________________________________________ *
	     call "PC_PRINTER_FREE_BMP" using	prn-handle
					by value  BmpID

           if Return-Code not = 0
               move "PC_PRINTER_FREE_BMP" to Operation
               perform 9999-Display-Error
	   end-if.
      * ______________________________________________________________ *
      * PC_WIN_SET_PDEFAULT					       *
      * 							       *
      * Set the default printer for all PC_WIN printer routines.       *
      * 							       *
      * SYNTAX:     call "PC_WIN_SET_PDEFAULT" using	deflt-printer  *
      * 			   returning status-code	       *
      * 							       *
      * PARAMETERS: deflt-printer  pic x(n).			       *
      * 	    status-code	   See Key in the Preface	       *
      * 							       *
      * ON ENTRY:   deflt-printer A null-terminated string specifying  *
      * 			  the default printer. If no name is   *
      * 			  specified (that is, the first        *
      * 			  character of the string is null)     *
      * 			  then the default printer is reset.   *
      * 							       *
      * ON EXIT:    status-code Printer status code:		       *
      * 							       *
      * 		0	Successful			       *
      * 	       17	Failed to find default printer	       *
      * 							       *
      * EXAMPLE:    call "PC_WIN_SET_PDEFAULT" using		       *
      * 			    "Local PostScript Printer" & x"00" *
      * 			     returning printer-retcode	       *
      * 	    end-call					       *
      * ______________________________________________________________ *

       AA50-LOOKUP     SECTION.
       AA50.
	     PERFORM SAVE-SCREEN.
011080	     MOVE 4		 TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM DEBTOR-LOOKUP.
	     PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S 
      *
010990 AA300		   SECTION.
011000 AA301.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     " to do Debtor Lookup "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6 HIGHLIGHT " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
011090 AA305.
011100       EXIT.

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      * 	   S C R E E N ,   K E Y B O A R D   &	 M O U S E
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё			  SCREEN-SHADOW 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To routine is used to display a shadow down the right and Ё
      *    Ё along the bottom of a pop-up box. The parameters that are Ё
      *    Ё required:						       Ё
      *	   Ё	       SHADE-ROW   - Top line of the box + 1.	       Ё
      *	   Ё	       SHADE-COL   - Left line of box + 2.	       Ё
      *	   Ё	       SHADE-WIDTH - Width of the box - 2.	       Ё
      *	   Ё	       SHADE-LINES - Hight of box - 1.		       Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё			  CHECK-CORRECT 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё This routine displays a pop-up window with the message    Ё
      *    Ё	      "Correct ENTER - N if incorrect"		       Ё
      *    Ё							       Ё
      *    Ё The response is returned in WS-OPTION (in upper case).    Ё
      *    Ё							       Ё
      *    Ё To start with the pop-up window higher or lower than row  Ё
      *    Ё 13 (default); move another value to SLIN and PERFORM      Ё
      *    Ё CHECK-SETUP THRU CHECK-EXIT.			       Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё			  ERROR-MESSAGE 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To display an error message with " - Press ANY key" at    Ё
      *    Ё the end; use PERFORM ERROR-MESSAGE.		       Ё
      *    Ё							       Ё
      *    Ё To display just the error message; use PERFORM	       Ё
      *    Ё ERROR-LENGTH THRU ERROR-EXIT.			       Ё
      *    Ё							       Ё
      *    Ё To display the error message higher or lower (default is  Ё
      *    Ё line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   Ё
      *    Ё or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     Ё
      *    Ё number to be used to SLIN and then PERFORM ERROR-DISPLAY  Ё
      *    Ё THRU ERROR-EXIT.					       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY FUNCTION.CRT.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё			  OPT-MESSAGE			       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё This routine is used to allow the OPERATOR to respond to  Ё
      *    Ё a request for an option, so that the correct action can   Ё
      *    Ё be performed by the program. The routine will display the Ё
      *    Ё message in a pop-up window and allow the OPERATOR to      Ё
      *    Ё respond to the 'question'. 			       Ё
      *    Ё							       Ё
      *    Ё The option request must be placed in WS-ERR-MES and may   Ё
      *    Ё not exceed 48 characters. The message will be centred in  Ё
      *    Ё the window. An example of a message request follows:      Ё
      *    Ё							       Ё
      *    Ё   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      Ё
      *    Ё   PERFORM OPT-MESSAGE (Use this format if function or any Ё
      *    Ё	       other keys are to be tested on exit)	       Ё
      *    Ё			     OR 			       Ё
      *    Ё   PERFORM OPT-MESSAGE UNTIL WS-OPTION = "N" OR "Y".       Ё
      *    Ё   (Use TEST AFTER if WS-OPTION is preloaded with a        Ё
      *    Ё   default option)					       Ё
      *    Ё							       Ё
      *    Ё This would be displayed as:			       Ё
      *    Ё	здддддддддддддддддддддддддддддддддддддддддддддддд©     Ё
      *    Ё	Ё	   Print transactions (Y/N) [ ] 	 Ё╟╟   Ё
      *    Ё	юдддддддддддддддддддддддддддддддддддддддддддддддды╟╟   Ё
      *    Ё	  ╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟   Ё
      *    Ё							       Ё
      *    Ё The response is returned in WS-OPTION (in upper case).    Ё
      *    Ё							       Ё
      *    Ё To display the request message higher or lower (default   Ё
      *    Ё is line 13) move the line number to be used to SLIN and   Ё
      *    Ё then PERFORM OPT-SETUP THRU OPT-EXIT.		       Ё
      *    Ё (The UNTIL and TEST AFTER options also apply here)        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY OPTION.CRT.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

      /    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	       R E A D	 F I L E S   R O U T I N E S	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
007340 AC000-READ	       SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.
      
       COPY DBHIST.RD.

       COPY DBTCON.RD.
      
       COPY DBTRAN.RD.
      
000140 COPY DEBMEM.RD.

       COPY DEBTOR.RD1.
      
       COPY DTHIST.RD.

       COPY SHARED.RD.

      /    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    ЁR E W R I T E   &	W R I T E   F I L E S	R O U T I N E SЁ
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
009850 AD000-WRITE	       SECTION.

       COPY DBTCON.WR.
      
011410 COPY DBTRAN.WR.

011430 COPY DEBTOR.WR.

       COPY SHARED.WR.

       COPY DEBTOR.LUP.

       COPY APAC.HLP.

      /
008930 CA155-GET-DEBTOR        SECTION 2.
008940 CA155-ACCOUNT.
	   IF PREVIOUS-STATEMENT
	       MOVE W80-ACNO	 TO DBH-ACNO
	       PERFORM READ-DBHIST THRU READ-DBHIST-EXIT
	       MOVE DBH-RECORD1  TO DEB-RECORD1
	   ELSE
012010	       MOVE W80-ACNO	 TO DEB-ACNO
014330	       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
	   IF WS-F-ERROR = 6 OR 56
               MOVE 1            TO WS-ERROR
014380     ELSE
014390     IF DEB-ACNO NOT = W80-ACNO
014400         MOVE 2            TO WS-ERROR
014410     ELSE
014420         MOVE 0            TO WS-ERROR
               MOVE DEB-TYPE     TO W80-TYPE.
009100 CA160-EXIT.
009110       EXIT.

009120 CA165-NAME              SECTION 2.
009130 CA165-EXTRACT.
009140	     MOVE ZERO		 TO WS-ERROR.
009150       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
009160         INTO W40-SURNAME W40-FNAME W40-SNAME
009170         OVERFLOW
009180		 MOVE 1 	 TO WS-ERROR.
009190 CA170-EXIT.
009200       EXIT.

009210 CA175-ADDRESS           SECTION 2.
009220 CA175-EXTRACT.
009230	     MOVE 0		 TO WS-ERROR.
009240       UNSTRING W60-ADDRESS DELIMITED BY "*"
009250         INTO W40-ADD1 W40-ADD2 W40-ADD3 W40-ADD4
009260         OVERFLOW
009270		 MOVE 1 	 TO WS-ERROR.
009280 CA180-EXIT.
009290       EXIT.
      /    *************************************************************
      *    ****    E X T R A C T   C O N S O L O D A T E D
      * 	   A C C O U N T   T R A N S A C T I O N S
      *    *************************************************************
019020 XA000		      SECTION 6.
       XA00.
	     PERFORM SAVE-SCREEN-3.
      *    *************************************************************
      * 	   D I S P L A Y   W A R N I N G   M E S S A G E
      *    *************************************************************
      *   зддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *   Ё This routine will generate consolodating transactions Ё╟╟
      *   Ё and as there is no recovery procedure, backups of the Ё╟╟
      *   Ё data files should be made before proceeding.	  Ё╟╟
      *   Ё		Have backups been made Y/N		  Ё╟╟
      *   юддддддддддддддддддддддддддддддддддддддддддддддддддддддды╟╟
      *     ╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟
	     MOVE 10		 TO SHADE-ROW.
	     MOVE 14		 TO SHADE-COL.
	     MOVE 55		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
	     DISPLAY "зддддддддддддддддддддддддддддддддддддддддддддддддд
      -		     "дддддд" AT 0912
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     "©" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "Ё " AT 1012
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     "This routine will generate consolodating transacti
      -		     "ons " WITH FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
		     "Ё" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "Ё " AT 1112
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     "and as there is no recovery procedure, backups of
      -		     "the " WITH FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
		     "Ё" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "Ё " AT 1212
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     "data files should be made before proceeding.
      -		     "    " WITH FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
		     "Ё" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "Ё            " AT 1312
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     "Have backups been made "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 4
		     "/" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 4
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT
			      BACKGROUND-COLOR 4
		     "                 Ё"
		      WITH FOREGROUND-COLOR 4 HIGHLIGHT
			   BACKGROUND-COLOR 4.
	     DISPLAY "ю" AT 1412
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     "дддддддддддддддддддддддддддддддддддддддддддддддддд
      -		     "ддддды" WITH FOREGROUND-COLOR 4 HIGHLIGHT
				   BACKGROUND-COLOR 4.
	     PERFORM SCREEN-SHADOW.

       XA02.
	     PERFORM TEST AFTER
		     UNTIL WS-OPTION = "Y" OR "N"
	       ACCEPT WS-OPTION AT 1352
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4 UPDATE AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	     END-PERFORM.
	     PERFORM RESTORE-SCREEN-3.
	   IF WS-OPTION = "N"
014970	       MOVE 9		 TO WS-STM
014990	       GO TO XA999-EXIT.
	   IF W05-CON-TYPE = "A"
	       OPEN OUTPUT DBTCON
	       CLOSE DBTCON
	       OPEN I-O DBTCON.
      *здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *Ё	      EXTRACTING CONSOLODATED STATEMENTS	      Ё╟╟
      *цдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢╟╟
      *Ё Debtor Ac. : AAAAAA NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN Ё╟╟
      *Ё Transaction: AAAAAA CCCCMMYY TT RRRRRRRR		      Ё╟╟
      *юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды╟╟
      *  ╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟
	     MOVE 10		 TO SHADE-ROW.
	     MOVE 10		 TO SHADE-COL.
	     MOVE 62		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
	     DISPLAY "зддддддддддддддддддддддддддддддддддддддддддддддддд
      -		     "ддддддддддддд" AT 0908
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "©" WITH FOREGROUND-COLOR 5 HIGHLIGHT
			      BACKGROUND-COLOR 5.
	     DISPLAY "Ё              " AT 1008
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "EXTRACTING CONSOLODATED STATEMENTS              "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "Ё" WITH FOREGROUND-COLOR 5 HIGHLIGHT
			      BACKGROUND-COLOR 5.
	     DISPLAY "ц" AT 1108
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "дддддддддддддддддддддддддддддддддддддддддддддддддд
      -		     "дддддддддддд"
		      WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 5
		     "╢" WITH FOREGROUND-COLOR 5 HIGHLIGHT
			      BACKGROUND-COLOR 5.
	     DISPLAY "Ё " AT 1208
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "Debtor Ac. : "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "                                                Ё"
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     DISPLAY "Ё " AT 1308
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "Transaction: "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "                                                Ё"
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     DISPLAY "ю" AT 1408
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "дддддддддддддддддддддддддддддддддддддддддддддддддд
      -		     "дддддддддддды"
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     PERFORM SCREEN-SHADOW.

       XA05.
	     PERFORM PRINT-ABORT.
           IF WS-OPTION = "Y"
014970	       MOVE 9		 TO WS-STM
014990	       GO TO XA999-EXIT.
021090	     PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
010780	   IF WS-F-ERROR = 6
	       PERFORM RESTORE-SCREEN-3
	       IF W05-CON-TYPE = "A"
		   CLOSE DBTCON
	       END-IF
	       GO TO XA999-EXIT.
	   IF DEB-CONAC = SPACES
	       GO TO XA05.
	     DISPLAY DEB-ACNO AT 1223
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE DEB-NAME	 TO W40-NAME.
000860	   IF DEB-TYPE = 1
000870         INSPECT W40-NAME REPLACING ALL "*" BY " "
000880         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
	     DISPLAY W40-NAME AT 1230
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF DEB-CONAC = DEB-ACNO
	       GO TO XA20.
      *    *************************************************************
      *    ****  C H E C K   T H A T   C O N S O L O D A T E D
      * 	 A C C O U N T	 N U M B E R   I S   V A L I D
      *    *************************************************************
	     MOVE DEB-ACNO	 TO W80-ACNO2.
	     MOVE DEB-CONAC	 TO W80-ACNO.
024240	     PERFORM CA155-GET-DEBTOR.
024250	   IF NOT(WS-ERROR = ZERO)
	       PERFORM XA10 THRU XA15
	       GO TO XA05.

       XA10.
	     MOVE W80-ACNO2	 TO W80-ACNO.
024240	     PERFORM CA155-GET-DEBTOR.
	     PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.

       XA15.
021090	     PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
	   IF NOT(DEB-ACNO = W80-ACNO2)
	       GO TO XA15.

       XA20.
	   IF NOT(DEB-ACNO = DEB-CONAC)
	       IF NOT(DEB-BAL = ZERO)
		   PERFORM XA30.
	   IF W05-CON-TYPE = "S"
	       GO TO XA05.
022970	     MOVE DEB-ACNO	 TO TRA-KEY.
	     PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
	   IF WS-F-ERROR = 5
	       GO TO XA05.

       XA25.
023010	     PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
	   IF (WS-F-ERROR = 5) OR
              (TRA-AC NOT = DEB-ACNO)
	       GO TO XA05.
	     DISPLAY TRA-AC AT 1323
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT
			  BACKGROUND-COLOR 5
		     TRA-DATE AT 1330
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5
		     TRA-CODE AT 1339
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5
		     TRA-REF AT 1342
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE TRA-RECORD1	 TO CON-TRAN.
	     MOVE DEB-CONAC	 TO DBT-CON.
	   IF DEB-CONAC = TRA-AC
	       MOVE "00"	 TO DBT-IND
	   ELSE
	       MOVE "01"	 TO DBT-IND.
	     PERFORM WRITE-DBTCON THRU WRITE-DBTCON-EXIT.
	     MOVE "Y"		 TO W05-PRINT-CS.
	     GO TO XA25.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Generate transfer transactions to CONSOLODATED account.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       XA30.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
015010	     INITIALIZE TRA-RECORD1.
      *    *************************************************************
      *    **** T R F	T O   C O N S O L O D A T E D	A C C O U N T
      *    *************************************************************
015130	     MOVE DEB-CONAC	 TO TRA-AC.
015090	     MOVE W12-T-YMD	 TO TRA-DATE.
	   IF DEB-BAL > ZERO
015110	       MOVE 81		 TO TRA-CODE
	   ELSE
	       MOVE 82		 TO TRA-CODE.
015120	     MOVE DEB-ACNO	 TO TRA-REF TRA-REG TRA-ORD.
	     MOVE DEB-BAL	 TO TRA-VALUE
				    TRA-TAXFREE.
015230	     MOVE ZERO		 TO TRA-TAX
015240				    TRA-DISC
				    TRA-PAID
				    TRA-AGE.
	     MOVE SPACES	 TO TRA-REC.
	     MOVE DEB-ACNO	 TO W80-ACNO2.
	     MOVE DEB-CONAC	 TO W80-ACNO.
024240	     PERFORM CA155-GET-DEBTOR.
	     ADD TRA-VALUE	 TO DEB-BAL.
	     PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
015290	     PERFORM WRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
      *    *************************************************************
      *    ****  T R F	 F R O M   S U B O R D O N A T E   A C C O U N T
      *    *************************************************************
	     PERFORM XA10 THRU XA15.
015130	     MOVE DEB-ACNO	 TO TRA-AC.
	   IF TRA-CODE = 82
	       MOVE 81		 TO TRA-CODE
	   ELSE
015110	       MOVE 82		 TO TRA-CODE.
015120	     MOVE DEB-CONAC	 TO TRA-REF TRA-REG TRA-ORD.
	     MULTIPLY -1 BY	 TRA-VALUE.
	     MULTIPLY -1 BY	 TRA-TAXFREE.
	     ADD TRA-VALUE	 TO DEB-BAL.
	     PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
015290	     PERFORM WRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
000140	     UNLOCK SHARED.

       XA999-EXIT.
	     EXIT.
      /    *************************************************************
      *    ****    P R I N T   S T A T E M E N T S
      *    *************************************************************
019020 XB000		      SECTION 50.
019030 XB00-INIT.
019040       DISPLAY CLR-SCREEN.
019050       DISPLAY S15B.
	     PERFORM TEST AFTER
		     UNTIL WS-OPTION = "Y" OR "N"
019060	       ACCEPT WS-OPTION AT 0532
019070		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	     END-PERFORM.
019130	   IF WS-OPTION = "N"
019140	       GO TO XB299-EXIT.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
      * ______________________________________________________________ *
      * PC_PRINTER_OPEN 					       *
      * 							       *
      * Opens a printer channel, giving it a document title, and       *
      * optionally displaying the dialog boxes for printer control,    *
      * font selection, and progress indication.		       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_OPEN" using	 printer-handle        *
      * 					 document-title        *
      * 			       by value  flags		       *
      * 			       by value  window-handle	       *
      * 			       returning status-code	       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	document-title	Group item defined as:		       *
      * 	    title-len	pic x(2) comp-5.		       *
      * 	    title-text	pic x.				       *
      * 	    flags	Numeric literal or pic x(4) comp-5.    *
      * 	    window-handle Numeric literal or pic x(4) comp-5.  *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	title-len	The length of the document title.      *
      * 	title-text	The title of the document to be        *
      * 			printed.			       *
      * 	flags	A set of bits to define printer options:       *
      * 	 bit 0	Display a dialog for defining printer	       *
      * 		characteristics (cannot be used with bit 2 or  *
      * 		bit 3)					       *
      * 	 bit 1	Display a font dialog box, so you can select   *
      * 		the font for the document		       *
      * 	 bit 2	Print in portrait orientation (cannot be used  *
      * 		with bit 0 or bit 3)			       *
      * 	 bit 3	Print in landscape orientation (cannot be used *
      * 		with bit 0 or bit 2)			       *
      * 	 bit 4	Display a progress indicator dialog box        *
      * 							       *
      * 	All remaining bits are reserved for future use and     *
      * 	should not be set.				       *
      * 							       *
      * 	window-handle  Handle of the parent window. The parent *
      * 		       window is used as a reference for       *
      * 		       positioning dialog boxes on the screen. *
      * 		       If window-handle is 0 positioning is    *
      * 		       system dependent.		       *
      * 							       *
      * ON EXIT:						       *
      * 	printer-handle	Returns a handle to be used for        *
      * 			subsequent printer operations.	       *
      * 	status-code	Printer status code:		       *
      * 							       *
      * 	    0		Successful			       *
      * 	    1		Could not open printer device	       *
      * 	    5		Failed to open file		       *
      * 	    9		No default printer found. Select a     *
      * 			printer from Windows Control Panel.    *
      * 	   24		User pressed Cancel on the printer     *
      * 			setup dialog or the font selection     *
      * 			dialog. 			       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You can check the success of the call by examining     *
      * 	RETURN-CODE.					       *
      * ______________________________________________________________ *
	     move 17		 to title-len.
	     move "STMF07W - Statements" & x"00"
                                 to title-text.
	     call "PC_PRINTER_OPEN" using by reference prn-handle
                                          by reference document-title
                                          by value prn-flags
                                          by value 0
	     end-call.
022160	     MOVE SPACES	 TO ST7-DET.
	     MOVE "O"		 TO prn-open.

019560 XB10-COMP.
	     MOVE 0401		 TO CPOS.
	     PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
019590       MOVE "Y"            TO WS-EXPAND.
019730	     DISPLAY "Statement message" AT 0402.
019740	     DISPLAY "English" AT 0603.
019860	     DISPLAY "Afrikaans" AT 1003.
019980	     MOVE SPACES	 TO W30-MESS-A(1) W30-MESS-A(2).

019990 XB11.
020000	     ACCEPT W30-SMES(2, 1) AT 0614
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
020010			 BACKGROUND-COLOR 5 UPDATE AUTO.
020020	     ACCEPT W30-SMES(2, 2) AT 0714
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
020010			 BACKGROUND-COLOR 5 UPDATE AUTO.
020020	     ACCEPT W30-SMES(2, 3) AT 0814
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
020010			 BACKGROUND-COLOR 5 UPDATE AUTO.
020020	     ACCEPT W30-SMES(2, 4) AT 0914
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
020010			 BACKGROUND-COLOR 5 UPDATE AUTO.
020180	     ACCEPT W30-SMES(1, 1) AT 1014
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
020010			 BACKGROUND-COLOR 5 UPDATE AUTO.
020200	     ACCEPT W30-SMES(1, 2) AT 1114
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
020010			 BACKGROUND-COLOR 5 UPDATE AUTO.
020020	     ACCEPT W30-SMES(1, 3) AT 1214
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
020010			 BACKGROUND-COLOR 5 UPDATE AUTO.
020020	     ACCEPT W30-SMES(1, 4) AT 1314
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
020010			 BACKGROUND-COLOR 5 UPDATE AUTO.
020240	     DISPLAY WS-CHK AT 1602 WITH FOREGROUND-COLOR 6 HIGHLIGHT.
020250	     ACCEPT WS-OPTION AT 1639
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
020320	   IF WS-OPTION = "N"
020330	       GO TO XB11.
020340	     DISPLAY "Print keyed message on accounts < (30), " AT 1802
020350		     "(60), (90) days >  <".
020360	     DISPLAY "XX = print on all statements" AT 2302
020370		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE 2		 TO WS-LENGTH.

020400 XB11A.
	     PERFORM TEST AFTER
		     UNTIL WS-MESS = "30" OR "60" OR "90" OR "XX"
020410	       ACCEPT WS-MESS AT 1859
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 AUTO
	       CALL "CBL_TOUPPER" USING WS-MESS
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	     END-PERFORM.
	     MOVE 1		 TO WS-LENGTH.
	     MOVE "Print 'I'nvoice or 'O'ther reference  [I]"
				 TO WS-ERR-MES.
	     MOVE "I"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "I" OR "O".
	     MOVE WS-OPTION	 TO WS-REF.

020440 XB12.
020450       MOVE 1              TO WS-SUB-3.
020460       MOVE 1              TO WS-PARKEY.
020470       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
020480       MOVE PAR-COMPANY    TO W95-COMP.
020490       MOVE PAR-TELEPHONE  TO W95-TEL.
020510	     ADD 1		 TO WS-PARKEY.
020520       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
020530       MOVE PAR-ADDRESS1   TO W95-ADD1.
020540       MOVE PAR-ADDRESS2   TO W95-ADD2.
020570       MOVE PAR-ADDRESS3   TO W95-ADD3.
020580       MOVE PAR-ADDRESS4   TO W95-ADD4.
020500       MOVE PAR-POST-CDE   TO W95-POST.
             MOVE 0301           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
020610	     DISPLAY "Print accounts that became zero this month ("
		      AT 0612
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT ")".
	     PERFORM TEST AFTER
		     UNTIL WS-OPTION = "Y" OR "N"
020630	       ACCEPT WS-OPTION AT 0661
020640		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	     END-PERFORM.
020700	   IF WS-OPTION = "N"
020710         MOVE 1            TO WS-S8
020720     ELSE
020730         MOVE ZERO         TO WS-S8.
020740	     DISPLAY "Selective print (" AT 0812
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT ")".
	     PERFORM TEST AFTER
		     UNTIL WS-OPTION = "Y" OR "N"
020760	       ACCEPT WS-OPTION AT 0834
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	     END-PERFORM.
020850	   IF WS-OPTION = "Y"
020860         MOVE 1            TO WS-NTP
020870	       GO TO XB125
020880     ELSE
020890         MOVE ZERO         TO WS-NTP.
020900	     DISPLAY "Is this a restart (" AT 1012
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT ")".
	     PERFORM TEST AFTER
		     UNTIL WS-OPTION = "Y" OR "N"
020920	       ACCEPT WS-OPTION AT 1036
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5 AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	     END-PERFORM.
021010	   IF WS-OPTION = "Y"
021020	       GO TO XB130.

       XB14.
	     MOVE "'A'll statements or 'R'ange of statements [A]"
				 TO WS-ERR-MES.
	     MOVE "A"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "A" OR "R".
	     MOVE WS-OPTION	 TO WS-RANGE.
	     DISPLAY "Debtor account:" AT 1412.
021030       MOVE 1              TO WS-SUB-2.

       XB14-RANGE-CHECK.
	   IF WS-RANGE = "R"
	       MOVE SPACES	 TO WS-ASTART WS-AEND
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	Save screen details and display the get range panel.   Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       PERFORM XB300
	       MOVE 0726	 TO COL-AND-LINE
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	  First account in the range that must be printed.     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       PERFORM XB500
	       IF WS-ERROR = 9
		   GO TO XB299-EXIT
	       END-IF
	       MOVE W80-ACNO	 TO WS-ASTART
	       MOVE 0926	 TO SPOS
	       MOVE SPOS	 TO COL-AND-LINE
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	  Last account in the range that must be printed.      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       PERFORM XB500
	       IF WS-ERROR = 9
		   GO TO XB299-EXIT
	       END-IF
	       MOVE W80-ACNO	 TO WS-AEND
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	 Start reading from the first account in the range.    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       PERFORM XB400
	       GO TO XB20-LOOP
	   END-IF.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	 Start reading from the first account in the file.     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
021040 XB15-INDEX.
	   IF PREVIOUS-STATEMENT
	       MOVE " !"	 TO DBH-NAME
	       PERFORM START-AT-DEBH-NAME THRU READ-DBHIST-EXIT
	   ELSE
	       MOVE " !"	 TO DEB-NAME
	       PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
010780	   IF WS-F-ERROR = 6 OR 56
021070	       GO TO XB299-EXIT.

021080 XB20-LOOP.
             PERFORM PRINT-ABORT.
           IF WS-OPTION = "Y"
014970         MOVE "**** PRINT ABORTED BY OPERATOR ****" 
				 TO ST7-DET
	       move st7-det	 to Print-Buffer
	       PERFORM PRINT-DETAILS
017710	       MOVE SPACES	 TO ST7-DET
014980	       PERFORM Throw-a-page
014990	       GO TO XB299-EXIT.
	   IF WS-RANGE = "R"
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	Check if last account in the range has been printed.   Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       IF WS-END = "Y"
		   GO TO XB200-END.
	   IF PREVIOUS-STATEMENT
021090	       PERFORM READ-DBHIST-NEXT THRU READ-DBHIST-EXIT
	       MOVE DBH-RECORD1  TO  DEB-RECORD1
	   ELSE
021090	       PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
010780	   IF WS-F-ERROR = 6 OR 56
	       GO TO XB200-END.
	   IF WS-RANGE = "R"
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Check if this is the last account in range to be printed. Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       IF DEB-ACNO = WS-AEND
		   MOVE "Y"	 TO WS-END.
	     DISPLAY DEB-ACNO AT 1428
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.

021100 XB21.
021110     IF DEB-BAL = ZERO
021120         IF WS-S8 = 1
021130		   GO TO XB100-CHECK
021140         ELSE
021150         IF (DEB-DEBT = ZERO) AND (DEB-CRED = ZERO)
021160		   GO TO XB100-CHECK.
021170	     PERFORM XB110 THRU XB115.
021180       MOVE SPACES         TO W40-NAME1.
021190     IF DEB-BAL NOT > ZERO
021200         MOVE ZERO         TO W90-CUR
021210     ELSE
021220         COMPUTE W90-CUR = DEB-BAL - (DEB-CUR + DEB-30 + DEB-60
021230                            + DEB-90 + DEB-120 + DEB-INT).
021240     IF DEB-TYPE NOT = 1
021250         MOVE DEB-NAME     TO W40-NAME
021260	       GO TO XB45-ADD.
021270       MOVE DEB-NAME       TO W60-NAME.
021280       PERFORM CA165-NAME.
021290     IF DEB-LANG = "A"
021300         MOVE W75-ATITLE (DEB-TITLE) 
                                 TO W60-NAME
021310     ELSE
021320         MOVE W75-ETITLE (DEB-TITLE) 
                                 TO W60-NAME.
021330       MOVE 7              TO WS-SUB-6.

021340 XB25-TITLE.
021350	   IF W60-NCHAR(WS-SUB-6) NOT = SPACE
021360	       GO TO XB30-INIT.
021370       SUBTRACT 1          FROM WS-SUB-6.
021380     IF WS-SUB-6 > 0
021390	       GO TO XB25-TITLE.

021400 XB30-INIT.
021410       MOVE 1              TO WS-SUB-5.
021420       ADD 2               TO WS-SUB-6.
021430	     MOVE W40-NCHAR(25)  TO W60-NCHAR(WS-SUB-6).
021440	     ADD 1		 TO WS-SUB-6.
021450	     MOVE "."		 TO W60-NCHAR(WS-SUB-6).
021460	   IF W40-NCHAR(49) = SPACE
021470	       GO TO XB35-SURNAME.
021480       ADD 1               TO WS-SUB-6.
021490	     MOVE W40-NCHAR(49)  TO W60-NCHAR(WS-SUB-6).
021500       ADD 1               TO WS-SUB-6.
021510	     MOVE "."		 TO W60-NCHAR(WS-SUB-6).

021520 XB35-SURNAME.
021530       ADD 2               TO WS-SUB-6.

021540 XB40-LOOP.
021550	     MOVE W40-NCHAR(WS-SUB-5)
				 TO W60-NCHAR(WS-SUB-6).
021560     IF WS-SUB-5 < 24
021570         ADD 1             TO WS-SUB-5
021580         ADD 1             TO WS-SUB-6
021590	       GO TO XB40-LOOP.
021600       MOVE W60-NAME       TO W40-NAME.

021610 XB45-ADD.
021620       MOVE DEB-ADDRESS    TO W60-ADDRESS.
021630       PERFORM CA175-ADDRESS.
021640       MOVE 0              TO WS-PAGE.
	     MOVE 1		 TO WS-ADVANCE.

021650 XB50-STM.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		     Setup the printer font		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     move FontTypeFace	 to Font-family-name.
	     move 12		 to Font-family-namelen.
	     move font-12-CPI	 to font-size.
	     move 0		 to font-style.
	     perform set-printer-font.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		     Print header Bitmap		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     perform print-header-BMP.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Advance printer location to past the Bitmap to be printed Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     perform advance-printer 14 times.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  Set to print Я 5cpi and Bold		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     move font-5-CPI	 to font-size.
	     move 8		 to font-style.
	     perform set-printer-font.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   This heading is printed at 5cpi and centered        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE "     S T A T E M E N T / S T A A T"
				 TO Print-Buffer.
	     MOVE 44		 TO Print-Buffer-Len.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM PRINT-DETAILS.
	     MOVE 96		 TO Print-Buffer-Len.
022750	     ADD 1		 TO WS-PAGE.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		      Set to print Я 12cpi		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     move font-12-CPI	 to font-size.
	     move 0		 to font-style.
	     perform set-printer-font.

022140 XB60-DEBT.
022160	     MOVE SPACES	 TO ST7-DET.
022170	     MOVE W40-NAME	 TO ST7-NAME.
023390	     MOVE W30-STL(1)	 TO ST7-MESS.
022180       MOVE 0              TO WS-ERROR.
	     move st7-det	 to Print-Buffer.
	     PERFORM PRINT-DETAILS.
022210	     MOVE SPACES	 TO ST7-DET.
022220	     MOVE W40-ADD1	 TO ST7-NAME.
023440	     MOVE W30-STL(2)	 TO ST7-MESS.
	     move st7-det	 to Print-Buffer.
	     PERFORM PRINT-DETAILS.
022240	     MOVE SPACES	 TO ST7-DET.
022260	     MOVE W40-ADD2	 TO ST7-NAME.
022430     IF W40-ADD2 = SPACES
022440         MOVE 1            TO WS-ERROR
022450	       IF NOT(DEB-POST = "0000" OR "9999")
022460		   MOVE DEB-POST TO ST7-PC2.
023440	     MOVE W30-STL(3)	 TO ST7-MESS.
	     move st7-det	 to Print-Buffer.
	     PERFORM PRINT-DETAILS.
022480	     MOVE SPACES	 TO ST7-DET.
022490	     MOVE W40-ADD3	 TO ST7-NAME.
022500     IF WS-ERROR = 0
022520         IF W40-ADD3 = SPACES
022530             MOVE 1        TO WS-ERROR
022540		   IF NOT(DEB-POST = "0000" OR "9999")
022550                 MOVE DEB-POST 
				 TO ST7-PC2.
023440	     MOVE W30-STL(4)	 TO ST7-MESS.
	     move st7-det	 to Print-Buffer.
	     PERFORM PRINT-DETAILS.
022580	     MOVE SPACES	 TO ST7-DET.
022590	     MOVE W40-ADD4	 TO ST7-NAME.
022610     IF WS-ERROR = 0
022630         IF W40-ADD4 = SPACES
022640             MOVE 1        TO WS-ERROR
022650		   IF NOT(DEB-POST = "0000" OR "9999")
022660                 MOVE DEB-POST 
				 TO ST7-PC2.
	     move st7-det	 to Print-Buffer.
	     PERFORM PRINT-DETAILS.
022690	     MOVE SPACES	 TO ST7-DET.
022700     IF WS-ERROR = 0
022710	       IF NOT(DEB-POST = "0000" OR "9999")
022720		   MOVE DEB-POST TO ST7-PC2.
	     move st7-det	 to Print-Buffer.
	     move 2		 to WS-ADVANCE.
	     PERFORM PRINT-DETAILS.
022690	     MOVE SPACES	 TO ST7-DET.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё The use of letter heads is not yet available in this ver- Ё
      *    Ё sion and will be included later.			       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
021680*	   IF NOT(WS-HEAD = "N")
	       MOVE "ACCOUNT NO:"
				 TO ST7-AH1
	       MOVE "DATE:"	 TO ST7-AH2
	       MOVE "PAGE NO:"	 TO ST7-AH3.
022600	     MOVE DEB-ACNO	 TO ST7-AC1.
022190	     MOVE W12-TODAY	 TO ST7-DATE.
022760	     MOVE WS-PAGE	 TO ST7-PAGE.
	     move st7-det	 to Print-Buffer.
	     PERFORM PRINT-DETAILS.
022740	     MOVE SPACES	 TO ST7-DET.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё The use of letter heads is not yet available in this ver- Ё
      *    Ё sion and will be included later.			       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
021680*	   IF NOT(WS-HEAD = "N")
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		      Switch on bold printing		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     move 8		 to font-style.
	     perform set-printer-font.
	     MOVE "    Date       Description     Reference       Debit
      -		  "            Credit           Balance" TO ST7-HD1
	     move st7-det	 to Print-Buffer.
	     move 1		 to WS-ADVANCE.
	     PERFORM PRINT-DETAILS.
022740	     MOVE SPACES	 TO ST7-DET.

022810 XB75-BFWD.
022820       MOVE 0              TO W25-WHOLE W90-BAL.
	   IF OPEN-ITEM
	       GO TO XB76.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Check if there is a brought forward balance to print.    Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
022830	   IF DEB-STM = 0
022840	       GO TO XB80-TRAN.
022850     IF DEB-LANG = "A"
022860         MOVE "REK GELEWER" 
				 TO ST7-BFWD
022870     ELSE
022880         MOVE "A/C RENDERED" 
				 TO ST7-BFWD.
022900	     MOVE DEB-STM	 TO ST7-TBAL W90-BAL.
	     move st7-det	 to Print-Buffer.
	     PERFORM PRINT-TRANSACTION-LINE.
022940	     MOVE SPACES	 TO ST7-DET.
022950       ADD 1               TO W25-WHOLE.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё For accounts that are Brought Forward skip the recalcula- Ё
      *    Ё tion of age analysis.				       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     GO TO XB80-TRAN.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Calculate the age analysis for this account (OPEN ITEM)  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       XB76.
	     MOVE ZERO		 TO W35-CUR
				    W35-30
				    W35-60
				    W35-90
				    W35-120
				    W35-150
				    W35-INT.
	   IF PREVIOUS-STATEMENT
	       MOVE DEB-ACNO	 TO TRH-AC
	       MOVE ZERO	 TO TRH-DATE TRH-CODE
	       MOVE SPACES	 TO TRH-REF
	   ELSE
	       MOVE DEB-ACNO	 TO TRA-AC
	       MOVE ZERO	 TO TRA-DATE TRA-CODE
	       MOVE SPACES	 TO TRA-REF.
	   IF PREVIOUS-STATEMENT
	       PERFORM START-AT-DTRH-KEY THRU READ-DTHIST-EXIT
	   ELSE
	       PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
	   IF WS-F-ERROR = 5 OR 55
	       GO TO XB80-TRAN.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Get the next transaction for calculation of the AGE ANA- Ё
      *    Ё  LYSIS for this account.				       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       XB78.
	   IF PREVIOUS-STATEMENT
	       PERFORM READ-DTHIST-NEXT THRU READ-DTHIST-EXIT
	       MOVE TRH-RECORD1  TO TRA-RECORD1
	   ELSE
	       PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
	   IF (WS-F-ERROR = 5 OR 55) OR
              (DEB-ACNO NOT = TRA-AC)
	       GO TO XB80-TRAN.
	   IF TRA-VALUE < ZERO
               IF (TRA-VALUE + TRA-ALLOCATED) = ZERO
		   GO TO XB78
               ELSE
                   ADD TRA-ALLOCATED 
                                 TO TRA-VALUE
                   MOVE ZERO     TO TRA-ALLOCATED
           ELSE
               IF TRA-VALUE = TRA-PAID
		   GO TO XB78
               ELSE
                   SUBTRACT TRA-PAID
                                 FROM TRA-VALUE
                   MOVE ZERO     TO TRA-PAID.
	   IF TRA-VALUE > ZERO
	       IF NOT(TRA-DATE > W30-120)
		   MOVE 5	 TO TRA-AGE
	       ELSE
	       IF NOT(TRA-DATE > W30-90)
		   MOVE 4	 TO TRA-AGE
	       ELSE
	       IF NOT(TRA-DATE > W30-60)
		   MOVE 3	 TO TRA-AGE
	       ELSE
	       IF NOT(TRA-DATE > W30-30)
		   MOVE 2	 TO TRA-AGE
	       ELSE
	       IF NOT(TRA-DATE > W30-CUR)
		   MOVE 1	 TO TRA-AGE
	       ELSE
		   MOVE ZERO	 TO TRA-AGE.
	   IF TRA-CODE = 17 OR 20
	       ADD TRA-VALUE	 TO W35-INT
           ELSE
           IF TRA-AGE = 5
	       ADD TRA-VALUE	 TO W35-150
           ELSE
           IF TRA-AGE = 4
	       ADD TRA-VALUE	 TO W35-120
           ELSE
           IF TRA-AGE = 3
	       ADD TRA-VALUE	 TO W35-90
           ELSE
           IF TRA-AGE = 2
	       ADD TRA-VALUE	 TO W35-60
           ELSE
           IF TRA-AGE = 1
	       ADD TRA-VALUE	 TO W35-30
	   ELSE
	   IF TRA-AGE = ZERO
	       ADD TRA-VALUE	 TO W35-CUR.
	     GO TO XB78.

022960 XB80-TRAN.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *	   Ё Background colour is used to show columns, to format the  Ё
      *    Ё document as the ASCII box characters can not be used with Ё
      *    Ё the windows print utility. This helps with readability    Ё
      *    Ё and gives the document a more professional look.	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 2		 TO fore-or-back.
	   IF PREVIOUS-STATEMENT
022970	       MOVE DEB-ACNO	 TO TRH-KEY
	       PERFORM START-AT-DTRH-KEY THRU READ-DTHIST-EXIT
	   ELSE
022970	       MOVE DEB-ACNO	 TO TRA-KEY
	       PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
	   IF WS-F-ERROR = 5 OR 55
022980	       GO TO XB90-SEND.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Get the next transaction to print on the statement.      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
023000 XB85-TLOOP.
	   IF PREVIOUS-STATEMENT
023010	       PERFORM READ-DTHIST-NEXT THRU READ-DTHIST-EXIT
	       MOVE TRH-RECORD1  TO TRA-RECORD1
	   ELSE
023010	       PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
	   IF (WS-F-ERROR = 5 OR 55) OR
              (TRA-AC NOT = DEB-ACNO)
	       GO TO XB90-SEND.
023020     IF TRA-VALUE = ZERO
023030	       GO TO XB85-TLOOP.
023040	     MOVE TRA-DATE	 TO W22-DTE2.
	     MOVE W22-CC2	 TO W22-CC3.
	     MOVE W22-YY2	 TO W22-YY3.
	     MOVE W22-MM2	 TO W22-MM3.
	     MOVE W22-DD2	 TO W22-DD3.
	     MOVE W22-DTE3	 TO ST7-TDTE.
	   IF WS-REF = "O"
	      IF NOT(TRA-REG = SPACES)
		  MOVE TRA-REG	 TO ST7-TREF
	      ELSE
		  MOVE TRA-REF	    TO ST7-TREF
	      END-IF
	   ELSE
023050	   IF NOT(TRA-REF = "00000000" OR SPACES)
023130	       MOVE TRA-REF	 TO ST7-TREF.
023070     IF DEB-LANG = "A"
023080	       MOVE W00-A-DESC(TRA-CODE) TO ST7-TDESC
023090     ELSE
023100	       MOVE W00-E-DESC(TRA-CODE) TO ST7-TDESC.
           IF TRA-VALUE > ZERO
023220	       MOVE TRA-VALUE	 TO ST7-TDBT
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		       A L L O C A T I O N S		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       IF NOT(TRA-ALLOCATED = ZERO)
                   MULTIPLY -1 BY TRA-ALLOCATED
				 GIVING ST7-TCRD
		   MOVE " Cr["	 TO ST7-PD
		   MOVE "]"	 TO ST7-CB
	       END-IF
           ELSE
023220	       MOVE TRA-VALUE	 TO ST7-TCRD.
             ADD TRA-VALUE       TO W90-BAL.
	     MOVE W90-BAL	 TO ST7-TBAL.
	     PERFORM PRINT-TRANSACTION-LINE.
023240	     MOVE SPACES	 TO ST7-DET.
023250       ADD 1               TO W25-WHOLE.
023260	   IF W25-WHOLE > WS-LINES
023270	       GO TO XB120-MULTI.
023310	     GO TO XB85-TLOOP.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  End of the STATEMENT details (transactions space). Print Ё
      *    Ё  end of page details and skip to the next page and print  Ё
      *    Ё  the headings and carried forward balance. 	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
023320 XB90-SEND.
023330	     ADD 1 WS-LINES	 GIVING W25-DECIMAL.
023340	     SUBTRACT W25-WHOLE  FROM W25-DECIMAL.
	   IF W25-DECIMAL > ZERO
023350	       PERFORM PRINT-TRANSACTION-LINE W25-DECIMAL TIMES.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	      A D V A N C E   P A P E R	  1   L I N E	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     perform advance-printer.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  Set to print Я 8 cpi and Bold		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     move font-8-CPI	 to font-size.
	     move 8		 to font-style.
	     perform set-printer-font.
	     MOVE 66		 to Print-Buffer-Len.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  W H I T E   B A C K G R O U N D	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 255		 TO color-red.
	     MOVE 255		 TO color-green.
	     MOVE 255		 TO color-blue.
	     perform printer-colour.
019250	     MOVE "REMITTANCE ADVICE"
				 TO ST7-HEADER.
	     MOVE 0		 TO WS-ERROR
				    WS-ERROR1.
	     move st7-det	 to Print-Buffer.
	     PERFORM PRINT-DETAILS.
023240	     MOVE SPACES	 TO ST7-DET.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		      Set to print Я 12cpi		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     move font-12-CPI	 to font-size.
	     move 0		 to font-style.
	     perform set-printer-font.
	     MOVE 96		 to Print-Buffer-Len.
021750	     MOVE W95-COMP	 TO ST7-NAME.
022170	     MOVE W40-NAME	 TO ST7-MESS.
	     move st7-det	 to Print-Buffer.
	     PERFORM PRINT-DETAILS.
021840	     MOVE SPACES	 TO ST7-DET.
	   IF WS-PRINT-BNK = "Y"
	       MOVE "Banking Details"
				 TO ST7-NAME
	   ELSE
021850	       MOVE W95-ADD1	 TO ST7-NAME.
022220	     MOVE W40-ADD1	 TO ST7-MESS.
	     move st7-det	 to Print-Buffer.
	     PERFORM PRINT-DETAILS.
021880	     MOVE SPACES	 TO ST7-DET.
	   IF WS-PRINT-BNK = "Y"
	       MOVE WS-BNK-NME	 TO ST7-NAME
	   ELSE
021920	       MOVE W95-ADD2	 TO ST7-NAME
022430	       IF W95-ADD2 = SPACES
022440		   MOVE 1	 TO WS-ERROR1
022450		   IF NOT(W95-POST = ZERO)
022460		       MOVE W95-POST
			     TO ST7-PC2.
022220	     MOVE W40-ADD2	 TO ST7-MESS.
022430	   IF W40-ADD2 = SPACES
022440         MOVE 1            TO WS-ERROR
022450	       IF NOT(DEB-POST = "0000" OR "9999")
022460		   MOVE DEB-POST TO ST7-MESS.
	     move st7-det	 to Print-Buffer.
	     PERFORM PRINT-DETAILS.
021880	     MOVE SPACES	 TO ST7-DET.
	   IF WS-PRINT-BNK = "Y"
	       MOVE WS-BRANCH	 TO ST7-NAME
	   ELSE
021970	       MOVE W95-ADD3	 TO ST7-NAME
022500	       IF WS-ERROR1 = 0
022520		   IF W95-ADD3 = SPACES
022530		       MOVE 1	 TO WS-ERROR1
022540		       IF NOT(W95-POST = ZERO)
022550			   MOVE W95-POST
				 TO ST7-PC2.
022220	     MOVE W40-ADD3	 TO ST7-MESS.
022500	   IF WS-ERROR = 0
022520         IF W40-ADD3 = SPACES
022530             MOVE 1        TO WS-ERROR
022540		   IF NOT(DEB-POST = "0000" OR "9999")
022550                 MOVE DEB-POST 
				 TO ST7-MESS.
	     move st7-det	 to Print-Buffer.
	     PERFORM PRINT-DETAILS.
021880	     MOVE SPACES	 TO ST7-DET.
	   IF WS-PRINT-BNK = "Y"
	       MOVE WS-BANK-ACCOUNT
				 TO ST7-NAME
	   ELSE
022020	       MOVE W95-ADD4	 TO ST7-NAME
022500	       IF WS-ERROR1 = 0
022520		   IF W95-ADD4 = SPACES
022530		       MOVE 1	 TO WS-ERROR1
022540		       IF NOT(W95-POST = ZERO)
022550			   MOVE W95-POST
				 TO ST7-PC2.
022220	     MOVE W40-ADD4	 TO ST7-MESS.
022500	   IF WS-ERROR = 0
022520         IF W40-ADD3 = SPACES
022530             MOVE 1        TO WS-ERROR
022540		   IF NOT(DEB-POST = "0000" OR "9999")
022550                 MOVE DEB-POST 
				 TO ST7-MESS.
	     move st7-det	 to Print-Buffer.
	     PERFORM PRINT-DETAILS.
021880	     MOVE SPACES	 TO ST7-DET.
	   IF WS-PRINT-BNK = "Y"
	       MOVE SPACES	 TO ST7-NAME
	   ELSE
022700	   IF WS-ERROR1 = 0
022710	       IF NOT(W95-POST = ZERO)
022720		   MOVE W95-POST
			     TO ST7-PC2.
022700	   IF WS-ERROR = 0
022710	       IF NOT(DEB-POST = "0000" OR "9999")
022720		   MOVE DEB-POST TO ST7-PC2.
	     move st7-det	 to Print-Buffer.
	     PERFORM PRINT-DETAILS.
022240	     MOVE SPACES	 TO ST7-DET.
	     MOVE "ACCOUNT NO:"  TO ST7-AH1
	     MOVE "DATE:"	 TO ST7-AH2R.
022600	     MOVE DEB-ACNO	 TO ST7-AC1.
022190	     MOVE W12-TODAY	 TO ST7-DATER.
	     move 2		 to WS-ADVANCE.
	     move st7-det	 to Print-Buffer.
	     PERFORM PRINT-DETAILS.
022740	     MOVE SPACES	 TO ST7-DET.
023410	     MOVE "INTEREST"	 TO ST7-A1.
023420	     MOVE "91+ DAYS"	 TO ST7-A2.
023430	     MOVE "61-90 DAYS"	 TO ST7-A3.
023560	     MOVE "31-60 DAYS"	 TO ST7-A4.
023570	     MOVE "01-30 DAYS"	 TO ST7-A5.
	     MOVE "  AMOUNT DUE" TO ST7-V3.
	     move 1		 to WS-ADVANCE.
	     move st7-det	 to Print-Buffer.
	     PERFORM PRINT-DETAILS.
023520	     MOVE SPACES	 TO ST7-DET.
023640	     MOVE DEB-INT	 TO ST7-INT.
023470	     ADD DEB-60 DEB-90 DEB-120
				 GIVING ST7-3MTHS.
023480	     MOVE DEB-30	 TO ST7-2MTHS.
023490	     MOVE DEB-CUR	 TO ST7-1MTH.
023630	     MOVE W90-CUR	 TO ST7-CUR.
023360	     MOVE DEB-BAL	 TO ST7-TBAL.
	     move 99		 to WS-ADVANCE.
	     move st7-det	 to Print-Buffer.
	     PERFORM PRINT-DETAILS.
023670	     MOVE SPACES	 TO ST7-DET.

023680 XB100-CHECK.
023690     IF WS-NTP = 1
023700	       GO TO XB125.
023730	     GO TO XB20-LOOP.

023750 XB110.
023760     IF DEB-LANG = "A"
023770         MOVE 1            TO WS-MSUB
023780     ELSE
023790         MOVE 2            TO WS-MSUB.
             MOVE SPACES         TO W30-STM.
023800     IF WS-MESS = "XX"
023810	       GO TO XB112.
023820     IF (DEB-90 > ZERO) OR (DEB-120 > ZERO)
023830		MOVE W30-90D (WS-MSUB)
				  TO W30-STM
023840	       GO TO XB115.
023850     IF WS-MESS = "90"
023860	       GO TO XB112.
023870     IF DEB-60 > ZERO
023880		MOVE W30-60D(WS-MSUB)
				  TO W30-STM
023890	       GO TO XB115.
023900     IF WS-MESS = "60"
023910	       GO TO XB112.
023920     IF DEB-30 > ZERO
023930		MOVE W30-30D(WS-MSUB)
				  TO W30-STM
023940	       GO TO XB115.

023950 XB112.
023960	     MOVE W30-MESS-A(WS-MSUB)
                                 TO W30-STM.

023970 XB115.
023980       EXIT.

023990 XB120-MULTI.
024020	     PERFORM XB90-SEND.
024030	     PERFORM XB50-STM THRU XB60-DEBT.
024040	     MOVE ZERO		 TO W25-WHOLE.
024060	     GO TO XB85-TLOOP.

024070 XB125.
             MOVE 1001           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
024090	     DISPLAY "Account No:" AT 1012.
	     PERFORM AA300.

       XB126.
024100	     ACCEPT W80-ACNO AT 1024
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W80-ACNO
		     GO TO XB127
016550		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO XB126
	       END-EVALUATE
024100	       DISPLAY W80-ACNO AT 1024
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5
018850         IF W80-ACNO = SPACES
018860		   GO TO XB126.
018870	     DISPLAY CLEAR-L25.

       XB127.
024110	   IF W80-ACNO = SPACES
024120	       GO TO XB200-END.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
024130	     PERFORM CA155-GET-DEBTOR.
024140     IF WS-ERROR NOT = ZERO
024150	       GO TO XB125.
	     DISPLAY DEB-NAME AT 1031
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
020240	     DISPLAY WS-CHK AT 1202.
020250	     ACCEPT WS-OPTION AT 1239
020260		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
020320	   IF WS-OPTION = "N"
020330	       GO TO XB125.
	     DISPLAY WS-BLNK78 AT 1202 WITH FOREGROUND-COLOR 3.
024160	     GO TO XB21.

024170 XB130.
             MOVE 1201           TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
024190	     DISPLAY "Account No:" AT 1212.
	     PERFORM AA300.

       XB131.
024200	     ACCEPT W80-ACNO AT 1224
024210		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W80-ACNO
		     GO TO XB132
016550		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO XB131
	       END-EVALUATE
024100	       DISPLAY W80-ACNO AT 1224
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5
018850         IF W80-ACNO = SPACES
018860		   GO TO XB131.
018870	     DISPLAY CLEAR-L25.

       XB132.
024220	   IF W80-ACNO = SPACES
024230	       GO TO XB200-END.
024240       PERFORM CA155-GET-DEBTOR.
024250     IF WS-ERROR NOT = ZERO
024260	       GO TO XB130.
             DISPLAY "Debtor account:" AT 1412.
	   IF PREVIOUS-STATEMENT
	       PERFORM START-AT-DEBH-NAME THRU READ-DBHIST-EXIT
	   ELSE
	       PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
	   IF WS-F-ERROR = 6 OR 56
	       GO TO XB130.
	     PERFORM XB14.
	   IF WS-RANGE = "R"
	       MOVE SPACES	 TO WS-AEND
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	Save screen details and display the get range panel.   Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       PERFORM XB300
	       MOVE 0726	 TO COL-AND-LINE SPOS
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	Display the restart account in the range to printed.   Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       DISPLAY W80-ACNO AT SPOS
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 5 AUTO
	       ADD 1		 TO SLIN
	       DISPLAY DEB-NAME AT SPOS
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       MOVE W80-ACNO	 TO WS-ASTART
	       MOVE 0926	 TO SPOS
	       MOVE SPOS	 TO COL-AND-LINE
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	  Last account in the range that must be printed.      Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	       PERFORM XB500
	       IF WS-ERROR = 9
		   GO TO XB299-EXIT
	       END-IF
	       MOVE W80-ACNO	 TO WS-AEND
	       PERFORM XB400
	   END-IF.

024320 XB135.
	   IF PREVIOUS-STATEMENT
	       PERFORM READ-DBHIST-NEXT THRU READ-DBHIST-EXIT
	       MOVE DBH-RECORD1  TO DEB-RECORD1
	   ELSE
	       PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
	   IF WS-F-ERROR = 6 OR 56
	       GO TO XB130.
024850     IF W80-ACNO NOT = DEB-ACNO
	       GO TO XB135.
024960	     GO TO XB21.

024970 XB200-END.
	     EXIT.

025000 XB299-EXIT.
025010	     EXIT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	Save screen details and display the get range panel.   Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       XB300	       SECTION 51.
       XB305.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 15		 TO SHADE-COL.
	     MOVE 53		 TO SHADE-WIDTH.
	     MOVE 9		 TO SHADE-LINES.
	     DISPLAY S05D.
	     PERFORM SCREEN-SHADOW.

       XB399.
	     EXIT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    ЁStart reading from the first account in the range to print.Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       XB400	       SECTION 51.
       XB405.
024220	     MOVE WS-ASTART	 TO W80-ACNO.
024240	     PERFORM CA155-GET-DEBTOR.
	   IF PREVIOUS-STATEMENT
	       PERFORM START-AT-DEBH-NAME THRU READ-DBHIST-EXIT
	   ELSE
	       PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
	     PERFORM RESTORE-SCREEN-3.

       XB499.
	     EXIT.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Get account numbers in the range that must be printed.   Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       XB500	       SECTION 51.
       XB505.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Display the USER options available for this input screen. Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     PERFORM AA300.

       XB515.
	     MOVE COL-AND-LINE	 TO SPOS.
024100	     ACCEPT W80-ACNO AT SPOS
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W80-ACNO
		     GO TO XB520
016550		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5" CALL X"E5"
		     GO TO XB515
	       END-EVALUATE
024100	       DISPLAY W80-ACNO AT SPOS
		       WITH FOREGROUND-COLOR 7 HIGHLIGHT
			    BACKGROUND-COLOR 5
018850         IF W80-ACNO = SPACES
018860		   GO TO XB515.
018870	     DISPLAY CLEAR-L25.

       XB520.
018880	   IF W80-ACNO = SPACES
	       MOVE 9		 TO WS-ERROR
018900	       GO TO XB599.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
018870	     DISPLAY CLEAR-L25.
024130	     PERFORM CA155-GET-DEBTOR.
024140     IF WS-ERROR NOT = ZERO
024150	       GO TO XB515.
	     ADD 1		 TO SLIN.
	     DISPLAY DEB-NAME AT SPOS
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO XB505.

       XB599.
	     EXIT.
      /
       YA000		      SECTION 80.
       YA00.
019040       DISPLAY CLR-SCREEN.
	     DISPLAY "DEBTORS (ACCOUNTS RECEIVABLE) STATEMENTS" AT 0321
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT.

       YA05.
	     MOVE "'P'revious or 'C'urrent statements  [C]"
				 TO WS-ERR-MES.
	     MOVE "C"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "C" OR "P".
	     MOVE WS-OPTION	 TO WS-STMT.
	   IF NOT PREVIOUS-STATEMENT
	       GO TO YA999.

       YA10.
019040	     DISPLAY CLR-SCREEN.
	     DISPLAY "DEBTORS (A/R) PREVIOUS STATEMENTS" AT 0325
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
      *
      *			      здддддддддддддддддд©
      *			      ЁPREVIOUS STATEMENTЁ╡╡
      *			      Ё Year : YY (00-99)Ё╡╡
      *			      Ё Month: MM (01-12)Ё╡╡
      *			      юдддддддддддддддддды╡╡
      *				╡╡╡╡╡╡╡╡╡╡╡╡╡╡╡╡╡╡╡╡
      *

	     DISPLAY "здддддддддддддддддд" AT 0631
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "©" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "Ё" AT 0731 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				      BACKGROUND-COLOR 5
		     "PREVIOUS STATEMENT"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "Ё" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "Ё " AT 0831 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				       BACKGROUND-COLOR 5
		     "Year :    (00-99)" WITH FOREGROUND-COLOR 2
					      BACKGROUND-COLOR 5
		     "Ё" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "Ё " AT 0931 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				       BACKGROUND-COLOR 5
		     "Month:    (01-12)" WITH FOREGROUND-COLOR 2
					      BACKGROUND-COLOR 5
		     "Ё" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "ю" AT 1031 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				      BACKGROUND-COLOR 5
		     "дддддддддддддддддды" WITH FOREGROUND-COLOR 0
						BACKGROUND-COLOR 5.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 33		 TO SHADE-COL.
	     MOVE 18		 TO SHADE-WIDTH.
	     MOVE 4		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
	     MOVE W12-YEAR	 TO W05-DYY.
	     MOVE W12-MONTH	 TO W05-DMM.
      *    ************************************************************
      *    ****  Y E A R   O F	 P R E V I O U S   S T A T E M E N T
      *    ************************************************************
       YA15.
	     ACCEPT S80-YEAR.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "C"	 TO WS-STMT
		     GO TO YA00
		 WHEN OTHER
		     CALL X"E5" CALL X"E5" CALL X"E5" CALL X"E5"
		     GO TO YA15
	       END-EVALUATE.
	   IF W05-DYY > W12-YEAR
	       MOVE "Year can not be > Current Year"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO YA15.
      *    ************************************************************
      *    *** M O N T H   O F	 P R E V I O U S   S T A T E M E N T
      *    ************************************************************
       YA20.
	     ACCEPT S80-MONTH.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO YA15
		 WHEN OTHER
		     CALL X"E5" CALL X"E5" CALL X"E5" CALL X"E5"
		     GO TO YA20
	       END-EVALUATE.
	   IF W05-DYY > W12-YEAR
	       MOVE "Month can not be < 01 or > 12"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO YA20.
019040	     DISPLAY CLR-SCREEN.
	     DISPLAY "DEBTORS (A/R) PREVIOUS STATEMENTS" AT 0325
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT.
      *    *************************************************************
      *    ****    Create paths and file names for history files    ****
      *    *************************************************************
	     MOVE W05-DYY	 TO W05-TYY.
	     MOVE W05-DMM	 TO W05-TMM.
	     INSPECT W02-DBHIST REPLACING FIRST "AR_ACNT.DAT"
			       BY "           ".
	     MOVE W02-DBHIST	 TO W60-ADDRESS.
	     STRING W60-ADDRESS DELIMITED BY " "
		    W05-DEB-HIS DELIMITED SIZE
		    INTO W02-DBHIST.
	     INSPECT W02-DTHIST REPLACING FIRST "AR_TRAN.DAT"
			       BY "           ".
	     MOVE W02-DTHIST	 TO W60-ADDRESS.
	     STRING W60-ADDRESS DELIMITED BY " "
		    W05-DBT-HIS DELIMITED SIZE
		    INTO W02-DTHIST.
	     OPEN I-O DBHIST.
000040	   IF NOT(WS-STATUS = "00")
	       PERFORM YA200
	       GO TO YA00.
	     OPEN I-O DTHIST.
000040	   IF NOT(WS-STATUS = "00")
	       PERFORM YA200
	       GO TO YA00.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Set up the run / age dates applicable to the history filesЁ
      *    Ё when the statements and ageing of balances was executed byЁ
      *    Ё the system. These dates are stored in the first record of Ё
      *    Ё the history account file, which is a dummy account with   Ё
      *    Ё an account number of "     !" and a name "  !".	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE "     !"	 TO DBH-ACNO.
	     PERFORM READ-DBHIST THRU READ-DBHIST-EXIT.
	   IF WS-F-ERROR = 56
	       GO TO YA200.
	     MOVE DBH-W30-AGE-DATES
				 TO W30-AGE-DATES.
	     MOVE DBH-STM-DTE	 TO W12-TODAY.
	     GO TO YA999.

       YA200.
	     MOVE "Details for period requested not available"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
	     CLOSE DBHIST
		   DTHIST.

       YA999.
	       EXIT.

      /
025020 ZA00-INIT	      SECTION 90.
025030 ZA00-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
042000	     DISPLAY CLR-SCREEN.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Setup various keys and the mouse for use with menus and  Ё
      *    Ё  other required user interface functions and procedures.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     COPY FUNCTION.PRO.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	    Setup the file ID's including path names           Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *    *************************************************************
      *    ****    P A R A M E T E R   F I L E
      *    *************************************************************
       ZA00A.
	     MOVE "PARAM"	 TO AFID-KEY.

       ZA00-READ-APACFIDS.
000030	     READ APACFIDS WITH IGNORE LOCK
	       KEY IS AFID-KEY.
	   IF WS-STATUS = "00"
	       GO TO ZA00-READ-APACFIDS-EXIT.
006260	     STRING "Missing " DELIMITED SIZE
		     AFID-KEY DELIMITED BY " "
		     " file ID - Status " DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370	     STOP RUN.

       ZA00-READ-APACFIDS-EXIT.
	     EXIT.

       ZA00A-CONTINUE.
	     MOVE AFID-PATH	 TO W02-PARAM.
	     MOVE "DBTCON"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-DBTCON.
      *    *************************************************************
      *    ****    D E B T O R	 T R A N S A C T I O N	 F I L E    ****
      * 		Setup the Transaction History path
      *    *************************************************************
	     MOVE "DBTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBTRAN W02-DTHIST.
      *    *************************************************************
      *    **** 	  D E B T O R	M E M O	  F I L E	    ****
      *    *************************************************************
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
      *    *************************************************************
      *    ****  D E B T O R   A C C O U N T   ( A / R )   F I L E  ****
      * 		  Setup the Account History path
      *    *************************************************************
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR W02-DBHIST.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    ****      N E T W O R K   (S H A R E D)   F I L E	    ****
      *    *************************************************************
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
      *    *************************************************************
      *    ****      S T A T E M E N T	 B I T M A P   F I L E	    ****
      *    *************************************************************
	     MOVE "STMBMP"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     STRING AFID-PATH DELIMITED BY " "
		    x"00" DELIMITED SIZE
		    INTO BmpFilename.
	   IF LS-USER NUMERIC
	       IF LS-USE > ZERO AND < 110
		   MOVE LS-USE	 TO WS-USUB
                   GO TO ZA01A.
             MOVE 110            TO WS-USUB.

       ZA01A.
014240       ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
014250       PERFORM READ-PARAM THRU READ-PARAM-EXIT.

       ZA02.
	     MOVE 6		 TO WS-PARKEY.
025460       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
011800	   IF NOT(PAR-CON-STM = "N" OR "Y")
011810	       MOVE "N" 	 TO PAR-CON-STM.
	   IF NOT(PAR-CON-TYPE = "A" OR "S")
	       MOVE "S" 	 TO PAR-CON-TYPE.
	     MOVE PAR-CON-STM	 TO W05-CON-STM.
	     MOVE PAR-CON-TYPE	 TO W05-CON-TYPE.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Test if the system has been setup for USB/WINDOWS printer Ё
      *    Ё and if not set default detail lines per statement to 34.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF PAR-STM-DET< 25 OR > 38
	       MOVE 34		 TO PAR-STM-DET
	       REWRITE PAR-RECORD1.
	     MOVE PAR-STM-DET	 TO WS-LINES.
025430	     MOVE 11		 TO WS-PARKEY.
025440	     MOVE 1		 TO WS-SUB-2.

025450 ZA15-READ-TRANS.
025460       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
025490	     MOVE 1		 TO WS-SUB-1.

025500 ZA20-TRAN-LOOP.
025510	     MOVE PAR-T-CODE(WS-SUB-1)
				 TO W00-TRAN-CODE(WS-SUB-2).
025520	     MOVE PAR-E-DESC(WS-SUB-1)
				 TO W00-E-DESC(WS-SUB-2).
025530	     MOVE PAR-A-DESC(WS-SUB-1)
				 TO W00-A-DESC(WS-SUB-2).
025540	     MOVE PAR-ACTION(WS-SUB-1)
				 TO W00-ACTION(WS-SUB-2).
025550	     ADD 1		 TO WS-SUB-2.
025560     IF WS-SUB-1 < 2
025570	       ADD 1		 TO WS-SUB-1
025580         GO TO ZA20-TRAN-LOOP.
025590     IF WS-PARKEY < 60
025600	       ADD 1		 TO WS-PARKEY
025610         GO TO ZA15-READ-TRANS.

025620 ZA25.
	     MOVE 4		 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-AGE	 TO WS-AGE.
025630	     MOVE 1		 TO WS-PARKEY.
025640       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
025650	     MOVE PAR-COMPANY	 TO W95-COMP.
025660	     MOVE PAR-DMY	 TO W12-TODAY.
025670	     MOVE PAR-YMD	 TO W12-T-YMD.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё The use of letter heads is not yet available in this ver- Ё
      *    Ё sion and will be included later.			       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
025680*    IF PAR-STMINV = "S" OR "X"
025690*        MOVE "N"		 TO WS-HEAD
025700*    ELSE
025710*        MOVE "Y"		 TO WS-HEAD.
      *    *************************************************************
      *    ****   G E T   D E B T O R	M O N T H - E N D   D A Y   ****
      *    *************************************************************
025730	     MOVE 1		 TO WS-NETKEY.
025740       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
      *
      *    ****    S E T   U P   A G E   D A T E S
      *
	     MOVE W12-T-YMD	 TO W22-DTE2.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё The month end dates are stored in the Control file in the Ё
      *    Ё first record as Cur, 30d, 60d, 90d and 120d and the next  Ё
      *    Ё routine is used since Ver 8.11.06. 		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE DEB-CUR-DTE	 TO W30-CUR.
	     MOVE DEB-30D-DTE	 TO W30-30.
	     MOVE DEB-60D-DTE	 TO W30-60.
	     MOVE DEB-90D-DTE	 TO W30-90.
	     MOVE DEB-120-DTE	 TO W30-120.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Get the banking details to be used for payments via EFT.  Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
025730	     MOVE 4		 TO WS-NETKEY.
025740       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
	     MOVE G-BANK-NAME	 TO WS-BNK-NME.
	     MOVE G-BANK-AC-TYP  TO WS-ACT-TYP.
	     MOVE G-BANK-CDE	 TO WS-BR-CODE.
	   IF G-BNK-AC-LNGTH = 10
	       MOVE G-BANK-AC2	 TO WS-ACT-NO
	   ELSE
	       MOVE G-BANK-AC	 TO WS-ACT-NO.
	   IF WS-ACT-TYP = "C" OR "T" OR "S" OR "R"
	       MOVE "Y" 	 TO WS-PRINT-BNK
	       IF WS-ACT-TYP = "C"
		   MOVE "Cheque" TO WS-ACT-TYPE
	       ELSE
	       IF WS-ACT-TYP = "T"
		   MOVE "Transmission"
				 TO WS-ACT-TYPE
	       ELSE
	       IF WS-ACT-TYP = "S"
		   MOVE "Savings"
				 TO WS-ACT-TYPE
	       ELSE
	       IF WS-ACT-TYP = "R"
		   MOVE "Credit Card"
				 TO WS-ACT-TYPE.
025830	     GO TO ZA999-EXIT.

       COPY ZA49.PRO.

      *    *************************************************************
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *    *************************************************************
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 ZA60.
041250	     MOVE WS-G1 	 TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
	     MOVE WS-G8 	 TO WS-TCH(WS-S1)  WS-BCH(WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
	     MOVE WS-G9 	 TO WS-TCH(1).
	     MOVE WS-G10	 TO WS-TCH(80).
	     MOVE WS-G11	 TO WS-BCH(1).
	     MOVE WS-G12	 TO WS-BCH(80).
	     MOVE WS-G14	 TO WS-TCHR(1)	WS-BCHR(1).
	     MOVE WS-G13	 TO WS-TCHR(80) WS-BCHR(80).
021930	     MOVE WS-G2 	 TO WS-TCHR(8)	WS-TCHR(49)
021940				    WS-TCHR(61) WS-TCHR(72).
021950	     MOVE WS-G3 	 TO WS-MCHR(8)	WS-MCHR(49)
021960				    WS-MCHR(61) WS-MCHR(72)
021960				    WS-MCHR(1)	WS-MCHR(80).
021970	     MOVE WS-G4 	 TO WS-BCHR(8)	WS-BCHR(49)
021980				    WS-BCHR(61) WS-BCHR(72).
	     MOVE WS-G13	 TO WS-TCHR(80) WS-BCHR(80).
	     MOVE WS-G14	 TO WS-TCHR(1)	WS-BCHR(1).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "Supervisorд"
				 TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"
				 TO WS-WRKHD
	       MOVE LS-USER
				 TO WS-WRKST.
025840 ZA200.

       COPY LOCKED.PRO.

       ZA205.
             EXIT PROGRAM.

025920 ZA999-EXIT.
025930       EXIT.
      /
025940 ZB000		     SECTION 91.
      
       COPY ERRORS.PRO.

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK  TO WS-FILE
041630         MOVE WS-NETKEY    TO WS-KEY
041720     ELSE
041730     IF WS-F-ERROR = 5
041740         MOVE W02-DBTRAN   TO WS-FILE
041750         MOVE ZERO         TO WS-KEY
               MOVE TRA-KEY      TO WS-KEYX
041760     ELSE
041770     IF WS-F-ERROR = 6
041780         MOVE W02-DEBTOR   TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041640     ELSE
041650     IF WS-F-ERROR = 55
041660	       MOVE W02-DTHIST	 TO WS-FILE
041670	       MOVE ZERO	 TO WS-KEY
	       MOVE TRH-KEY	 TO WS-KEYX
041760     ELSE
041770	   IF WS-F-ERROR = 56
041780	       MOVE W02-DBHIST	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE DBH-ACNO	 TO WS-KEYX.

       COPY DISPERR.PRO.

       9999-Errors     section 91.
       9999-Display-Error.

	   display Operation "Error: " Return-Code.
           evaluate Return-Code
               when 0  display "Success"
               when 1  display "Could not open printer device"
               when 2  display "Invalid printer control code specified"
               when 3  display
                   "No printer device associated with specified handle"
               when 4  display "Out of memory while printing"
               when 5  display "Failed to open file"
               when 6  display "Disk full while spooling file"
               when 7  display
                   "Print job aborted. No job sent to print spooler"
               when 8  display
                   "Printer information structure badly constructed"
               when 9  display "No default printer found"
               when 10 display "Error attempting to display dialog"
               when 11 display "Write failure"
               when 12 display "No fonts found usable with this printer"
               when 13 display "The font requested does not exist"
               when 14 display "User aborted print job"
               when 15 display "Reserved"
               when 16 display "Reserved"
               when 17 display "Reserved"
               when 18 display "Failed to load bitmap"
               when 19 display "Invalid bitmap id"
               when 20 display "Failed to free bitmap"
               when 21 display "Failed to print bitmap"
               when 22 display "Bad parameter"
               when 23 display "Internal error"
               when 24 display "User pressed Cancel on Dialog box"
               when other display "Unknown error"
	   end-evaluate.

	   stop run.
