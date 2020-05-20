      $SET LINKCOUNT"640"
      ******************************************************************
      *                                                                *
      *    *******   ********  *******	 *******   *******   ********  *
      *    **	 **	**     **    **  **    **  **	 **	**     *
      *    **	 **	**     **    **  **    **  **	 **	**     *
      *    **	 **	**     *******	 **    **  *******	**     *
      *    **	 **	**     **	 **    **  **		**     *
      *    **	 **	**     **	 **    **  **		**     *
      *    *******	**     **	 *******   **		**     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *  D E B T O R ' S   S A L E S   R E S T R I C T I O N S   P R G *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  Feb 2007	- New file (DEBDEP/AR_REST.DAT) - Debtor sales *
      * 		  restrictions file has been included which    *
      * 		  will allow for departments to de excluded or *
      * 		  only selected departments allowed. This will *
      * 		  allow the user the abillity to allow or bar  *
      * 		  sales of selected departments.	       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTPDPT.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   FEBRUARY 2007.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2007 - 2011
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
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

000160 COPY DEBDEP.SL.

000140 COPY DEBMEM.SL.

000140 COPY DEBTOR.ISL.

000210 COPY DEPART.SL.

000120 COPY PARAM.SL.

000220 COPY RECOVER.SL.

       COPY SHARED.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000330 COPY DEBDEP.FDE.

000140 COPY DEBMEM.FDE.

000310 COPY DEBTOR.FDE.

000500 COPY DEPART.FDE.

000350 COPY PARAM.FDE.

000390 COPY RECOVER.GFD.

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
001550 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001560 77  WS-SUB	   PIC	9(04)	 COMP-5.
001560 77  WS-S            PIC  9(04)    COMP-5.
001570 77  WS-S1           PIC  9(04)    COMP-5.
001580 77  WS-S2           PIC  9(04)    COMP-5.
001590 77  WS-S3           PIC  9(04)    COMP-5.
001600 77  WS-S4           PIC  9(04)    COMP-5.
001610 77  WS-S5           PIC  9(04)    COMP-5.
001620 77  WS-S6           PIC  9(04)    COMP-5.
001630 77  WS-S7           PIC  9(04)    COMP-5.
001640 77  WS-S8           PIC  9(04)    COMP-5.
001650 77  WS-S9           PIC  9(04)    COMP-5.
002850 77  WS-IXS1         PIC  9(04)    COMP-5.
001670 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001710 77  WS-RECKEY       PIC  9(06)    COMP-5.
001720 77  WS-RECOVER	   PIC	9(06)	 COMP-5.
001660 77  WS-SHARED	   PIC	9(06)	 COMP-5.
001730 77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
001820 77  WS-OPTION       PIC  X(01).
       77  WS-RESTRICT	   PIC	X(01).
       77  WS-RESTCODE	   PIC	X(01).
001850 77  WS-ERROR        PIC  9(01) VALUE ZERO.
       77  WS-RT-HD	   PIC	X(10) VALUE "-Retail   ".
       77  WS-WS-HD	   PIC	X(10) VALUE "-Wholesale".
       77  WS-CS-HD	   PIC	X(10) VALUE "-Cash sale".
002010 77  WS-ERR1	   PIC	X(22) VALUE "Invalid Account Number".
002020 77  WS-ERR2         PIC  X(09) VALUE "No Record".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
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
003290	   03  WS-MID-LNE.
003300	       05  WS-MCHR  PIC  X(80) VALUE "³                ³
      -        "                      ³          ³         ³         ³".
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
003350	   03  WS-MD-LNE.
003360	       05  WS-MCH   PIC  X(80) VALUE "³      ³
      -        "                      ³           ³          ³       ³".
	   03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
002450	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
002470	   03  WS-BOT-LNE.
002480	       05  WS-BCHR PIC	X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
	       05  FILLER  PIC	X(01).
               05  WS-F-LINE
			   PIC	X(78).
	       05  FILLER  PIC	X(01).
002470     03  WS-BOT-LNE2.
002480	       05  WS-BCH  PIC	X(01) OCCURS 80.
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250	       05  FILLER  PIC	X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270			   PIC	X(50).
                  07  FILLER
			   PIC	X(10).
003280	       05  FILLER  PIC	X(19).
           03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

002230 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "DTP".
	   03  WS-PROG	       PIC  X(03) VALUE "DPT".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002310 01  W02-FID.

       COPY APACFIDS.ID.

000140 COPY CONTROL.ID.

000330 COPY DEBDEP.ID.

000140 COPY DEBMEM.ID.

       COPY DEBTOR.ID.

003760 COPY DEPART.ID.

       COPY PARAM.ID.

       COPY RECOVER.ID.

       COPY SHARED.ID.

       COPY W12.WS.

       COPY W20.WS.

002850 01  W25-CALCS.
002860	   03  W25-RESULT      PIC  9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880	       05  W25-WHOLE   PIC  9(05).
002890	       05  W25-DECIMAL PIC  9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
	       05  W25-KEY     PIC  9(04).
	       05  W25-SUB     PIC  9(01).
	       05  FILLER      PIC  9(02).
002900     03  W25-TOTAL       PIC S9(09)V99 COMP-3.
002910     03  W25-VALUE       PIC S9(09)V99 COMP-3.
001660	   03  W25-TIME        PIC  9(08).
001670     03  W25-TRED REDEFINES  W25-TIME.
001680	       05  FILLER      PIC  9(02).
001690	       05  W25-FACT    PIC  9(06).
	       05  W25-DATE REDEFINES W25-FACT.
		   07  W25-YY  PIC  9(02).
		   07  W25-MM  PIC  9(02).
		   07  W25-DD  PIC  9(02).
	   03  W25-CUR-CC      PIC  9(02) COMP-5.
	   03  W25-PRV-CC      PIC  9(02) COMP-5.
	   03  W25-NXT-CC      PIC  9(02) COMP-5.

       COPY W40.WS.

       COPY W40.DBT.

       COPY FUNCTION.WS.

003530 COPY W50.WS.

003550 01  W60-NME-ADD.
003560     03  W60-NAME.
003570         05  W60-NCHAR OCCURS 40 PIC X(01).
003580     03  W60-ADDRESS.
003590         05  W60-ACHAR OCCURS 70 PIC X(01).
003580     03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).
003600     03  W60-ADDRESS1.
               05  W60-PAD     PIC X(60).
               05  FILLER      PIC X(10).

       01  W75-RESTRICTIONS.
	   03  W75-DEPARTMENTS.
	       05  W75-DEPT OCCURS 10.
		   07  W75-DEPT-CDE
			       PIC  X(04).
		   07  W75-DEPT-DSC
			       PIC  X(30).
	   03  W75-DEP-CODES REDEFINES W75-DEPARTMENTS.
	       05  W75-DCDE-1  PIC  X(04).
	       05  W75-DDSC-1  PIC  X(30).
	       05  W75-DCDE-2  PIC  X(04).
	       05  W75-DDSC-2  PIC  X(30).
	       05  W75-DCDE-3  PIC  X(04).
	       05  W75-DDSC-3  PIC  X(30).
	       05  W75-DCDE-4  PIC  X(04).
	       05  W75-DDSC-4  PIC  X(30).
	       05  W75-DCDE-5  PIC  X(04).
	       05  W75-DDSC-5  PIC  X(30).
	       05  W75-DCDE-6  PIC  X(04).
	       05  W75-DDSC-6  PIC  X(30).
	       05  W75-DCDE-7  PIC  X(04).
	       05  W75-DDSC-7  PIC  X(30).
	       05  W75-DCDE-8  PIC  X(04).
	       05  W75-DDSC-8  PIC  X(30).
	       05  W75-DCDE-9  PIC  X(04).
	       05  W75-DDSC-9  PIC  X(30).
	       05  W75-DCDE-10 PIC  X(04).
	       05  W75-DDSC-10 PIC  X(30).

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
           03  W75-DNO        PIC X(06) OCCURS 18.
	   03  W75-SLEDG      PIC  9(03)    COMP-3 OCCURS 10.

003750 01  W75-TITLES.
003760     03  W75-DESCRIP.
003770         05  FILLER      PIC X(28)      VALUE
003780                     "Mr     Mnr    Mrs    Mev    ".
003790         05  FILLER      PIC X(28)      VALUE
003800                     "Miss   Mej    Ms     Ms     ".
003810         05  FILLER      PIC X(28)      VALUE
003820                     "Dr     Dr     Rev    Ds     ".
003830         05  FILLER      PIC X(28)      VALUE
003840                     "Prof   Prof   The HonSy Edel".
003850         05  FILLER      PIC X(28)      VALUE
003860                     "                            ".
003870     03  W75-CODES REDEFINES W75-DESCRIP.
003880         05  W75-TITLE  OCCURS 10.
003890             07  W75-ETITLE  PIC X(07).
003900             07  W75-ATITLE  PIC X(07).

003910 01  W80-ACCOUNT.
003920     03  W80-ACNO        PIC X(06).
003940     03  W80-TYPE        PIC 9(01).

004490 01  W100-EDIT.
004500     03  W100-LBAL       PIC Z(06)9.99-.
004500     03  W100-LARR       PIC Z(06)9.99.
004500     03  W100-BAL        PIC Z(08)9.99-.
004510     03  W100-CUR        PIC Z(08)9.99-.
004520     03  W100-30         PIC Z(08)9.99-.
004530     03  W100-60         PIC Z(08)9.99-.
004540     03  W100-90         PIC Z(08)9.99-.
004540     03  W100-120        PIC Z(08)9.99-.
004550     03  W100-INT        PIC Z(08)9.99.
007100	   03  W100-VAL        PIC Z(08)9.99-.
007120	   03  W100-V2.
007130	       05  W100-S6V2   PIC Z(05)9.99-.
007140	   03  W100-V3 REDEFINES W100-V2.
007150         05  W100-PRICE  PIC X(09).
007160         05  FILLER      PIC X(01).
007170	   03  W100-V4 REDEFINES W100-V2.
007180         05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.
004560	   03  W100-CRL        PIC Z(06)9.
004570     03  W100-KEY        PIC Z(05)9.

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

004580 SCREEN SECTION.

       COPY BLANK.CRT.

005000 01  S05.
005010	   03  LINE  3 COLUMN 37 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "COMPANY".

005020 01  S06.
005030	   03  LINE  3 COLUMN 37 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE "INTERNAL".

       COPY DEBLUP.CRT.

       COPY DEBTOR.CRT.

007910 01  S36.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(06) USING W80-ACNO AUTO.

       01  S40.
	   03  S40-REST.
	   05  LINE  8 COLUMN 74 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  X(01) USING WS-RESTCODE AUTO.
	   03  S40-1A.
	   05  LINE 12 COLUMN 41 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(04) USING W75-DCDE-1 AUTO.
	   03  S40-1D.
	   05  LINE 12 COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(30) FROM W75-DDSC-1.
	   03  S40-2A.
	   05  LINE 13 COLUMN 41 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(04) USING W75-DCDE-2 AUTO.
	   03  S40-2D.
	   05  LINE 13 COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(30) FROM W75-DDSC-2.
	   03  S40-3A.
	   05  LINE 14 COLUMN 41 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(04) USING W75-DCDE-3 AUTO.
	   03  S40-3D.
	   05  LINE 14 COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(30) FROM W75-DDSC-3.
	   03  S40-4A.
	   05  LINE 15 COLUMN 41 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(04) USING W75-DCDE-4 AUTO.
	   03  S40-4D.
	   05  LINE 15 COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(30) FROM W75-DDSC-4.
	   03  S40-5A.
	   05  LINE 16 COLUMN 41 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(04) USING W75-DCDE-5 AUTO.
	   03  S40-5D.
	   05  LINE 16 COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(30) FROM W75-DDSC-5.
	   03  S40-6A.
	   05  LINE 17 COLUMN 41 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(04) USING W75-DCDE-6 AUTO.
	   03  S40-6D.
	   05  LINE 17 COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(30) FROM W75-DDSC-6.
	   03  S40-7A.
	   05  LINE 18 COLUMN 41 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(04) USING W75-DCDE-7 AUTO.
	   03  S40-7D.
	   05  LINE 18 COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(30) FROM W75-DDSC-7.
	   03  S40-8A.
	   05  LINE 19 COLUMN 41 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(04) USING W75-DCDE-8 AUTO.
	   03  S40-8D.
	   05  LINE 19 COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(30) FROM W75-DDSC-8.
	   03  S40-9A.
	   05  LINE 20 COLUMN 41 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(04) USING W75-DCDE-9 AUTO.
	   03  S40-9D.
	   05  LINE 20 COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(30) FROM W75-DDSC-9.
	   03  S40-10A.
	   05  LINE 21 COLUMN 41 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(04) USING W75-DCDE-10 AUTO.
	   03  S40-10D.
	   05  LINE 21 COLUMN 46 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
				 PIC  X(30) FROM W75-DDSC-10.

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

       COPY DEPART1.CRT.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
007730 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
003240	   IF LS0-DBLEV < 5
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
010270	     PERFORM ZA000-INIT.
	     PERFORM BA000.
013040	     CLOSE DEBDEP
		   RECOVER.

       AA49.
013150       EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
010910 AA50.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
011180	     PERFORM DEBTOR-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
011360 AA99.
011370       EXIT.

       COPY FUNCTION.CRT.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			  OPT-MESSAGE			       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine is used to allow the OPERATOR to respond to  ³
      *    ³ a request for an option, so that the correct action can   ³
      *    ³ be performed by the program. The routine will display the ³
      *    ³ message in a pop-up window and allow the OPERATOR to      ³
      *    ³ respond to the 'question'. 			       ³
      *    ³							       ³
      *    ³ The option request must be placed in WS-ERR-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³							       ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      ³
      *    ³   PERFORM OPT-MESSAGE.				       ³
      *    ³							       ³
      *    ³ This would be displayed as:			       ³
      *    ³	ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿     ³
      *    ³	³	   Print transactions (Y/N) [ ] 	 ³°°   ³
      *    ³	ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°   ³
      *    ³	  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°   ³
      *    ³							       ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³							       ³
      *    ³ To display the request message higher or lower (default   ³
      *    ³ is line 13) move the line number to be used to SLIN and   ³
      *    ³ then PEFORM OPT-SETUP THRU OPT-EXIT.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY OPTION.CRT.

011390 COPY LOCKED.RC2.

011410 COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
011650 AC000              SECTION.

       COPY CONTROL.RD.

011190 COPY DEBDEP.RD.

000140 COPY DEBMEM.RD.

011710 COPY DEBTOR.RD.

011210 COPY DEPART.RD.

011290 COPY PARAM.RD.

000140 COPY SHARED.RD.

      /
      *    ****    WRITE FILES ROUTINES
      *
011650 AD000		  SECTION.

       COPY CONTROL.WR.

       COPY DEBDEP.WR.

       COPY DEBTOR.WR.

       COPY PARAM.WR.

011970 COPY DEBTOR.ILU.

       COPY DEPART1.LUP.

       COPY APAC.HLP.
      /
      *       ****   *****   ***    ***   *   *  *****	****   *   *
      *       *   *  *	    *	*  *   *  *   *  *	*   *  *   *
      *       ****   ***    *	   *   *  *   *  ***	****	* *
      *       *   *  *	    *	*  *   *   * *	 *	*   *	 *
      *       *   *  *****   ***    ***     *	 *****	*   *	 *
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      * 	 THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *        ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *        ³ ORIGINAL ACTION (REC-TYPE)  ÄÄ  RECOVERY PROCESS ³
      *        ³ 0 = RECORD CHANGED	     ÄÄ     (REWRITE)	   ³
      *        ³ 1 = RECORD WAS ADDED	     ÄÄ     (DELETE)	   ³
      *        ³ 2 = RECORD WAS DELETED      ÄÄ     (WRITE)	   ³
      *        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
010910 AY000	       SECTION.
010970 AY02.
010980       MOVE 02             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-DEBTOR-LOCK THRU READ-DEBTOR-EXIT.
011000       MOVE DEB-RECORD1    TO REC-DEBTOR.
011010       GO TO AY50.
       AY33.
	     MOVE 33		 TO REC-FILE.
	     MOVE ZERO		 TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
	     MOVE DBD-RECORD1	 TO REC-DEBDEP.
             GO TO AY50.
       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.
011120 AY49.
011130       MOVE 99             TO REC-FILE.
011140       MOVE ZERO           TO REC-KEY.
011150       MOVE SPACES         TO REC-DETAIL.
011160       PERFORM AY50 THRU AY59.
011170       ADD 1               TO WS-TRANS.
011180     IF WS-RECOVER > 95
011190         CLOSE RECOVER
011200         OPEN OUTPUT RECOVER
011210         CLOSE RECOVER
011220         OPEN I-O RECOVER
011230         MOVE ZERO         TO WS-RECOVER.
011240       GO TO AY59.
011250 AY50.
011260	     ADD 1		 TO WS-RECOVER.
011270       MOVE WS-RECOVER     TO WS-RECKEY.
011280       MOVE WS-TRANS       TO REC-TRAN.
011290       WRITE REC-RECORD.
011300     IF WS-STATUS NOT = "00"
011310         DISPLAY "Write error Recovery file - Status " AT 2212
011320			WITH FOREGROUND-COLOR 6 HIGHLIGHT
			WS-STATUS
011330			WITH FOREGROUND-COLOR 7 HIGHLIGHT
011340         STOP RUN.
011370 AY59.
011380       EXIT.
      *
      *    ****    Start processing transaction
      *
012470 AY60.
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
012500	     MOVE 4		 TO WS-PARKEY.
012510       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
012520     IF PAR-USERS < 24
012530         MOVE 1            TO WS-SUB
	       MOVE ZERO	 TO WS-WAIT
012540         GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4	S E C O N D S
      *
012550	     DISPLAY "WAITING" AT 2551
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
012560	     COMMIT.
	     ACCEPT WS-STIME FROM TIME.
	     MOVE 400		 TO WS-WAIT.
012580	     PERFORM LOCK-REC-LOOP.
012590	     DISPLAY SPACE AT 2551
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
012600       GO TO AY60.

012610 AY61.
012620	     MOVE "DB"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012010	     MOVE 1		 TO WS-NETKEY.
012680	     PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
012740	     GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N	P R O G R E S S
      *
012750 AY62.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES OR
		   PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R	O R   S T O C K   F I L E S
      * 	  B E I N G   U P D A T E D
      *
	       IF NOT (PAR-PROG(WS-SUB) = SPACES)
012760		   IF PAR-PROG(WS-SUB) = "DB" OR "DS"
      *
      *    ****   Y E S   -   S E T   W A I T	P E R I O D
      *
		       GO TO AY62-WAIT
		   ELSE
		       ADD 1	 TO WS-SUB
		       GO TO AY62
		   END-IF
	       ELSE
      *
      *    ****   I S	T H I S   P R O G R A M   I N	T H E  Q
      *
	       IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S	I T   N E X T	I N   L I N E	T O   P R O C E S S
      *
		   IF WS-WAIT = ZERO
		       GO TO AY63
		   ELSE
		       GO TO AY62-WAIT
		   END-IF
	       ELSE
		   GO TO AY62-WAIT
	       END-IF
	   END-IF.
	     MOVE LS-USER	 TO PAR-USR(WS-SUB).
	     MOVE WS-SUB	 TO PAR-USERS.
	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	     GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
	     MOVE 300		 TO WS-WAIT.
	   IF NOT (PAR-USR(WS-SUB) = LS-USER)
	       IF WS-SUB < 24
		   ADD 1	 TO WS-SUB
		   GO TO AY62.

       AY62-CHECK.
	   IF WS-WAIT > ZERO
	       COMMIT
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 14 BLINK
	       ACCEPT WS-STIME FROM TIME
	       PERFORM LOCK-REC-LOOP
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 14 BLINK
	       GO TO AY60.
012880	     DISPLAY SPACE AT 2572
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.

012900 AY63.
012920	     MOVE WS-SUB	 TO WS-USUB.
012940	     GO TO AY61.

012970 AY70.
013030	     MOVE 4		 TO WS-PARKEY.
013040       PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      * 	   NETWORK record 2.
      *
013010	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020	     PERFORM AY49 THRU AY59.
	     MOVE 1		 TO WS-USUB.

       AY72.
	   IF NOT (PAR-USR(WS-USUB) = LS-USER)
	       IF WS-USUB < 24
		   ADD 1	 TO WS-USUB
		   GO TO AY72
	       ELSE
		   GO TO AY85.


       AY75.
013050	     MOVE SPACES	 TO PAR-PROG(WS-USUB)
013060				    PAR-USR(WS-USUB).
	   IF WS-USUB < 24
	       ADD 1 WS-USUB	 GIVING WS-SUB
	   ELSE
	       GO TO AY80.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES)
	       MOVE PAR-PROG(WS-SUB)
				 TO PAR-PROG(WS-USUB)
	       MOVE PAR-USR(WS-SUB)
				 TO PAR-USR(WS-USUB)
	       ADD 1		 TO WS-USUB
	       GO TO AY75.

       AY80.
013070	     SUBTRACT 1 FROM WS-USUB
				 GIVING PAR-USERS.
013080	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.

       AY85.
013090	     COMMIT.

011860 AY999.
011870       EXIT.
      *
      *    ****    Read debtor record - using account number.
      *
013610 CA155-GET-DEBTOR  SECTION 2.
013620 CA155.
013630       MOVE W80-ACNO       TO DEB-ACNO.
013640       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
013650     IF WS-F-ERROR = 6
013660         MOVE 1            TO WS-ERROR
013670     ELSE
013680     IF DEB-ACNO NOT = W80-ACNO
013690         MOVE 2            TO WS-ERROR
013700     ELSE
013710         MOVE 0            TO WS-ERROR
013720         MOVE DEB-TYPE     TO W80-TYPE.
013730 CA160-EXIT.
013740       EXIT.
      *
013750 CA165-NAME        SECTION 2.
013760 CA165.
013770       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
013780                           INTO W40-SURNAME 
013790                                W40-FNAME 
013800                                W40-SNAME
013810       OVERFLOW
013820           MOVE 1          TO WS-ERROR.
013830 CA170-EXIT.
013840       EXIT.
      *
014530 CA175-ADDRESS	 SECTION 2.
014540 CA175.
014550       UNSTRING W60-ADDRESS DELIMITED BY "*"
014560				 INTO W40-ADD1
				      W40-ADD2
				      W40-ADD3
				      W40-ADD4
014570       OVERFLOW
014580           MOVE 1          TO WS-ERROR.
014590 CA175-EXIT.
014600       EXIT.
      *
014610 CA180-ADDRESS     SECTION 2.
014620 CA180.
014630       UNSTRING W60-ADDRESS1 DELIMITED BY "*"
014640                           INTO W40-PADD1 
                                      W40-PADD2 
                                      W40-PADD3 
                                      W40-PADD4
014650       OVERFLOW
014660           MOVE 1          TO WS-ERROR.
014670 CA180-EXIT.
014680       EXIT.
      /
      *  **  S A L E S	 R E S T R I C T I O N	 M A I N T E N A N C E
      *
021000 BA000		       SECTION 50.
021010 BA00.
021020       DISPLAY CLR-SCREEN.
020330	     DISPLAY "DEBTORS - SALES RESTRICTIONS" AT 0227
                      WITH FOREGROUND-COLOR 15.
020350       DISPLAY "Account :" AT 0302.
020370       MOVE SPACES         TO W80-ACNO.

020360 BA05.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Debtor Lookup "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
020380       ACCEPT S36 AT 0312.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W80-ACNO
		     GO TO BA999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA05
	       END-EVALUATE
016570	       DISPLAY S36 AT 0312
016580	       IF W80-ACNO = SPACES
016590		   GO TO BA05.
	     DISPLAY WS-BLNK78 AT 1202 WITH FOREGROUND-COLOR 3.
	     DISPLAY CLEAR-L25.
020390     IF W80-ACNO = SPACES
020400	       GO TO BA999.
             MOVE 6              TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
             MOVE W80-ACNO       TO DEB-ACNO.
020410       PERFORM CA155-GET-DEBTOR.
020420     IF WS-ERROR = 1
020430	       MOVE WS-ERR2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
020440	       GO TO BA05.
020450     IF WS-ERROR = 2
020460	       MOVE WS-ERR1	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
020470	       GO TO BA05.
	     DISPLAY WS-BLNK78 AT 0302 WITH FOREGROUND-COLOR 3.
020480       MOVE DEB-NAME       TO W60-NAME.
             MOVE DEB-CONTACT    TO W40-CONTACT.
020490       MOVE DEB-ADDRESS    TO W60-ADDRESS.
020500       MOVE DEB-PADDR      TO W60-ADDRESS1.
020510       MOVE DEB-POST       TO W40-POST.
020520       MOVE DEB-PPOST      TO W40-PPOST.
020530	     MOVE DEB-LANG	 TO W40-LANG.
020540	     MOVE SPACES	 TO W40-NAME1 W40-ADDRESS1 W40-PADDRESS.
020550       MOVE DEB-H-TEL      TO W40-HTEL.
020560       MOVE DEB-B-TEL      TO W40-TEL.
020560       MOVE DEB-FAX        TO W40-FAX.
020570       MOVE DEB-TITLE      TO W40-TITLE.
020580	     MOVE DEB-ID	 TO W40-ID.
019900	     MOVE DEB-TAX	 TO W40-TAX.
020590       MOVE DEB-CAT        TO W40-CAT.
020600       MOVE DEB-CRL        TO W40-CRL.
020600       MOVE DEB-OI         TO W40-OI.
	     MOVE DEB-ART11	 TO W40-EXEMPT.
020600       MOVE DEB-LAB        TO W40-LAB.
	   IF NOT VALID-PRICE
	       MOVE "R" 	 TO DEB-PRICE.
	     MOVE DEB-PRICE	 TO W40-PRICE.
020610     IF DEB-FIN = 1
020620         MOVE "N"          TO W40-FIN
020630     ELSE
020640         MOVE "Y"          TO W40-FIN.
020650       MOVE DEB-TERMS      TO W40-TERMS.
             MOVE DEB-INV-TYPE   TO W40-INV-TYPE.
020660       MOVE DEB-SMAN       TO W40-SMAN.
020670       MOVE DEB-DISC       TO W40-DISC.
020670       MOVE DEB-SDISC      TO W40-SDISC.
	     MOVE DEB-CONAC	 TO W40-CONAC.
020680       PERFORM CA175-ADDRESS.
020690       PERFORM CA180-ADDRESS.
	     DISPLAY WS-BLNK78 AT 0302 WITH FOREGROUND-COLOR 3.
020710     IF W80-TYPE NOT = 1
020720	       GO TO BA15.
020730       PERFORM CA165-NAME.
020760       DISPLAY S22.
             DISPLAY S36 AT 0415.

020770 BA10.
020780       DISPLAY S25.
	     PERFORM CHECK-CORRECT.
020860	   IF WS-OPTION = "N"
020870	       GO TO BA00.
	     GO TO BA25.

021360 BA15.
021370	   IF W80-TYPE = 3
021380         DISPLAY S06
021390     ELSE
021400         DISPLAY S05.
021410       DISPLAY S23.
             DISPLAY S36 AT 0415.
021420       MOVE W60-NAME       TO W40-NAME.

021450 BA20.
021460       DISPLAY S24.
	     PERFORM CHECK-CORRECT.
021540	   IF WS-OPTION = "N"
021550	       GO TO BA00.
	   IF NOT(DEB-RESTRICT = "A" OR "E" OR "N")
	       MOVE "N" 	 TO DEB-RESTRICT.
	     MOVE DEB-RESTRICT	 TO WS-RESTCODE.
	   IF NO-SALES-RESTRICTIONS
	       MOVE "N" 	 TO WS-RESTRICT
	   ELSE
	       MOVE "Y" 	 TO WS-RESTRICT.
      *    *************************************************************
      *    **** 	 C L E A R   W O R K   A R E A S	    ****
      *    *************************************************************
	     MOVE 1		 TO WS-S.

       BA25.
	     MOVE SPACES	 TO W75-DEPT-CDE(WS-S)
				    W75-DEPT-DSC(WS-S).
	   IF WS-S < 10
	       ADD 1		 TO WS-S
	       GO TO BA25.
      *
      *    ****   R E S T R I C T I O N   W I N D O W
      *
       BA30.
      * 	   ÕÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¸
      * 	   ³	    SALES RESTRICTIONS	       ³°°
      * 	   ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ°°
      * 	   ³Allowed/Excluded/Neither- A/E/N [ ]³°°
      * 	   ÃÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´°°
      * 	   ³Code³      Description	       ³°°
      * 	   ÃÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´°°
      * 	   ³XXXX³XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX³°°
      * 	   ³XXXX³XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX³°°
      * 	   ³XXXX³XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX³°°
      * 	   ³XXXX³XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX³°°
      * 	   ³XXXX³XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX³°°
      * 	   ³XXXX³XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX³°°
      * 	   ³XXXX³XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX³°°
      * 	   ³XXXX³XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX³°°
      * 	   ³XXXX³XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX³°°
      * 	   ³XXXX³XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX³°°
      * 	   ÀÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°
      * 	     °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
      *
	     PERFORM SAVE-SCREEN-3.
	     MOVE 06		 TO SHADE-ROW.
	     MOVE 42		 TO SHADE-COL.
	     MOVE 35		 TO SHADE-WIDTH.
	     MOVE 17		 TO SHADE-LINES.
	     DISPLAY "ÕÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ" AT 0540
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "¸" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "³" AT 0640
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "        SALES RESTRICTIONS         ³"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 7.
	     DISPLAY "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ" AT 0740
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "µ" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "³Allowed/Excluded/Neither- " AT 0840
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "A" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7
		     "/" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "E" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7
		     "/" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "N" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7
		     " [ ]" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "ÃÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0940
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "´" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "³Code³	    Description           " AT 1040
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     DISPLAY "ÃÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1140
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "´" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
018620	     MOVE 1240		 TO CPOS.
018630	     PERFORM BA100 UNTIL CLIN > 21.
	     DISPLAY "À" AT 2240
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "ÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.
	     MOVE DEB-ACNO	 TO DBD-ACNO.
	     PERFORM READ-DEBDEP THRU READ-DEBDEP-EXIT.
	   IF WS-F-ERROR = 33
	       IF WS-RESTRICT = "N"
		   GO TO BA45
	       END-IF
	       MOVE "Restriction File Corrupted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA999.
	   IF WS-RESTRICT = "N"
	       MOVE "Y" 	 TO WS-RESTRICT.
	     MOVE 1		 TO WS-S.

       BA35.
	   IF DBD-DEPT-CDE(WS-S) = SPACES
	       GO TO BA40.
	     MOVE DBD-DEPT-CDE(WS-S)
				 TO W75-DEPT-CDE(WS-S)
				    DPT-CODE.
022380	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE SPACES	 TO WS-ERR-STRING
018910	       STRING W75-DEPT-CDE(WS-S) DELIMITED SIZE
		      " " DELIMITED SIZE
		      "Invalid or Deleted" DELIMITED SIZE
		       INTO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
	       MOVE WS-S	 TO WS-S9
	       ADD 1 WS-S	 GIVING WS-S8
	       PERFORM BA110
	       GO TO BA40.
	     MOVE DPT-DESC	 TO W75-DEPT-DSC(WS-S).

       BA40.
	   IF WS-S < 10
	       ADD 1		 TO WS-S
	       GO TO BA35.

       BA45.
	     DISPLAY S40.
011010	     DISPLAY "F2" AT 2541
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
011020		     "=Sales Ledger Lookup,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		      "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 3
		      " key to end" WITH FOREGROUND-COLOR 1
					 BACKGROUND-COLOR 3.

       BA50.
	     ACCEPT S40-REST.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA00
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA50.
	     CALL "CBL_TOUPPER" USING WS-RESTCODE
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS
	   IF NOT(WS-RESTCODE = "A" OR "E" OR "N")
	       GO TO BA50.
	   IF WS-RESTCODE = "N"
	       IF WS-RESTRICT = "Y"
		   MOVE "N"	 TO WS-OPTION
		   MOVE "Delete Restrictions (Y/N) [ ]"
				 TO WS-ERR-MES
		   PERFORM OPT-MESSAGE
		   IF WS-OPTION = "Y"
		       GO TO BA90
		   ELSE
		       GO TO BA50.
       BA51.
	     ACCEPT S40-1A.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO BA50
		 WHEN DOWN-KEY
001340		     GO TO BA52
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA51
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA65
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM DEPART-VIEW
011320		     PERFORM RESTORE-SCREEN
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA51.
	     MOVE 4		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W75-DCDE-1
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	   IF W75-DCDE-1 = SPACES
	       MOVE 1		 TO WS-S9
	       MOVE 2		 TO WS-S8
	       PERFORM BA110
	       DISPLAY S40
	       GO TO BA51.
	     MOVE W75-DCDE-1	 TO DPT-CODE.
	     PERFORM BA120.
	   IF WS-F-ERROR = 7
	       GO TO BA51.
	     MOVE DPT-DESC	 TO W75-DDSC-1.
	     DISPLAY S40-1D.

       BA52.
	     ACCEPT S40-2A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA65
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM DEPART-VIEW
011320		     PERFORM RESTORE-SCREEN
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA52
	   ELSE
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO BA51
		 WHEN DOWN-KEY
001340		     GO TO BA53
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA52.
	     MOVE 4		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W75-DCDE-2
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	   IF W75-DCDE-2 = SPACES
	       MOVE 2		 TO WS-S9
	       MOVE 3		 TO WS-S8
	       PERFORM BA110
	       DISPLAY S40
	       GO TO BA52.
	     MOVE W75-DCDE-2	 TO DPT-CODE.
	     PERFORM BA120.
	   IF WS-F-ERROR = 7
	       GO TO BA52.
	     MOVE DPT-DESC	 TO W75-DDSC-2.
	     DISPLAY S40-2D.

       BA53.
	     ACCEPT S40-3A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA65
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM DEPART-VIEW
011320		     PERFORM RESTORE-SCREEN
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA53
	   ELSE
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO BA52
		 WHEN DOWN-KEY
001340		     GO TO BA54
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA53.
	     MOVE 4		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W75-DCDE-3
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	   IF W75-DCDE-3 = SPACES
	       MOVE 3		 TO WS-S9
	       MOVE 4		 TO WS-S8
	       PERFORM BA110
	       DISPLAY S40
	       GO TO BA53.
	     MOVE W75-DCDE-3	 TO DPT-CODE.
	     PERFORM BA120.
	   IF WS-F-ERROR = 7
	       GO TO BA53.
	     MOVE DPT-DESC	 TO W75-DDSC-3.
	     DISPLAY S40-3D.

       BA54.
	     ACCEPT S40-4A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA65
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM DEPART-VIEW
011320		     PERFORM RESTORE-SCREEN
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA54
	   ELSE
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO BA53
		 WHEN DOWN-KEY
001340		     GO TO BA55
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA54.
	     MOVE 4		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W75-DCDE-4
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	   IF W75-DCDE-4 = SPACES
	       MOVE 4		 TO WS-S9
	       MOVE 5		 TO WS-S8
	       PERFORM BA110
	       DISPLAY S40
	       GO TO BA54.
	     MOVE W75-DCDE-4	 TO DPT-CODE.
	     PERFORM BA120.
	   IF WS-F-ERROR = 7
	       GO TO BA54.
	     MOVE DPT-DESC	 TO W75-DDSC-4.
	     DISPLAY S40-4D.

       BA55.
	     ACCEPT S40-5A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA65
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM DEPART-VIEW
011320		     PERFORM RESTORE-SCREEN
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA55
	   ELSE
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO BA54
		 WHEN DOWN-KEY
001340		     GO TO BA56
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA55.
	     MOVE 4		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W75-DCDE-5
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	   IF W75-DCDE-5 = SPACES
	       MOVE 5		 TO WS-S9
	       MOVE 6		 TO WS-S8
	       PERFORM BA110
	       DISPLAY S40
	       GO TO BA55.
	     MOVE W75-DCDE-5	 TO DPT-CODE.
	     PERFORM BA120.
	   IF WS-F-ERROR = 7
	       GO TO BA55.
	     MOVE DPT-DESC	 TO W75-DDSC-5.
	     DISPLAY S40-5D.

       BA56.
	     ACCEPT S40-6A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA65
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM DEPART-VIEW
011320		     PERFORM RESTORE-SCREEN
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA56
	   ELSE
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO BA55
		 WHEN DOWN-KEY
001340		     GO TO BA57
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA56.
	     MOVE 4		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W75-DCDE-6
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	   IF W75-DCDE-6 = SPACES
	       MOVE 6		 TO WS-S9
	       MOVE 7		 TO WS-S8
	       PERFORM BA110
	       DISPLAY S40
	       GO TO BA56.
	     MOVE W75-DCDE-6	 TO DPT-CODE.
	     PERFORM BA120.
	   IF WS-F-ERROR = 7
	       GO TO BA56.
	     MOVE DPT-DESC	 TO W75-DDSC-6.
	     DISPLAY S40-6D.

       BA57.
	     ACCEPT S40-7A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA65
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM DEPART-VIEW
011320		     PERFORM RESTORE-SCREEN
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA57
	   ELSE
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO BA56
		 WHEN DOWN-KEY
001340		     GO TO BA58
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA57.
	     MOVE 4		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W75-DCDE-7
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	   IF W75-DCDE-7 = SPACES
	       MOVE 7		 TO WS-S9
	       MOVE 8		 TO WS-S8
	       PERFORM BA110
	       DISPLAY S40
	       GO TO BA57.
	     MOVE W75-DCDE-7	 TO DPT-CODE.
	     PERFORM BA120.
	   IF WS-F-ERROR = 7
	       GO TO BA57.
	     MOVE DPT-DESC	 TO W75-DDSC-7.
	     DISPLAY S40-7D.

       BA58.
	     ACCEPT S40-8A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA65
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM DEPART-VIEW
011320		     PERFORM RESTORE-SCREEN
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA58
	   ELSE
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO BA57
		 WHEN DOWN-KEY
001340		     GO TO BA59
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA58.
	     MOVE 4		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W75-DCDE-8
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	   IF W75-DCDE-8 = SPACES
	       MOVE 8		 TO WS-S9
	       MOVE 9		 TO WS-S8
	       PERFORM BA110
	       DISPLAY S40
	       GO TO BA58.
	     MOVE W75-DCDE-8	 TO DPT-CODE.
	     PERFORM BA120.
	   IF WS-F-ERROR = 7
	       GO TO BA58.
	     MOVE DPT-DESC	 TO W75-DDSC-8.
	     DISPLAY S40-8D.

       BA59.
	     ACCEPT S40-9A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA65
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM DEPART-VIEW
011320		     PERFORM RESTORE-SCREEN
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA59
	   ELSE
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO BA58
		 WHEN DOWN-KEY
001340		     GO TO BA60
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA59.
	     MOVE 4		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W75-DCDE-9
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	   IF W75-DCDE-9 = SPACES
	       MOVE 9		 TO WS-S9
	       MOVE 10		 TO WS-S8
	       PERFORM BA110
	       DISPLAY S40
	       GO TO BA59.
	     MOVE W75-DCDE-9	 TO DPT-CODE.
	     PERFORM BA120.
	   IF WS-F-ERROR = 7
	       GO TO BA59.
	     MOVE DPT-DESC	 TO W75-DDSC-9.
	     DISPLAY S40-9D.

       BA60.
	     ACCEPT S40-10A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA65
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM DEPART-VIEW
011320		     PERFORM RESTORE-SCREEN
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA60
	   ELSE
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO BA59
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA59.
	     MOVE 4		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W75-DCDE-10
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	   IF W75-DCDE-10 = SPACES
	       MOVE SPACES	 TO W75-DDSC-10
	       DISPLAY S40
	       GO TO BA60.
	     MOVE W75-DCDE-10	 TO DPT-CODE.
	     PERFORM BA120.
	   IF WS-F-ERROR = 7
	       GO TO BA60.
	     MOVE DPT-DESC	 TO W75-DDSC-10.
	     DISPLAY S40-10D.

       BA65.
	     PERFORM CHECK-CORRECT.
020860	   IF WS-OPTION = "N"
020870	       GO TO BA50.
	     MOVE 10		 TO WS-S.

       BA70.
	   IF W75-DEPT-CDE(WS-S) = SPACES
	       IF WS-S > 1
		   SUBTRACT 1	 FROM WS-S
		   GO TO BA70.
      *
      *    ****   C H E C K   I F   A N Y   D A T A
      *
	   IF W75-DEPT-CDE(WS-S) = SPACES
	       IF WS-RESTRICT = "Y"
		   MOVE "N"	 TO WS-OPTION
		   MOVE "Delete Restrictions (Y/N) [ ]"
				 TO WS-ERR-MES
		   PERFORM OPT-MESSAGE
		   IF WS-OPTION = "Y"
		       GO TO BA90
		   ELSE
		       GO TO BA51.
	     MOVE 1		 TO WS-S WS-S9.
	     PERFORM AY60 THRU AY999.
	   IF NOT(DEB-RESTRICT = WS-RESTCODE)
	       MOVE ZERO	 TO WS-ACTION
	       PERFORM AY02 THRU AY59.
	     MOVE WS-RESTCODE	 TO DEB-RESTRICT.
	   IF WS-RESTRICT = "Y"
	       MOVE ZERO	 TO WS-ACTION
	       PERFORM AY33 THRU AY59.
	     MOVE DEB-ACNO	 TO DBD-ACNO.
	     MOVE SPACES	 TO DBD-DEPT-CODES.

       BA75.
	   IF W75-DEPT-CDE(WS-S) = SPACES
	       IF WS-S < 10
		   ADD 1	 TO WS-S
		   GO TO BA75.
	     MOVE W75-DEPT-CDE(WS-S)
				 TO DBD-DEPT-CDE(WS-S9).
	   IF WS-S < 10
	       ADD 1		 TO WS-S WS-S9
	       GO TO BA75.
	   IF WS-RESTRICT = "N"
	       PERFORM WRITE-DEBDEP THRU WRITE-DEBDEP-EXIT
	       MOVE 1		 TO WS-ACTION
	       PERFORM AY33 THRU AY59
	   ELSE
	       PERFORM REWRITE-DEBDEP THRU WRITE-DEBDEP-EXIT.
	     GO TO BA140.

       BA90.
	     PERFORM RESTORE-SCREEN-3.
	     PERFORM AY60 THRU AY999.
	     MOVE 2		 TO WS-ACTION.
	     PERFORM AY33 THRU AY59.
	     PERFORM DELETE-DEBDEP-REC THRU WRITE-DEBDEP-EXIT.
	     MOVE ZERO		 TO WS-ACTION.
	     PERFORM AY02 THRU AY59.
	     MOVE "N"		 TO DEB-RESTRICT.
	     GO TO BA140.

       BA100.
	     DISPLAY "³    ³                              " AT CPOS
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
	     ADD 1		 TO CLIN.

       BA110.
	   IF WS-S9 < 10
	       MOVE DBD-DEPT-CDE(WS-S8)
				 TO DBD-DEPT-CDE(WS-S9).
	   IF WS-S8 < 10
	       ADD 1		 TO WS-S8 WS-S9
	       GO TO BA110.
	     MOVE SPACES	 TO DBD-DEPT-CDE(WS-S8).

       BA120.
022380	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE "Invalid Sales Ledger/Group Code"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE.

       BA140.
035040	     PERFORM REWRITE-DEBTOR-UNLOCK THRU WRITE-DEBTOR-EXIT.
020150	     PERFORM AY70 THRU AY999.
	     GO TO BA00.

021720 BA999.
021730       EXIT.
      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *   
041980 ZA000-INIT	       SECTION 80.
041990 ZA000-OPEN.
	     PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
042000	     DISPLAY CLR-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   S e t   u p	 t h e	 F U N C T I O N   k e y s     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		       ESC, F1 to F10 keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    PAGE-UP and PAGE-DOWN keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   A C T I V A T E   M O U S E		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 64		 TO MOUSE-FUNC.
	     MOVE 1		 TO MOUSE-PARAM.
	     CALL X"AF" USING MOUSE-FUNC
			      MOUSE-PARAM.
	   IF MOUSE-FUNC NOT = 255
	       MOVE "Y"		 TO MOUSE-ATTACHED
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM
	   ELSE
	       GO TO ZA00A.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       Set MOUSE key to act as FUNCTION key	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    Setup the file ID's including path names           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
      *    *************************************************************
      *    **** D E B T O R   S A L E S   R E S T R I T I O N	F I L E 	  ****
      *    *************************************************************
	     MOVE "DEBDEP"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBDEP.
      *    *************************************************************
      *    **** 	  D E B T O R	M E M O	  F I L E	    ****
      *    *************************************************************
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
      *    *************************************************************
      *    **** 	       D E B T O R   F I L E		    ****
      *    *************************************************************
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
      *    *************************************************************
      *    **** 	   D E P A R T M E N T	 F I L E	    ****
      *    *************************************************************
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    **** 	    R E C O V E R Y   F I L E		    ****
      *    *************************************************************
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
      *    *************************************************************
      *    ****      N E T W O R K   (S H A R E D)   F I L E	    ****
      *    *************************************************************
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.

042170 ZA02.
	     OPEN I-O DEBDEP.
042220	   IF RUNTIME-ERROR
               IF FLE-LOCKED
042230             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 33	 TO WS-F-ERROR
                   GO TO ZA50.
042240     IF WS-STATUS NOT = "00"
042250	       MOVE 33		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT (WS-STATUS = "41")
050210		   PERFORM OPEN-ERROR.
042490	     OPEN OUTPUT RECOVER.
042500       CLOSE RECOVER.
042510       OPEN I-O RECOVER.
044780	     GO TO ZA999-EXIT.
      *    *************************************************************
      *    ****   F I L E   L I M I T	E X C E E D E D
      * 	  (Config.sys Dos/Win/Windows 95/98/NT/2000)
      * 	  (Config.nt Windows XP)
      * 	  (System.ini Windows.me)
      *    *************************************************************
039780 ZA49.
039790	     DISPLAY "Too many files OPEN" AT 0708
039800                WITH FOREGROUND-COLOR 14.
039810	     DISPLAY "Check the FILES option in CONFIG.SYS (Dos,Windows
      -		     "3/95/98/NT/2000)" AT 0908.
	     DISPLAY "CONFIG.NT  (Windows XP)" AT 1034.
	     DISPLAY "SYSTEM.INI (Windows ME)" AT 1134.
039820	     DISPLAY "Press any key to continue" AT 1308.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
055330	     GO TO ZA205.

039850 ZA50.
041850	   IF WS-F-ERROR = 18
041860	       MOVE "DEBTOR RESTRICTIONS"
				 TO WS-FILE.
039980       DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.

055970 ZA200.
055980       DISPLAY "Files locked - Try later" AT 2312
055990                WITH FOREGROUND-COLOR 14
056000               " " WS-STATUS WITH FOREGROUND-COLOR 15.
056010       DISPLAY "Press any key" AT 2512 
056020		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
056030	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO.
056040 ZA205.
056050       EXIT PROGRAM.

       ZA55.
041220       MOVE 1              TO WS-S1.

041240 ZA60.
041250       MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (8)  WS-TCHR (49)
021940                              WS-TCHR (61) WS-TCHR (72).
021970       MOVE WS-G4          TO WS-BCHR (8)  WS-BCHR (49)
021980                              WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
	     MOVE "SupervisorÄ"  TO WS-WRKHD.

042880 ZA999-EXIT.
042890       EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *   
042900 ZB000-ERROR	     SECTION 81.

       COPY ERRORS.PRO.

041560 DISPLAY-FILE-NAME.
041610	   IF WS-F-ERROR = 2
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
056380	   ELSE
056390     IF WS-F-ERROR = 6
056400	       MOVE W02-DEBTOR	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
056420	   ELSE
056430     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041640	   ELSE
041850	   IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041760	   ELSE
041770	   IF WS-F-ERROR = 18
041170	       MOVE W02-RECOVER  TO WS-FILE
041180         MOVE WS-RECKEY    TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX
041760	   ELSE
041770	   IF WS-F-ERROR = 33
041780	       MOVE W02-DEBDEP	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
	       MOVE DBD-ACNO	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY.

       COPY DISPERR.PRO.
