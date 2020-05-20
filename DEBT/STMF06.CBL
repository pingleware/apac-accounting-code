      $SET LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   ********  **     **  *******    ****     ******    *
      *   **	**     **     ***   ***  **	   **  **   **	  **   *
      *   **	       **     ** * * **  **	  **	**  **	       *
      *    ******      **     **  *  **  *****	  **	**  *******    *
      * 	**     **     **     **  **	  **	**  **	  **   *
      *   **	**     **     **     **  **	   **  **   **	  **   *
      *    ******      **     **     **  **	    ****     ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      * 	A C C O U N T S	  R E C E I V A B L E	/	       *
      *                                                                *
      *     D E B T O R	  S T A T E M E N T S	P R O G R A M	       *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
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
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STMF06.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   NOVEMBER 1992.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1992 - 2011
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

000200	   SELECT STMREP  ASSIGN W02-STATEMENT
                          ORGANIZATION LINE SEQUENTIAL.

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
000330 FD  STMREP    LABEL RECORD OMITTED
000420		     LINAGE IS WS-PGE-LENGTH.
002140 01  STM-LINE1.
002150     03  STM-DET         PIC X(96).
002140 01  ST3-LINE1.
002150     03  ST3-DET         PIC X(96).
002160 01  ST3-LINE2.
002170     03  ST3-COMP.
002180         05  ST3-EXCOMP  PIC X(36).
002190         05  FILLER      PIC X(36).
002200     03  FILLER          PIC X(24).
002160 01  ST6-LINE2A.
002170	   03  FILLER	       PIC  X(16).
002180	   03  ST6-HEADER      PIC  X(15).
002200	   03  FILLER	       PIC  X(65).
002230 01  ST3-LINE3.
002240     03  ST3-ADD         PIC  X(30).
002250     03  FILLER          PIC  X(02).
002750     03  ST3-TEL         PIC  X(19).
002760     03  ST3-TNO         PIC  X(14).
002220     03  FILLER          PIC  X(31).
002290 01  ST3-LINE4.
002320     03  ST3-MESS.
002350	       05  ST3-PC1     PIC  X(08).
002350	       05  ST3-PC2     PIC  X(08).
002330	       05  FILLER      PIC  X(36).
	   03  FILLER	       PIC  X(44).
002290 01  ST3-LINE5.
           03  FILLER          PIC  X(08).
002300     03  ST3-NAME        PIC  X(40).
002310	   03  FILLER	       PIC  X(30).
	   03  ST3-AH1	       PIC  X(08).
	   03  ST3-AC1.
	       05  ST3-DATE    PIC  Z9/99/9999.
	   03  ST6-PG REDEFINES ST3-AC1.
	       05  ST6-PAGE    PIC  ZZZ9.
	       05  FILLER      PIC  X(04).
002340 01  ST3-LINE6.
	   02  FILLER	       PIC  X(04).
	   02  ST6-HD1.
	   03  FILLER	       PIC  X(01).
002420	   03  ST3-TDTE        PIC  Z9/99/9999.
002430	   03  FILLER	       PIC  X(02).
           03  ST3-BFWD.
002440	       05  ST3-TDESC   PIC  X(12).
	       05  FILLER      PIC  X(04).
002470         05  ST3-TREF    PIC  X(08).
002490	   03  FILLER	       PIC  X(04).
002500     03  ST3-V1.
002510         05  ST3-TDBT    PIC  Z(08)9.99.
002520	   03  ST3-PD	       PIC  X(04).
002500	   03  ST3-V2.
002510         05  ST3-TCRD    PIC  Z(08)9.99.
	   03  ST3-CB	       PIC  X(01).
002520	   03  FILLER	       PIC  X(03).
002530     03  ST3-V3.
002540         05  ST3-TBAL    PIC  Z(08)9.99-.
002550     03  FILLER          PIC  X(02).
002690 01  ST3-LINE7.
	   03  FILLER	       PIC  X(03).
002700     03  ST3-A1          PIC  X(12).
002710     03  ST3-A2          PIC  X(10).
002720     03  ST3-A3          PIC  X(12).
002720     03  ST3-A4          PIC  X(12).
002720     03  ST3-A5          PIC  X(12).
002730     03  FILLER          PIC  X(35).
002600 01  ST3-LINE8.
002610     03  FILLER          PIC  X(01).
           03  ST3-INT         PIC  Z(06)9.99-.
002610     03  FILLER          PIC  X(01).
002620     03  ST3-3MTHS       PIC  Z(06)9.99-.
002610     03  FILLER          PIC  X(01).
002620     03  ST3-2MTHS       PIC  Z(06)9.99-.
002630     03  FILLER          PIC  X(01).
002640     03  ST3-1MTH        PIC  Z(06)9.99-.
002650     03  FILLER          PIC  X(01).
002660     03  ST3-CUR         PIC  Z(06)9.99-.
002670     03  FILLER          PIC  X(01).
002680     03  ST3-DUE         PIC  Z(06)9.99-.
002670     03  FILLER          PIC  X(12).
002680     03  ST3-DUE2        PIC  Z(06)9.99-.
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
002970 77  WS-LINES	   PIC	9(02).
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
       77  WS-DET-LINES    PIC	9(02)	 VALUE 34.
003650 77  WS-COND	   PIC	X(06).
003660 77  WS-NORM	   PIC	X(06).
003070 77  WS-EXPP	   PIC	X(06).
003080 77  WS-ECAN	   PIC	X(06).
003090 77  WS-8LPI	   PIC	X(06).
003100 77  WS-6LPI	   PIC	X(06).
       77  WS-10CPI	   PIC	X(06).
       77  WS-12CPI	   PIC	X(06).
       77  WS-17CPI	   PIC	X(06).
003110 77  WS-EXPAND	   PIC	X(01)	 VALUE " ".
003180 77  WS-COMP	   PIC	9(01).
003180 77  WS-STM	   PIC	9(01).
003190 77  WS-TYPE	   PIC	X(01).
003200 77  WS-HEAD	   PIC	X(01).
003240 77  WS-MESS	   PIC	X(02).
003250 77  WS-ACNO	   PIC	X(06).
003250 77  WS-ASTART	   PIC	X(06).
003250 77  WS-AEND	   PIC	X(06).
003260 77  WS-S1	   PIC	9(04)	  COMP-5.
003270 77  WS-S2	   PIC	9(04)	  COMP-5.
003280 77  WS-S3	   PIC	9(04)	  COMP-5.
003290 77  WS-S4	   PIC	9(04)	  COMP-5.
003300 77  WS-S5	   PIC	9(04)	  COMP-5.
003310 77  WS-S6	   PIC	9(04)	  COMP-5.
003320 77  WS-S7	   PIC	9(04)	  COMP-5.
003330 77  WS-S8	   PIC	9(04)	  COMP-5.
003340 77  WS-CHK	   PIC	X(33)	  VALUE
003350			   "ENTER if correct - N if incorrect".
003360 77  WS-ERR1	   PIC	X(22)	  VALUE
003370                     "Invalid account number".
003380 77  WS-ERR2	   PIC	X(09) VALUE "No record".
003420 77  WS-AC	   PIC	X(07) VALUE "Account".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
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
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
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

       01  W02-FILE-IDS.
	   03  W02-STATEMENT.
	       05  W02-STMNT  PIC X(07) VALUE "STMF06.".
               05  W02-USE    PIC X(05) VALUE SPACES.

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

003710 01  W10-SPARES.
004300	   03  W10-DATE	       PIC  X(10).
	   03  W10-RDATE REDEFINES W10-DATE.
	       05  W10-DTE     PIC  99/99/9999.
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
004020             07  W30-SMESS PIC X(26) OCCURS 6.
	   03  W30-MESS2 REDEFINES W30-MESS6.
	       05  W30-MESS-A	 OCCURS 2.
		   07  W30-SMES  PIC X(52) OCCURS 3.
004030	   03  W30-AMESSAGES.
004040         05  FILLER      PIC X(26) VALUE
004050                         "Ons het nog nie u betaling".
004060         05  FILLER      PIC X(26) VALUE
004070                         "vir VERLEDE MAAND ontvang".
004080         05  FILLER      PIC X(26) VALUE
004090                         "nie. Indien daar 'n rede".
004100         05  FILLER      PIC X(26) VALUE
004110                         "is, word u vriendelik ver-".
004120         05  FILLER      PIC X(26) VALUE
004130                         "soek om ons in kennis te".
004140         05  FILLER      PIC X(26) VALUE
004150                         "stel.".
004160         05  FILLER      PIC X(26) VALUE
004170                         "U rekening is nou TWEE".
004180         05  FILLER      PIC X(26) VALUE
004190                         "MAANDE agterstallig en u".
004200         05  FILLER      PIC X(26) VALUE
004210                         "word versoek om dringend".
004220         05  FILLER      PIC X(26) VALUE
004230                         "aandag daaraan te skenk".
004240         05  FILLER      PIC X(52) VALUE SPACES.
004250         05  FILLER      PIC X(26) VALUE
004260                         "U rekening is nou MEER AS".
004270         05  FILLER      PIC X(26) VALUE
004280                         "TWEE MAANDE agterstallig!".
004290         05  FILLER      PIC X(26) VALUE
004300                         "Tensy u rekening nie op".
004310         05  FILLER      PIC X(26) VALUE
004320                         "datum gebring word nie,".
004330         05  FILLER      PIC X(26) VALUE
004340                         "sal GEREGTILIKE STAPPE".
004350         05  FILLER      PIC X(26) VALUE
004360                         "teen u geneem word.".
004370         05  FILLER      PIC X(26) VALUE
004380                         "We do not appear to have".
004390         05  FILLER      PIC X(26) VALUE
004400                         "received your payment for".
004410         05  FILLER      PIC X(26) VALUE
004420                         "LAST MONTH. You are kindly".
004430         05  FILLER      PIC X(26) VALUE
004440                         "requested to contact us if".
004450         05  FILLER      PIC X(26) VALUE
004460                         "there is a problem with".
004470         05  FILLER      PIC X(26) VALUE
004480                         "your account.".
004490         05  FILLER      PIC X(26) VALUE
004500                         "Your account is now TWO".
004510         05  FILLER      PIC X(26) VALUE
004520                         "MONTHS in arrear and we".
004530         05  FILLER      PIC X(26) VALUE
004540                         "request that you give this".
004550         05  FILLER      PIC X(26) VALUE
004560                         "matter your urgent atten-".
004570         05  FILLER      PIC X(52) VALUE
004580                         "tion.".
004590         05  FILLER      PIC X(26) VALUE
004600                         "Your account is now MORE".
004610         05  FILLER      PIC X(26) VALUE
004620                         "THAN TWO MONTHS in arrear!".
004630         05  FILLER      PIC X(26) VALUE
004640                         "Unless your account is".
004650         05  FILLER      PIC X(26) VALUE
004660                         "brought up-to-date, LEGAL".
004670         05  FILLER      PIC X(52) VALUE
004680                         "ACTION will be instituted.".
004690     03  W30-AMESS       REDEFINES W30-AMESSAGES.
004700         05  W30-ARRM    OCCURS 2.
004710             07  W30-30D PIC X(156).
004720             07  W30-60D PIC X(156).
004730             07  W30-90D PIC X(156).
004740	   03  W30-STM.
             04  W30-STM6.
004750         05  W30-STML    PIC X(26)  OCCURS 6.
             04  W30-STM2 REDEFINES W30-STM6.
	       05  W30-STL     PIC X(52)  OCCURS 3.

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
004950	   02  W40-POST        PIC X(08).
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
	     "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  5 COLUMN 13 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	     "³       Select the Range of accounts to print         ³".
	   03  LINE  6 COLUMN 13 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	     "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE  7 COLUMN 13 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	     "³ Start A/C:                                          ³".
	   03  LINE  8 COLUMN 13 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	     "³                                                     ³".
	   03  LINE  9 COLUMN 13 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	     "³ End A/C:                                            ³".
	   03  LINE 10 COLUMN 13 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	     "³                                                     ³".
	   03  LINE 11 COLUMN 13 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	     "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE 12 COLUMN 13 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	     "³    Statements will be printed in name sequence.     ³".
	   03  LINE 13 COLUMN 13 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
	     "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

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
      *	   IF W05-PRINT-CS = "Y"
      *	       PERFORM XC000.
	   IF PREVIOUS-STATEMENT
	       CLOSE DBHIST
		     DTHIST.

       AA49.
006950	     EXIT PROGRAM.

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
010990 AA300              SECTION.
011000 AA301.
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
011090 AA305.
011100       EXIT.

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      * 	   S C R E E N ,   K E Y B O A R D   &	 M O U S E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³			  SCREEN-SHADOW 		       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To routine is used to display a shadow down the right and ³
      *    ³ along the bottom of a pop-up box. The parameters that are ³
      *    ³ required:						       ³
      *	   ³	       SHADE-ROW   - Top line of the box + 1.	       ³
      *	   ³	       SHADE-COL   - Left line of box + 2.	       ³
      *	   ³	       SHADE-WIDTH - Width of the box - 2.	       ³
      *	   ³	       SHADE-LINES - Hight of box - 1.		       ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³			  CHECK-CORRECT 		       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine displays a pop-up window with the message    ³
      *    ³	      "Correct ENTER - N if incorrect"		       ³
      *    ³							       ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³							       ³
      *    ³ To start with the pop-up window higher or lower than row  ³
      *    ³ 13 (default); move another value to SLIN and PERFORM      ³
      *    ³ CHECK-SETUP THRU CHECK-EXIT.			       ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³			  ERROR-MESSAGE 		       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display an error message with " - Press ANY key" at    ³
      *    ³ the end; use PERFORM ERROR-MESSAGE.		       ³
      *    ³							       ³
      *    ³ To display just the error message; use PERFORM	       ³
      *    ³ ERROR-LENGTH THRU ERROR-EXIT.			       ³
      *    ³							       ³
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   ³
      *    ³ or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     ³
      *    ³ number to be used to SLIN and then PERFORM ERROR-DISPLAY  ³
      *    ³ THRU ERROR-EXIT.					       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

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

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

       COPY ABORT.PRN.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       R E A D	 F I L E S   R O U T I N E S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   &	W R I T E   F I L E S	R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
	       MOVE 1		 TO WS-ERROR
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
009140       MOVE ZERO TO WS-ERROR.
009150       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
009160         INTO W40-SURNAME W40-FNAME W40-SNAME
009170         OVERFLOW
009180           MOVE 1 TO WS-ERROR.
009190 CA170-EXIT.
009200       EXIT.
009210 CA175-ADDRESS           SECTION 2.
009220 CA175-EXTRACT.
009230       MOVE 0 TO WS-ERROR.
009240       UNSTRING W60-ADDRESS DELIMITED BY "*"
009250         INTO W40-ADD1 W40-ADD2 W40-ADD3 W40-ADD4
009260         OVERFLOW
009270           MOVE 1 TO WS-ERROR.
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
      *   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *   ³ This routine will generate consolodating transactions ³°°
      *   ³ and as there is no recovery procedure, backups of the ³°°
      *   ³ data files should be made before proceeding.	  ³°°
      *   ³		Have backups been made Y/N		  ³°°
      *   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°
      *     °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
	     MOVE 10		 TO SHADE-ROW.
	     MOVE 14		 TO SHADE-COL.
	     MOVE 55		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -		     "ÄÄÄÄÄÄ" AT 0912
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     "¿" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "³ " AT 1012
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     "This routine will generate consolodating transacti
      -		     "ons " WITH FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
		     "³" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "³ " AT 1112
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     "and as there is no recovery procedure, backups of
      -		     "the " WITH FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
		     "³" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "³ " AT 1212
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     "data files should be made before proceeding.
      -		     "    " WITH FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
		     "³" WITH FOREGROUND-COLOR 4 HIGHLIGHT
			      BACKGROUND-COLOR 4.
	     DISPLAY "³            " AT 1312
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
		     "                 ³"
		      WITH FOREGROUND-COLOR 4 HIGHLIGHT
			   BACKGROUND-COLOR 4.
	     DISPLAY "À" AT 1412
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 4
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -		     "ÄÄÄÄÄÙ" WITH FOREGROUND-COLOR 4 HIGHLIGHT
				   BACKGROUND-COLOR 4.
	     PERFORM SCREEN-SHADOW.

       XA02.
	     ACCEPT WS-OPTION AT 1352
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 4 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT(WS-OPTION = "Y" OR "N")
	       GO TO XA02.
	     PERFORM RESTORE-SCREEN-3.
	   IF WS-OPTION = "N"
014970	       MOVE 9		 TO WS-STM
014990	       GO TO XA999-EXIT.
	   IF W05-CON-TYPE = "A"
	       OPEN OUTPUT DBTCON
	       CLOSE DBTCON
	       OPEN I-O DBTCON.
      *ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *³	      EXTRACTING CONSOLODATED STATEMENTS	      ³°°
      *ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´°°
      *³ Debtor Ac. : AAAAAA NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN ³°°
      *³ Transaction: AAAAAA CCCCMMYY TT RRRRRRRR		      ³°°
      *ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°
      *  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
	     MOVE 10		 TO SHADE-ROW.
	     MOVE 10		 TO SHADE-COL.
	     MOVE 62		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0908
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "¿" WITH FOREGROUND-COLOR 5 HIGHLIGHT
			      BACKGROUND-COLOR 5.
	     DISPLAY "³              " AT 1008
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "EXTRACTING CONDOLODATED STATEMENTS              "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "³" WITH FOREGROUND-COLOR 5 HIGHLIGHT
			      BACKGROUND-COLOR 5.
	     DISPLAY "Ã" AT 1108
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -		     "ÄÄÄÄÄÄÄÄÄÄÄÄ"
		      WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 5
		     "´" WITH FOREGROUND-COLOR 5 HIGHLIGHT
			      BACKGROUND-COLOR 5.
	     DISPLAY "³ " AT 1208
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "Debtor Ac. : "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "                                                ³"
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     DISPLAY "³ " AT 1308
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "Transaction: "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "                                                ³"
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     DISPLAY "À" AT 1408
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -		     "ÄÄÄÄÄÄÄÄÄÄÄÄÙ"
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Generate transfer transactions to CONSOLODATED account.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
019060	     ACCEPT WS-OPTION AT 0532
019070		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
019130	   IF WS-OPTION = "N"
019140	       GO TO XB299-EXIT.
             DISPLAY "Waiting for printer response" AT 1412
                      WITH FOREGROUND-COLOR 14 BLINK.
             OPEN OUTPUT STMREP.
	     CALL X"91" USING X91-RES X91-FUN STMREP.
010410	     DISPLAY WS-BLNK78 AT 1402 WITH FOREGROUND-COLOR 3.
	     MOVE WS-12CPI	 TO ST3-DET.
             WRITE ST3-LINE1 BEFORE 0 LINES.
019560 XB10-COMP.
	     MOVE 0401		 TO CPOS.
	     PERFORM ERASE-SCREEN-LOOP UNTIL CLIN = 24.
019590       MOVE "Y"            TO WS-EXPAND.
019730	     DISPLAY "Statement message" AT 0402.
019740	     DISPLAY "English" AT 0603.
019860	     DISPLAY "Afrikaans" AT 1003.
019980	     MOVE SPACES TO W30-MESS-A (1) W30-MESS-A (2).
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
020180	     ACCEPT W30-SMES(1, 1) AT 1014
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
020010			 BACKGROUND-COLOR 5 UPDATE AUTO.
020200	     ACCEPT W30-SMES(1, 2) AT 1114
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
020010			 BACKGROUND-COLOR 5 UPDATE AUTO.
020200	     ACCEPT W30-SMES(1, 3) AT 1214
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
020010			 BACKGROUND-COLOR 5 UPDATE AUTO.
020240	     DISPLAY WS-CHK AT 1602
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT.
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
020410	     ACCEPT WS-MESS AT 1859
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-MESS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
020420	   IF NOT (WS-MESS = "30" OR "60" OR "90" OR "XX")
020430	       GO TO XB11A.
	     MOVE 1		 TO WS-LENGTH.

       XB11B.
	     MOVE "Print 'I'nvoice or 'O'ther reference  [I]"
				 TO WS-ERR-MES.
	     MOVE "I"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	     MOVE WS-OPTION	 TO WS-REF.
020250	   IF NOT (WS-REF = "I" OR "O")
	       GO TO XB11B.

020440 XB12.
020450       MOVE 1              TO WS-SUB-3.
020460       MOVE 1              TO WS-PARKEY.
020470       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
020480       MOVE PAR-COMPANY    TO W95-COMP.
020490       MOVE PAR-TELEPHONE  TO W95-TEL.
020510       ADD 1 TO WS-PARKEY.
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
020630	     ACCEPT WS-OPTION AT 0661
020640		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
020700	   IF WS-OPTION = "N"
020710         MOVE 1            TO WS-S8
020720     ELSE
020730         MOVE ZERO         TO WS-S8.
020740	     DISPLAY "Selective print (" AT 0812
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT ")".

020750 XB13.
020760	     ACCEPT WS-OPTION AT 0834
		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
020830	   IF NOT (WS-OPTION = "N" OR "Y")
020840	       GO TO XB13.
020850     IF WS-OPTION = "Y"
020860         MOVE 1            TO WS-NTP
020870	       GO TO XB125
020880     ELSE
020890         MOVE ZERO         TO WS-NTP.
020900	     DISPLAY "Is this a restart (" AT 1012
		     "Y" WITH FOREGROUND-COLOR 6 HIGHLIGHT "/"
		     "N" WITH FOREGROUND-COLOR 6 HIGHLIGHT ")".

020910 XB13A.
020920	     ACCEPT WS-OPTION AT 1036
		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
020990	   IF NOT (WS-OPTION = "Y" OR "N")
021000	       GO TO XB13A.
021010     IF WS-OPTION = "Y"
021020	       GO TO XB130.

       XB14.
	     MOVE "'A'll statements or 'R'ange of statements [A]"
				 TO WS-ERR-MES.
	     MOVE "A"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	   IF NOT(WS-OPTION = "A" OR "R")
	       GO TO XB14.
	     MOVE WS-OPTION	 TO WS-RANGE.
	     DISPLAY "Debtor account:" AT 1412.
021030       MOVE 1              TO WS-SUB-2.

       XB14-RANGE-CHECK.
	   IF WS-RANGE = "R"
	       MOVE SPACES	 TO WS-ASTART WS-AEND
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	Save screen details and display the get range panel.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       PERFORM XB300
	       MOVE 0726	 TO COL-AND-LINE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	  First account in the range that must be printed.     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       PERFORM XB500
	       IF WS-ERROR = 9
		   GO TO XB299-EXIT
	       END-IF
	       MOVE W80-ACNO	 TO WS-ASTART
	       MOVE 0926	 TO SPOS
	       MOVE SPOS	 TO COL-AND-LINE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	  Last account in the range that must be printed.      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       PERFORM XB500
	       IF WS-ERROR = 9
		   GO TO XB299-EXIT
	       END-IF
	       MOVE W80-ACNO	 TO WS-AEND
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 Start reading from the first account in the range.    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       PERFORM XB400
	       GO TO XB20-LOOP
	   END-IF.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	 Start reading from the first account in the file.     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
021040 XB15-INDEX.
	   IF PREVIOUS-STATEMENT
	       MOVE " !"	 TO DBH-NAME
	       PERFORM START-AT-DEBH-NAME THRU READ-DBHIST-EXIT
	   ELSE
	       MOVE " !"	 TO DEB-NAME
	       PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
010780	   IF WS-F-ERROR = 6 OR 56
021070	       GO TO XB299-EXIT.
      *
      *    The printing of Consolodated statements needs to be changed
      *    and tested extensively.
      *    As the extracting and transferring has already been done
      *    when the statements were originally printed and these rou-
      *    tines need to be by-passed when printing history statements.
      *    The code will be commented out of the running programs until
      *    the testing has been completed.
      *
      *	   IF NOT PREVIOUS-STATEMENT
      *	       IF (W05-CON-STM = "Y") AND (WS-STM = 0)
      *		   MOVE 1	 TO WS-STM
      *		   PERFORM XA000
      *		   IF WS-STM = 9
      *		       GO TO XB299-EXIT
      *		   ELSE
      *		       GO TO XB15-INDEX.

021080 XB20-LOOP.
             PERFORM PRINT-ABORT.
           IF WS-OPTION = "Y"
014970         MOVE "**** PRINT ABORTED BY OPERATOR ****" 
                                 TO ST3-DET
014980         WRITE ST3-LINE1 BEFORE PAGE
014990	       GO TO XB299-EXIT.
	   IF WS-RANGE = "R"
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	Check if last account in the range has been printed.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Check if this is the last account in range to be printed. ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
021180	     MOVE SPACES	 TO W40-NAME1.
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
021350     IF W60-NCHAR (WS-SUB-6) NOT = SPACE
021360	       GO TO XB30-INIT.
021370       SUBTRACT 1          FROM WS-SUB-6.
021380     IF WS-SUB-6 > 0
021390	       GO TO XB25-TITLE.

021400 XB30-INIT.
021410       MOVE 1              TO WS-SUB-5.
021420       ADD 2               TO WS-SUB-6.
021430       MOVE W40-NCHAR (25) TO W60-NCHAR (WS-SUB-6).
021440       ADD 1               TO WS-SUB-6.
021450       MOVE "."            TO W60-NCHAR (WS-SUB-6).
021460     IF W40-NCHAR (49) = SPACE
021470	       GO TO XB35-SURNAME.
021480       ADD 1               TO WS-SUB-6.
021490       MOVE W40-NCHAR (49) TO W60-NCHAR (WS-SUB-6).
021500       ADD 1               TO WS-SUB-6.
021510       MOVE "."            TO W60-NCHAR (WS-SUB-6).

021520 XB35-SURNAME.
021530       ADD 2               TO WS-SUB-6.

021540 XB40-LOOP.
021550       MOVE W40-NCHAR (WS-SUB-5) 
                                 TO W60-NCHAR (WS-SUB-6).
021560     IF WS-SUB-5 < 24
021570         ADD 1             TO WS-SUB-5
021580         ADD 1             TO WS-SUB-6
021590	       GO TO XB40-LOOP.
021600       MOVE W60-NAME       TO W40-NAME.

021610 XB45-ADD.
021620       MOVE DEB-ADDRESS    TO W60-ADDRESS.
021630       PERFORM CA175-ADDRESS.
021640       MOVE 0              TO WS-PAGE.

021650 XB50-STM.
021660       MOVE SPACES         TO ST3-DET.
021670       WRITE ST3-LINE1 BEFORE 1 LINES.
	     MOVE WS-EXPP	 TO ST3-DET.
	     WRITE ST3-LINE1 BEFORE 0 LINES.
021740	     MOVE SPACES	 TO ST3-LINE1.
019250	     MOVE "STATEMENT/STAAT"
				 TO ST6-HEADER.
019230	     WRITE ST3-LINE1 BEFORE 2 LINES.
021720	     MOVE WS-EXPP	 TO ST3-LINE1.
021730	     WRITE ST3-LINE1 BEFORE 0 LINES.
021740	     MOVE SPACES	 TO ST3-LINE1.
021750	     MOVE W95-COMP	 TO ST3-EXCOMP.
021760	     WRITE ST3-LINE1 BEFORE 0 LINES.
021770	     MOVE WS-ECAN	 TO ST3-LINE1.
021780	     WRITE ST3-LINE1 BEFORE 0 LINES.
	     MOVE SPACES	 TO ST3-DET.
021830       WRITE ST3-LINE1 BEFORE 2 LINES.
021840       MOVE SPACES         TO ST3-DET.
021850       MOVE W95-ADD1       TO ST3-ADD.
022100     IF W95-TEL NOT = SPACES
022120         MOVE "Telephone/Telefoon" 
                                 TO ST3-TEL
022130         MOVE W95-TEL      TO ST3-TNO.
021860       WRITE ST3-LINE1 BEFORE 1 LINES.
021870       MOVE 6              TO WS-ERROR.
021880       MOVE SPACES         TO ST3-DET.
021890     IF W95-ADD2 = SPACES
021900	       GO TO XB55-POST.
021910       SUBTRACT 1          FROM WS-ERROR.
021920       MOVE W95-ADD2       TO ST3-ADD.
021930       WRITE ST3-LINE1 BEFORE 1 LINES.
021940     IF W95-ADD3 = SPACES
021950	       GO TO XB55-POST.
021960       SUBTRACT 1          FROM WS-ERROR.
021970       MOVE W95-ADD3       TO ST3-ADD.
021980       WRITE ST3-LINE1 BEFORE 1 LINES.
021990     IF W95-ADD4 = SPACES
022000	       GO TO XB55-POST.
022010       SUBTRACT 1          FROM WS-ERROR.
022020       MOVE W95-ADD4       TO ST3-ADD.
022030       WRITE ST3-LINE1 BEFORE 1 LINES.

022040 XB55-POST.
022050       MOVE SPACES         TO ST3-DET.
022060	   IF NOT (W95-POST = ZERO)
022070         MOVE W95-POST     TO ST3-PC1.
022080       WRITE ST3-LINE1 BEFORE WS-ERROR LINES.
022090       MOVE SPACES         TO ST3-DET.

       XB58.
022750       ADD 1               TO WS-PAGE.

022140 XB60-DEBT.
022160       MOVE SPACES         TO ST3-DET.
022170       MOVE W40-NAME       TO ST3-NAME.
022180       MOVE 0              TO WS-ERROR.
022200       WRITE ST3-LINE1 BEFORE 1 LINES.
022210       MOVE SPACES         TO ST3-DET.
022220       MOVE W40-ADD1       TO ST3-NAME.
	     MOVE "AC/REK:"	 TO ST3-AH1.
022600       MOVE DEB-ACNO       TO ST3-AC1.
022230       WRITE ST3-LINE1 BEFORE 1 LINES.
022240       MOVE SPACES         TO ST3-DET.
022260       MOVE W40-ADD2       TO ST3-NAME.
022430     IF W40-ADD2 = SPACES
022440         MOVE 1            TO WS-ERROR
022450	       IF NOT (DEB-POST = "0000" OR "9999")
022460             MOVE DEB-POST TO ST3-PC2.
022470       WRITE ST3-LINE1 BEFORE 1 LINES.
022480       MOVE SPACES         TO ST3-DET.
022490       MOVE W40-ADD3       TO ST3-NAME.
022500     IF WS-ERROR = 0
022520         IF W40-ADD3 = SPACES
022530             MOVE 1        TO WS-ERROR
022540		   IF NOT (DEB-POST = "0000" OR "9999")
022550                 MOVE DEB-POST 
                                 TO ST3-PC2.
	     MOVE "PG/BL :"	 TO ST3-AH1.
022760	     MOVE WS-PAGE	 TO ST6-PAGE.
022570       WRITE ST3-LINE1 BEFORE 1 LINES.
022580       MOVE SPACES         TO ST3-DET.
022590       MOVE W40-ADD4       TO ST3-NAME.
022610     IF WS-ERROR = 0
022630         IF W40-ADD4 = SPACES
022640             MOVE 1        TO WS-ERROR
022650		   IF NOT (DEB-POST = "0000" OR "9999")
022660                 MOVE DEB-POST 
                                 TO ST3-PC2.
022680       WRITE ST3-LINE1 BEFORE 1 LINES.
022690       MOVE SPACES         TO ST3-DET.
022700     IF WS-ERROR = 0
022710	       IF NOT (DEB-POST = "0000"  OR "9999")
022720             MOVE DEB-POST TO ST3-PC2.
	     MOVE "DATE  :"	 TO ST3-AH1.
022190	     MOVE W12-TODAY	 TO ST3-DATE.
022730	     WRITE ST3-LINE1 BEFORE 2 LINES.
022740	     MOVE SPACES	 TO ST3-DET.
	     MOVE "   Date      Description     Reference         Debit
      -		  "         Credit          Balance" TO ST6-HD1.
022730	     WRITE ST3-LINE1 BEFORE 0 LINES.
022730	     WRITE ST3-LINE1 BEFORE 1 LINES.
	     MOVE "   Datum     Beskrywing      Verwysing         Debiet
      - 	  "         Krediet         Balans"  TO ST6-HD1.
022730	     WRITE ST3-LINE1 BEFORE 0 LINES.
022730	     WRITE ST3-LINE1 BEFORE 2 LINES.
022740	     MOVE SPACES	 TO ST3-DET.

022810 XB75-BFWD.
022820       MOVE 0              TO W25-WHOLE W90-BAL.
	   IF OPEN-ITEM
	       GO TO XB76.
022830	   IF DEB-STM = 0
022840	       GO TO XB80-TRAN.
022850     IF DEB-LANG = "A"
022860         MOVE "REK GELEWER" 
				 TO ST3-BFWD
022870     ELSE
022880         MOVE "A/C RENDERED" 
				 TO ST3-BFWD.
022900	     MOVE DEB-STM	 TO ST3-TBAL W90-BAL.
022930       WRITE ST3-LINE1 BEFORE 1 LINES.
022940       MOVE SPACES         TO ST3-DET.
022950	     ADD 1		 TO W25-WHOLE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ For accounts that are Brought Forward skip the recalcula- ³
      *    ³ tion of age analysis.				       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     GO TO XB80-TRAN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Calculate the age analysis for this account (OPEN ITEM)  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
	       IF NOT (TRA-DATE > W30-120)
		   MOVE 5	 TO TRA-AGE
	       ELSE
	       IF NOT (TRA-DATE > W30-90)
		   MOVE 4	 TO TRA-AGE
	       ELSE
	       IF NOT (TRA-DATE > W30-60)
		   MOVE 3	 TO TRA-AGE
	       ELSE
	       IF NOT (TRA-DATE > W30-30)
		   MOVE 2	 TO TRA-AGE
	       ELSE
	       IF NOT (TRA-DATE > W30-CUR)
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
	   IF PREVIOUS-STATEMENT
022970	       MOVE DEB-ACNO	 TO TRH-KEY
	       PERFORM START-AT-DTRH-KEY THRU READ-DTHIST-EXIT
	   ELSE
022970	       MOVE DEB-ACNO	 TO TRA-KEY
	       PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
	   IF WS-F-ERROR = 5 OR 55
022980	       GO TO XB90-SEND.

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
	     MOVE W22-DTE3	 TO ST3-TDTE.
	   IF WS-REF = "O"
	      IF NOT (TRA-REG = SPACES)
		  MOVE TRA-REG	 TO ST3-TREF
	      ELSE
		  MOVE TRA-REF	 TO ST3-TREF
	      END-IF
	   ELSE
023050	   IF NOT (TRA-REF = "00000000" OR SPACES)
023130	       MOVE TRA-REF	 TO ST3-TREF.
023070     IF DEB-LANG = "A"
023080	       MOVE W00-A-DESC (TRA-CODE) TO ST3-TDESC
023090     ELSE
023100	       MOVE W00-E-DESC (TRA-CODE) TO ST3-TDESC.
           IF TRA-VALUE > ZERO
023220         MOVE TRA-VALUE    TO ST3-TDBT
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		       A L L O C A T I O N S		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *	       IF NOT (TRA-ALLOCATED = ZERO)
      * 	    MULTIPLY -1 BY TRA-ALLOCATED
      *				 GIVING ST3-TCRD
      *		   MOVE "Pmt["	 TO ST3-PD
      *		   MOVE "]"	 TO ST3-CB
      *	      END-IF
	   ELSE
023220         MOVE TRA-VALUE    TO ST3-TCRD.
             ADD TRA-VALUE       TO W90-BAL.
             MOVE W90-BAL        TO ST3-TBAL.
023230       WRITE ST3-LINE1 BEFORE 1 LINES.
023240       MOVE SPACES         TO ST3-DET.
023250       ADD 1               TO W25-WHOLE.
023260	   IF W25-WHOLE > WS-DET-LINES
023270	       GO TO XB120-MULTI.
023310	     GO TO XB85-TLOOP.

023320 XB90-SEND.
	     ADD 2 WS-DET-LINES  GIVING W25-DECIMAL.
023330*      MOVE 36		 TO W25-DECIMAL.
023340       SUBTRACT W25-WHOLE FROM W25-DECIMAL.
023350       WRITE ST3-LINE1 BEFORE W25-DECIMAL LINES.
023390       MOVE W30-STL (1)    TO ST3-MESS.
022230	     WRITE ST3-LINE1 BEFORE 1 LINES.
022240       MOVE SPACES         TO ST3-DET.
023440       MOVE W30-STL (2)    TO ST3-MESS.
022230	     WRITE ST3-LINE1 BEFORE 1 LINES.
022240       MOVE SPACES         TO ST3-DET.
023440	     MOVE W30-STL (3)	 TO ST3-MESS.
022230	     WRITE ST3-LINE1 BEFORE 2 LINES.
022240       MOVE SPACES         TO ST3-DET.
023410	     MOVE "INTEREST"	 TO ST3-A1
023420	     MOVE "91+ DAYS"	 TO ST3-A2
023430	     MOVE "61-90 DAYS"	 TO ST3-A3.
023560	     MOVE "31-60 DAYS"	 TO ST3-A4
023570	     MOVE "01-30 DAYS"	 TO ST3-A5.
023400	     WRITE ST3-LINE1 BEFORE 1 LINES.
023520	     MOVE SPACES	 TO ST3-DET.
	   IF OPEN-ITEM
023640	       MOVE W35-INT	 TO ST3-INT
023470	       ADD W35-90 W35-120 W35-150
				 GIVING ST3-3MTHS
023480	       MOVE W35-60	 TO ST3-2MTHS
023490	       MOVE W35-30	 TO ST3-1MTH
023630	       MOVE W35-CUR	 TO ST3-CUR
	   ELSE
023640	       MOVE DEB-INT	 TO ST3-INT
023470	       ADD DEB-60 DEB-90 DEB-120
				 GIVING ST3-3MTHS
023480	       MOVE DEB-30	 TO ST3-2MTHS
023490	       MOVE DEB-CUR	 TO ST3-1MTH
023630	       MOVE W90-CUR	 TO ST3-CUR.
023360	     MOVE DEB-BAL	 TO ST3-TBAL.
023660       WRITE ST3-LINE1 BEFORE PAGE.
023670       MOVE SPACES         TO ST3-DET.

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
023880		MOVE W30-60D (WS-MSUB)
				  TO W30-STM
023890	       GO TO XB115.
023900     IF WS-MESS = "60"
023910	       GO TO XB112.
023920     IF DEB-30 > ZERO
023930		MOVE W30-30D (WS-MSUB)
				  TO W30-STM
023940	       GO TO XB115.

023950 XB112.
023960       MOVE W30-MESS-A (WS-MSUB) 
                                 TO W30-STM.
023970 XB115.
023980       EXIT.

023990 XB120-MULTI.
024020	     PERFORM XB90-SEND.
024030	     PERFORM XB50-STM THRU XB60-DEBT.
024040       MOVE ZERO TO W25-WHOLE.
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
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
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
024130       PERFORM CA155-GET-DEBTOR.
024140     IF WS-ERROR NOT = ZERO
024150	       GO TO XB125.
	     DISPLAY DEB-NAME AT 1031
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
020240	     DISPLAY WS-CHK AT 1202.
020250	     ACCEPT WS-OPTION AT 1239
020260		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	Save screen details and display the get range panel.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       PERFORM XB300
	       MOVE 0726	 TO COL-AND-LINE SPOS
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	Display the restart account in the range to printed.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	  Last account in the range that must be printed.      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
024850	   IF W80-ACNO NOT = DEB-ACNO
	       GO TO XB135.
024960	     GO TO XB21.

024970 XB200-END.
             CLOSE STMREP.

025000 XB299-EXIT.
025010       EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	Save screen details and display the get range panel.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³Start reading from the first account in the range to print.³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       XB400	       SECTION 51.
       XB405.
024220	     MOVE WS-ASTART	 TO W80-ACNO.
024240       PERFORM CA155-GET-DEBTOR.
	   IF PREVIOUS-STATEMENT
	       PERFORM START-AT-DEBH-NAME THRU READ-DBHIST-EXIT
	   ELSE
	       PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
	     PERFORM RESTORE-SCREEN-3.

       XB499.
	     EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Get account numbers in the range that must be printed.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       XB500	       SECTION 51.
       XB505.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Display the USER options available for this input screen. ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
      /    *************************************************************
      *    * P R I N T	 C O N S O L O D A T E D   S T A T E M E N T S *
      *    *************************************************************
019020 XC000	       SECTION 51.
019030 XC00.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 10		 TO SHADE-ROW.
	     MOVE 19		 TO SHADE-COL.
	     MOVE 46		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
		      AT 0917 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				   BACKGROUND-COLOR 5
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "³ " AT 1017 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				       BACKGROUND-COLOR 5
		     "Printing Consolodated Statements:       "
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     " ³" WITH BACKGROUND-COLOR 0 FOREGROUND-COLOR 5.
	     DISPLAY "À" AT 1117 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				      BACKGROUND-COLOR 5
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH BACKGROUND-COLOR 0 FOREGROUND-COLOR 5.
	     PERFORM SCREEN-SHADOW.
	     OPEN I-O DBTCON.
      *	     MOVE ZERO		 TO WS-CON.
	     MOVE " !"		 TO DBT-CON.
	     MOVE SPACES	 TO DBT-IND
				    DBT-AC
				    DBT-REF.
	     MOVE ZERO		 TO DBT-DATE
				    DBT-CODE.
	     PERFORM START-AT-DBT-KEY THRU READ-DBTCON-EXIT.
	   IF WS-F-ERROR = 64
	       MOVE "No transactions for Consolodated Statements"
				 TO WS-ERR-MES
	       PERFORM ERROR-LENGTH THRU ERROR-EXIT
	       .
      *	       GO TO XC900.

       XC05.
	     PERFORM READ-DBTCON-NEXT THRU READ-DBTCON-EXIT.
      *    *************************************************************
      *     E N D   O F   C O N S O L O D A T E D   S T A T E M E N T S
      *    *************************************************************
	   IF WS-F-ERROR = 64
	       GO TO XC700.
      *    *************************************************************
      *     I S   T H I S   T H E   F I R S T	C O N S O L O D A T E D
      *     S T A T E M E N T	T O   B E   P R I N T E D   ?
      *    *************************************************************

      *	   IF WS-CON = ZERO
	       MOVE DBT-CON	 TO W80-ACNO
				    W80-ACNO2
	       PERFORM CA155-GET-DEBTOR
	       .
      *	       PERFORM XC500 THRU XC560.
      *    *************************************************************
      *      H A S   T H E   C O N S O L O D A T I O N	 A C C O U N T
      *      C H A N G E D   ?
      *    *************************************************************

      *	   IF NOT(DBT-CON = W80-ACNO)
      *	       GO TO XC************

      *    *************************************************************
      * 	 H A S	 T H E	 S U B O R N A T E   A C C O U N T
      * 	 C H A N G E D	 ?
      *    *************************************************************

      *	   IF DBT-AC = W80-ACNO2
      *	       GO TO XC***************


023020	   IF DBT-VALUE = ZERO
023030	       GO TO XC05.
023040	     MOVE DBT-DATE	 TO W22-DTE2.
	     MOVE W22-CC2	 TO W22-CC3.
	     MOVE W22-YY2	 TO W22-YY3.
	     MOVE W22-MM2	 TO W22-MM3.
	     MOVE W22-DD2	 TO W22-DD3.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    **** END OF CONSOLODATED TRANSACTIONS ****	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       XC700.

       XC999-EXIT.
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
	     PERFORM OPT-MESSAGE.
	     MOVE WS-OPTION	 TO WS-STMT.
020250	   IF NOT (WS-STMT = "C" OR "P")
	       GO TO YA05.
	   IF NOT PREVIOUS-STATEMENT
	       GO TO YA999.

       YA10.
019040	     DISPLAY CLR-SCREEN.
	     DISPLAY "DEBTORS (A/R) PREVIOUS STATEMENTS" AT 0325
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
      *
      *			      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *			      ³PREVIOUS STATEMENT³²²
      *			      ³ Year : YY (00-99)³²²
      *			      ³ Month: MM (01-12)³²²
      *			      ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ²²
      *				²²²²²²²²²²²²²²²²²²²²
      *

	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0631
		      WITH FOREGROUND-COLOR 5 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "³" AT 0731 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				      BACKGROUND-COLOR 5
		     "PREVIOUS STATEMENT"
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 5
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "³ " AT 0831 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				       BACKGROUND-COLOR 5
		     "Year :    (00-99)" WITH FOREGROUND-COLOR 2
					      BACKGROUND-COLOR 5
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "³ " AT 0931 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				       BACKGROUND-COLOR 5
		     "Month:    (01-12)" WITH FOREGROUND-COLOR 2
					      BACKGROUND-COLOR 5
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "À" AT 1031 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				      BACKGROUND-COLOR 5
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" WITH FOREGROUND-COLOR 0
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Set up the run / age dates applicable to the history files³
      *    ³ when the statements and ageing of balances was executed by³
      *    ³ the system. These dates are stored in the first record of ³
      *    ³ the history account file, which is a dummy account with   ³
      *    ³ an account number of "     !" and a name "  !".	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
      *
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
	     MOVE 44		 TO USER-NUMBER.
	     MOVE 1		 TO USER-SETTING.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
	   IF TRM-STMP (WS-S6) = 1
               MOVE "LPT1"       TO W02-STATEMENT
           ELSE
           IF TRM-STMP (WS-S6) = 2
               MOVE "LPT2"       TO W02-STATEMENT
           ELSE
           IF TRM-STMP (WS-S6) = 3
               MOVE "LPT3"       TO W02-STATEMENT
           ELSE
           IF TRM-STMP (WS-S6) = 4
               MOVE "COM1"       TO W02-STATEMENT
           ELSE
           IF TRM-STMP (WS-S6) = 5
               MOVE "COM2"       TO W02-STATEMENT
           ELSE
           IF TRM-STMP (WS-S6) = 9
	       MOVE LS-USER	 TO W02-USE.
             ADD 501 TRM-PRN4 (WS-S6)
                                 GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE TRM-CONP (WS-S6)
				 TO WS-COND.
             MOVE TRM-NORP (WS-S6)
				 TO WS-NORM.
             MOVE TRM-EXPP (WS-S6)
				 TO WS-EXPP.
             MOVE TRM-ECAN (WS-S6)
				 TO WS-ECAN.
             MOVE TRM-8LPI (WS-S6)
				 TO WS-8LPI.
             MOVE TRM-6LPI (WS-S6)
				 TO WS-6LPI.
             MOVE TRM-10CPI (WS-S6)
				 TO WS-10CPI.
             MOVE TRM-12CPI (WS-S6)
				 TO WS-12CPI.
             MOVE TRM-17CPI (WS-S6)
				 TO WS-17CPI.
	     MOVE TRM-LENGTH (WS-S6)
				 TO WS-PRN-LENGTH.
	     MOVE TRM-PAGE (WS-S6)
				 TO WS-PGE-LENGTH.
	     MOVE 34		 TO WS-DET-LINES.
	   IF WS-PGE-LENGTH > 66
	       COMPUTE WS-DET-LINES = WS-DET-LINES +
				      (WS-PGE-LENGTH - 66)
	   ELSE
	   IF WS-PGE-LENGTH < 66
	       COMPUTE WS-DET-LINES = WS-DET-LINES -
				      (66 - WS-PGE-LENGTH).

       ZA02.
	     MOVE 6		 TO WS-PARKEY.
025460       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
011800	   IF NOT(PAR-CON-STM = "N" OR "Y")
011810	       MOVE "N" 	 TO PAR-CON-STM.
	   IF NOT(PAR-CON-TYPE = "A" OR "S")
	       MOVE "S" 	 TO PAR-CON-TYPE.
	     MOVE PAR-CON-STM	 TO W05-CON-STM.
	     MOVE PAR-CON-TYPE	 TO W05-CON-TYPE.
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
025680     IF PAR-STMINV = "S" OR "X"
025690	       MOVE "N" 	 TO WS-HEAD
025700     ELSE
025710	       MOVE "Y" 	 TO WS-HEAD.
      *    *************************************************************
      *    ****   G E T   D E B T O R	M O N T H - E N D   D A Y   ****
      *    *************************************************************
	     MOVE 1		 TO WS-NETKEY.
	     PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
      *	   IF DEB-MTH-END < 21 OR > 31 AND < 99
      *	       MOVE 99		 TO DEB-MTH-END.
      *
      *    ****    S E T   U P   A G E   D A T E S
      *
	     MOVE W12-T-YMD	 TO W22-DTE2.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ The month end dates are stored in the Control file in the ³
      *    ³ first record as Cur, 30d, 60d, 90d and 120d and the next  ³
      *    ³ routine is used since Ver 8.11.06. 		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE DEB-CUR-DTE	 TO W30-CUR.
	     MOVE DEB-30D-DTE	 TO W30-30.
	     MOVE DEB-60D-DTE	 TO W30-60.
	     MOVE DEB-90D-DTE	 TO W30-90.
	     MOVE DEB-120-DTE	 TO W30-120.
044780	     GO TO ZA999-EXIT.
      *    *************************************************************
      *    ****   F I L E   L I M I T	E X C E E D E D
      * 	  (Config.sys Dos/Win/Windows 95/98/NT/2000)
      * 	  (Config.nt Windows XP)
      * 	  (System.ini Windows.me)
      *    *************************************************************
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
039810	     DISPLAY "Check the FILES option in CONFIG.SYS (Dos,Windows
      -		     "3/95/98/NT/2000)" AT 0908.
	     DISPLAY "CONFIG.NT  (Windows XP)" AT 1034.
	     DISPLAY "SYSTEM.INI (Windows ME)" AT 1134.
039820	     DISPLAY "Press any key to continue" AT 1308.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     GO TO ZA205.

       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
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
021950       MOVE WS-G3          TO WS-MCHR (8)  WS-MCHR (49)
021960                              WS-MCHR (61) WS-MCHR (72)
021960				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (8)  WS-BCHR (49)
021980                              WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"
				 TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"
				 TO WS-WRKHD
	       MOVE LS-USER
				 TO WS-WRKST.
025840 ZA200.
025850       DISPLAY "Files locked - Try later" AT 2312
025860                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
025870       DISPLAY "Press any key" AT 2512 
025880		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
025890	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
025920 ZA999-EXIT.
025930       EXIT.
      /
025940 ZB000		     SECTION 91.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
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
041880     IF WS-STATUS = "10"
041890	       MOVE "End of FILE"
				 TO WS-STAT-MESSAGE
041900     ELSE
041910     IF WS-STATUS = "22"
041920	       MOVE "Duplicate record number"
				 TO WS-STAT-MESSAGE
041930     ELSE
041940     IF WS-STATUS = "23"
041950	       MOVE "Invalid record number"
				 TO WS-STAT-MESSAGE
041960     ELSE
041970     IF WS-STATUS = "24"
041980	       MOVE "DISK full"  TO WS-STAT-MESSAGE
041990     ELSE
042000     IF WS-STATUS = "30"
042010         MOVE "DISK error" TO WS-STAT-MESSAGE
042020     ELSE
042030     IF WS-STATUS = "94"
042040	       MOVE "FILE locked"
				 TO WS-STAT-MESSAGE.
045380       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
045390       DISPLAY "Status " AT 1212 
                      WS-STATUS WITH FOREGROUND-COLOR 11
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
045400     IF WS-STATUS NOT = "94" 
045410         DISPLAY "Key    " AT 1412 
                        WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
