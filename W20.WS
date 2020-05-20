002640 01  W20-DATE.
	     03  W20-CCYY.
             05  W20-CC.
               07  W20-CENT  PIC  9(02).
002650       05  W20-YY.
002660	   07  W20-YEAR  PIC  9(02).
           03  W20-CNYR REDEFINES W20-CCYY
                             PIC  9(04).
002670     03  W20-MM.
002680       05  W20-MONTH   PIC  9(02).
002690     03  W20-DD.
002700       05  W20-DAY     PIC  9(02).
002710 01  W20-DTE1 REDEFINES W20-DATE.
002720     03  W20-DTE       PIC  9(08).

       01  W21-DATES.
           03  W21-DATE      PIC  9(08).
           03  W21-DATE2 REDEFINES W21-DATE.
             05  W21-CCYY    PIC  9(04).
             05  W21-MM      PIC  9(02).
             05  W21-DD      PIC  9(02).
           03  W21-DTE1 REDEFINES W21-DATE.
             05  W21-DTE     PIC  9(08).
           03  W22-DATE      PIC  9(08).

002730 01  W22-DATES.
002740     03  W22-DTE1      PIC  9(08).
002750     03  W22-DTE2      PIC  9(08).
002760     03  W22-DATE2 REDEFINES W22-DTE2.
             05  W22-CC2     PIC  9(02).
002770       05  W22-YY2     PIC  9(02).
002780       05  W22-MM2     PIC  9(02).
002790       05  W22-DD2     PIC  9(02).
002760     03  W22-DATE3 REDEFINES W22-DTE2.
             05  W22-CY2     PIC  9(04).
             05  FILLER      PIC  9(04).
002800     03  W22-DTE3      PIC  9(08).
002810     03  W22-DATE3 REDEFINES W22-DTE3.
002820       05  W22-DD3     PIC  9(02).
002830       05  W22-MM3     PIC  9(02).
             05  W22-CC3     PIC  9(02).
002840       05  W22-YY3     PIC  9(02).
