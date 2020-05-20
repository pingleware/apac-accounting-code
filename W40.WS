001810 01  W40-BINARY.
001820     03  W40-BIN.
001830	       05  W40-DEC     PIC  9(02)    COMP-X.
001840     03  W40-HEX REDEFINES W40-BIN.
001850	       05  W40-CHAR    PIC  X(01).
	   03  W40-N1	       PIC  9(02)    COMP-X.
	   03  W40-N2	       PIC  9(02)    COMP-X.
	   03  W40-SWITCH      PIC  9(02)    COMP-X.
001860     03  W40-CSHPASS     PIC  X(06) VALUE SPACES.

       01  W41-SAVE.
           03  W41-LENGTH      PIC 9(04) COMP-X VALUE 2000.
           03  W41-START       PIC 9(04) COMP-X VALUE 1.
	   03  W41-BUFFER      PIC 9(04) COMP-X VALUE 1.

       01  W42-ATTRIB.
	   03  FILLER	       PIC X(2000).

       01  W42-ATTRIB2.
	   03  W42-LINES OCCURS 25.
	       05  W42-BYTE OCCURS 80.
		   07  W42-CHAR PIC 9(02) COMP-X.

       01  W42-ATTRIB3.
           03  FILLER          PIC X(2000).

       01  W42-ATTRIB4.
	   03  W42-LINE OCCURS 25.
	       05  W42-BTE OCCURS 80.
		   07  W42-CHR PIC 9(02) COMP-X.

       01  W43-SCREEN.
	   03  FILLER	       PIC X(2000).

004400 01  W43-SCREEN2.
004410	   03  W43-SCREEN2A.
004420	     05  W43-LINE    OCCURS 25.
      *
      *    ****   U S E D   I N   L O O K U P	R O U T I N E S
      * 		    C R E D I T O R S
      * 		    D E B T O R S
      * 		    G E N E R A L   L E D G E R
      * 		    G U A R A N T E E
      * 		    J O B   C O S T I N G
      * 		    J O B   R E M A R K S
      * 		    S A L E S	O R D E R
      * 		    S T O C K
      * 		    W A R E H O U S E
      *
004430	      07  W43-DET.
		09  FILLER     PIC  X(09).
      *
      *    ****   U S E D   I N   L O O K U P	R O U T I N E S
      * 		    P U R C H A S E   O R D E R
      *
		09  W43-DET2.
		 11  FILLER    PIC  X(16).
      *
      *    ****   U S E D   I N   L O O K U P	R O U T I N E S
      * 		    R O U T E
      *
		 11  W43-DET2A.
		  13  FILLER   PIC  X(04).
      *
      *    ****   U S E D   I N   L O O K U P	R O U T I N E S
      * 		    C O M P A N Y   C O D E
      *
		  13  W43-DET3.
		    15 FILLER  PIC  X(02).
      *
      *    ****   U S E D   I N   L O O K U P	R O U T I N E S
      *
		    15 W43-DET3A PIC X(42).
		  13  FILLER   PIC  X(02).
		09  FILLER     PIC  X(05).

004410	   03  W43-SCREEN2B  REDEFINES W43-SCREEN2A.
004420	     05  W43-LINE2   OCCURS 25.
004430	       07  FILLER      PIC  X(10).
      *
      *    ****   U S E D   I N   L O O K U P	R O U T I N E S
      * 		    M O D E L	N U M B E R S
      *
	       07  W43-DET4A.
		 09  FILLER    PIC  X(28).
      *
      *    ****   U S E D   I N   L O O K U P	R O U T I N E S
      *
		 09  W43-DET4.
		  11  FILLER   PIC  X(13).
      *
      *    ****   U S E D   I N   L O O K U P	R O U T I N E S
      * 		    D E B T O R   S T A N D I N G   O R D E R S
      *
		  11  W43-DET5 PIC  X(24).
		 09  FILLER    PIC  X(02).
	       07  FILLER      PIC  X(03).

004410	   03  W43-SCREEN2C  REDEFINES W43-SCREEN2A.
004420	     05  W43-LINE3   OCCURS 25.
004430	       07  FILLER      PIC  X(41).
      *
      *    ****   U S E D   I N   L O O K U P	R O U T I N E S
      * 		    T E C H N I C I A N S
      *
	       07  W43-DET6    PIC  X(34).
	       07  FILLER      PIC  X(05).

004410	   03  W43-SCREEN2D  REDEFINES W43-SCREEN2A.
004420	     05  W43-LINE4   OCCURS 25.
004430	       07  FILLER      PIC  X(17).
      *
      *    ****   U S E D   I N   L O O K U P	R O U T I N E S
      * 		P O S	P I C K I N G	S L I P S
      *
	       07  W43-DET7    PIC  X(43).
	       07  FILLER      PIC  X(20).

       01  W43-SERIALS REDEFINES W43-SCREEN2.
	   03  W43-SERNO       PIC  X(18) OCCURS 111.
	   03  FILLER	       PIC  X(01).

       01  W43-SCREEN3.
           03  FILLER          PIC X(2000).

       01  W44-FUNCTION        PIC 9(02)     COMP-X.
