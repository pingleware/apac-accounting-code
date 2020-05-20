      *>
      *> VARIANTARG vartypes definitions
      *>


         03 ()-data      PIC xxxxxxxx.
         03 ()-VT-UI1        REDEFINES ()-data PIC x COMP-5.
         03 ()-VT-I1         REDEFINES ()-data PIC s9(2) COMP-5.
         03 ()-VT-I2         REDEFINES ()-data PIC s9(4) COMP-5.
         03 ()-VT-UI2        REDEFINES ()-data PIC xx COMP-5.
         03 ()-VT-I4         REDEFINES ()-data PIC s9(9) COMP-5.
         03 ()-VT-UI4        REDEFINES ()-data PIC xxxx COMP-5.
         03 ()-VT-INT        REDEFINES ()-data PIC s9(9) COMP-5.
         03 ()-VT-UINT       REDEFINES ()-data PIC xxxx COMP-5.
         03 ()-VT-R4         REDEFINES ()-data COMP-1.
         03 ()-VT-R8         REDEFINES ()-data COMP-2.
         03 ()-VT-BOOL       REDEFINES ()-data PIC s9(4) COMP-5.
         03 ()-VT-SCODE      REDEFINES ()-data PIC xxxx COMP-5.
         03 ()-VT-cyVal      REDEFINES ()-data.
            05 ()-VT-cyVal-Lo      PIC xxxx  COMP-5.
            05 ()-VT-cyVal-Hi      PIC s9(9) COMP-5.
         03 ()-VT-Date       REDEFINES ()-data COMP-2.
         03 ()-VT-BStr       REDEFINES ()-Data Pointer.
         03 ()-VT-Variant    REDEFINES ()-Data Pointer.
         03 ()-VT-UnKnown    REDEFINES ()-Data Pointer.
         03 ()-VT-IDispatch  REDEFINES ()-Data Pointer.
         03 ()-VT-SafeArray  REDEFINES ()-Data Pointer.
         03 ()-VT-BYREF      REDEFINES ()-data Pointer.

      *> ByRef types
         03 ()-VT-pUI1       REDEFINES ()-data Pointer.
         03 ()-VT-pI2        REDEFINES ()-data Pointer.
         03 ()-VT-pI4        REDEFINES ()-data Pointer.
         03 ()-VT-pR4        REDEFINES ()-data Pointer.
         03 ()-VT-pR8        REDEFINES ()-data Pointer.
         03 ()-VT-pBOOL      REDEFINES ()-data Pointer.
         03 ()-VT-pERROR     REDEFINES ()-data Pointer.
         03 ()-VT-pCY        REDEFINES ()-data Pointer.
         03 ()-VT-pDATE      REDEFINES ()-data Pointer.
         03 ()-VT-pBSTR      REDEFINES ()-data Pointer.
         03 ()-VT-pUNKNOWN   REDEFINES ()-data Pointer.
         03 ()-VT-pDISPATCH  REDEFINES ()-data Pointer.
         03 ()-VT-pARRAY     REDEFINES ()-data Pointer.
         03 ()-VT-pVARIANT   REDEFINES ()-data Pointer.
