*&---------------------------------------------------------------------*
*& Report ZR_CALLRFC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZR_CALLRFC.
    PARAMETERS : LCARRID LIKE SFLIGHT-CARRID OBLIGATORY,
                 LCURR TYPE S_CURRCODE DEFAULT 'USD'.
                 
    DATA: LPRICE TYPE S_PRICE,
            LT_AIR LIKE STANDARD TABLE OF SFLIGHT WITH HEADER LINE.

    CALL FUNCTION 'ZRFC_TEST_SIMPLE' 
        EXPORTING
            CARRID = LCARRID
            CURRENCY = LCURR
        IMPORTING
            PRICE = LPRICE
        TABLES
            AIRDATA = LT_AIR
        EXCEPTIONS
            UNIT_NOT_FOUND = 1 
            OTHERS = 2.

    IF SY-SUBRC <> 0. 
        CASE SY-SUBRC.
            WHEN 1.
            MESSAGE E001(00) WITH 'NO DATA FOUND'.
            WHEN 2.
            MESSAGE E001(00) WITH 'SYSTEM ERROR'.
        ENDCASE. 
        EXIT.
    ENDIF.

    WRITE:'Min price:',LPRICE LEFT-JUSTIFIED.
    SKIP.
    WRITE: / 'All data'. 

    LOOP AT LT_AIR.
        WRITE: /(50) SY-ULINE. 
        WRITE: / LT_AIR-CARRID,
                 LT_AIR-CONNID,
                 LT_AIR-FLDATE, 
                 LT_AIR-PRICE,
                 LT_AIR-CURRENCY. 
        AT LAST.
            WRITE: /(50) SY-ULINE. 
        ENDAT.
    ENDLOOP.
