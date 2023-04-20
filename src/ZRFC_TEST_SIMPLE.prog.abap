
DATA: MaxPrice LIKE SFLIGHT-PRICE.
DATA: MinPrice LIKE SFLIGHT-PRICE.
DATA: AIRDATA  LIKE STANDARD TABLE OF SFLIGHT WITH HEADER LINE.

FUNCTION ZRFC_TEST_SIMPLE.        "Function 定義
*"----------------------------------------------------------------------
*"*"ローカルインタフェース:
*"  IMPORTING
*"     REFERENCE(CARRID) TYPE  SFLIGHT-CARRID
*"     VALUE(CURRENCY) TYPE  S_CURRCODE DEFAULT 'USD'
*"  EXPORTING
*"     REFERENCE(PRICE) TYPE  S_PRICE
*"  EXCEPTIONS
*"      UNIT_NOT_FOUND
*"----------------------------------------------------------------------    
    SELECT * FROM SFLIGHT
    INTO TABLE AIRDATA
    WHERE CARRID = CARRID
    AND CURRENCY = CURRENCY.

    IF SY-SUBRC <> 0.
        RAISE UNIT_NOT_FOUND.      "Exception情報返却
    ELSE.
        PERFORM GETMAXPRICE TABLES AIRDATA. 
        PRICE = MaxPrice.
    ENDIF. 

ENDFUNCTION.

*"----------------------------------------------------------------------
*"*"FORM GetMaxPrice
*"----------------------------------------------------------------------   
FORM GetMaxPrice TABLES L_TAB STRUCTURE SFLIGHT. 
    SORT L_TAB BY PRICE ASCENDING.
    READ TABLE L_TAB INDEX 1.
    MinPrice = L_TAB-PRICE.
ENDFORM.