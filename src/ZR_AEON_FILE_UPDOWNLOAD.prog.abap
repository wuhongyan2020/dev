*&---------------------------------------------------------------------*
*& Report ZR_AEON_FILE_UPDOWNLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZR_AEON_FILE_UPDOWNLOAD.

PARAMETERS : LCARRID LIKE SFLIGHT-CARRID OBLIGATORY,
LCURR TYPE S_CURRCODE DEFAULT 'USD'.

START-OF-SELECTION.
    PERFORM F_FILE_UPLOAD.
    PERFORM F_F4_FOR_ERROR.
    PERFORM F_FILE_SAVE.

FORM F_FILE_UPLOAD.
*   選択画面「ファイルアプロード」検索ヘルプの設定
    CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
        EXPORTING
            WINDOW_TITLE = 'ファイルアプロード'
            FILE_FILTER = 'CSVファイル（*.csv）|*.csv|EXCELファイル（*.xlxs）'
            INITIAL_DIRECTORY = 'D:/SAP_UP'
        CHANGING
            FILE_TABLE = LIT_FIELNM
            RC = LV_RETURN
        EXCEPTIONS
            FILE_OPEN_DIALOG_FAILED = 1
            CNTL_ERROR = 2
            ERROR_NO_GUI = 3
            NOT_SUPPORTED_BU_GUI = 4
            OTHERS = 5.

    IF SY-SUBRC <> 0. 
        MESSAGE ID      SY-MSGID
        TYPE            SY-MSGTY
        NUMBER          SY-MSGNO
        WITH            SY-MSGV1
                        SY-MSGV2
                        SY-MSGV3
                        SY-MSGV4.
    ELSE.
        READ TABLE LIT_FIELNM INTO LW_FIELNM INDEX 1.
*       第１行目に選択画面「ファイルアプロード」を設置する
        P_UPLOAD = LW_FIELNM-FILENAME.
    ENDIF.
ENDFORM.

FORM F_F4_FOR_ERROR.
    DATA:
       LV_FILENAME TYPE STRING.
       LV_FILENAME_1 TYPE STRING.
       LV_PATH TYPE STRING.
       LV_FULLPATH TYPE STRING.
    
    LV_FILENAME = '教育／研修情報エラー' && '.CSV'. 

    CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
        WINDOW_TITLE = 'エラーファイル'
        DEFAULT_FILE_NAME = LV_FILENAME
        INITIAL_DIRECTORY = 'D:/SAP_DL'
    CHANGING
        FILENAME = LV_FILENAME_1
        PATH = LV_PATH
        FULLPATH = LV_FULLPATH
    EXCEPTIONS
        CNTL_ERROR = 1
        ERROR_NO_GUI = 2
        NOT_SUPPORTED_BU_GUI = 3
        OTHERS = 4.

    IF SY-SUBRC <> 0. 
        MESSAGE ID      SY-MSGID
        TYPE            SY-MSGTY
        NUMBER          SY-MSGNO
        WITH            SY-MSGV1
                        SY-MSGV2
                        SY-MSGV3
                        SY-MSGV4.
    ELSE.
        READ TABLE LIT_FIELNM INTO LW_FIELNM INDEX 1.
*       フルーパスを「エラーファイル」に保存
        P_ERROR = LV_FULLPATH.
    ENDIF.

ENDFORM.

FORM F_FILE_SAVE.
  
    DATA: 
    LV_FILENAME TYPE RLGRAP_FILENAME,      "Function用
    LV_INDEX TYPE I,                       "シリアルナンバー
    LT_INTERN TYPE KCDE_INTERN,           "kcd
    LW_INTERN TYPE KCDE_INTERN_STRUC,     "kcd
    LW_SINNSOKU TYPE TYP_W_SINNSOKu.       "作業エリア

*   パス設定
    WRITE P_UPLOAD TO LV_FILENAME.

*   CSVファイルを読み込み
    CALL FUNCTION 'KCD_CSV_FILE_TO_INTERN_CONVERT' 
    EXPORTING
        I_FILENAME = LV_FILENAME
        I_SEPARATOR = C_SEPARATOR
    TABLES
        E_INTERN = LT_INTERN
    EXCEPTIONS
        UPLOAD_CSV = 1
        UPLOAD_FILETYPE = 2
        OTHERS = 3.

    IF SY-SUBRC <> 0. 
        MESSAGE ID      SY-MSGID
        TYPE            SY-MSGTY
        NUMBER          SY-MSGNO
        WITH            SY-MSGV1
                        SY-MSGV2
                        SY-MSGV3
                        SY-MSGV4.
    ENDIF.

    LOOP AT LT_INTERN INTO LW_INTERN.
        MOVE : LW_INTERN-COL TO LV_INDEX.
        ASSIGN COMPONENT LV_INDEX OF STRUCTURE LW_SINNSOKU TO <LFS_FIELD>.
        MOVE : LW_INTERN-VALUE TO <LFS_FIELD>.

        AT END OF ROW.

*       更新テーブルを更新
        APPEND LW_SINNSOKU TO C_LIT_SINNSOKU.
        CLEAR LW_SINNSOKU.

        ENDAT.

    ENDLOOP.

ENDFORM.