*----------------------------------------------------------------------*
* 程序名 : 教育／研修情報受信インターフェース
* 程序ID : Z_UPD_KYOIKU
* 创建日期 :2023.2.24
*----------------------------------------------------------------------*
REPORT  Z_UPD_KYOIKU LINE-SIZE 400.

TYPE-POOLS: KCDE.

*&---------------------------------------------------------------------*
* 结构体声明
*&---------------------------------------------------------------------*

TYPES:
  BEGIN OF TYP_W_SINNSOKU,
    UPMOD TYPE C LENGTH 1 ,                                 "更新モード
    BUKRS TYPE P0001-BUKRS,                                 "会社コード
    PERNR TYPE PA0022-PERNR,                                "従業員番号
    BEGDA TYPE PA0022-BEGDA,                                "有効開始日
    ENDDA TYPE PA0022-ENDDA,                                "有効終了日
    SLAND TYPE PA0022-SLAND,                                "国コード
    AUSBI TYPE PA0022-AUSBI,                                "教育修了コード
    SLART TYPE PA0022-SLART,                                "教育機関コード
    INSTI TYPE PA0022-INSTI,                                "教育機関名
    FACCD TYPE PA0022-FACCD,                                "学部
    DPTMT TYPE PA0022-DPTMT,                                "学科
    SLTP1 TYPE PA0022-SLTP1,                                "専攻１
    SLABS TYPE PA0022-SLABS,                                "卒業区分
    EMARK TYPE PA0022-EMARK,                                "最終学歴
  END OF TYP_W_SINNSOKU,

  TYP_IT_SINNSOKU TYPE STANDARD TABLE OF TYP_W_SINNSOKU,

  BEGIN OF TYP_W_ERROR_OUT,
    UPMOD TYPE C LENGTH 1 ,                                 "更新モード
    BUKRS TYPE P0001-BUKRS,                                 "会社コード
    PERNR TYPE PA0022-PERNR,                                "従業員番号
    BEGDA TYPE PA0022-BEGDA,                                "有効開始日
    ENDDA TYPE PA0022-ENDDA,                                "有効終了日
    SLAND TYPE PA0022-SLAND,                                "国コード
    AUSBI TYPE PA0022-AUSBI,                                "教育修了コード
    SLART TYPE PA0022-SLART,                                "教育機関コード
    INSTI TYPE PA0022-INSTI,                                "教育機関名
    FACCD TYPE PA0022-FACCD,                                "学部
    DPTMT TYPE PA0022-DPTMT,                                "学科
    SLTP1 TYPE PA0022-SLTP1,                                "専攻１
    SLABS TYPE PA0022-SLABS,                                "卒業区分
    EMARK TYPE PA0022-EMARK,                                "最終学歴
  END OF TYP_W_ERROR_OUT,

  TYP_IT_ERROR_OUT TYPE STANDARD TABLE OF TYP_W_ERROR_OUT,

  BEGIN OF TYP_W_ERROR,
    UPMOD TYPE C LENGTH 1 ,                                 "更新モード
    BUKRS TYPE P0001-BUKRS,                                 "会社コード
    PERNR TYPE PA0022-PERNR,                                "従業員番号
    BEGDA TYPE PA0022-BEGDA,                                "有効開始日
    ENDDA TYPE PA0022-ENDDA,                                "有効終了日
    SLAND TYPE PA0022-SLAND,                                "国コード
    AUSBI TYPE PA0022-AUSBI,                                "教育修了コード
    SLART TYPE PA0022-SLART,                                "教育機関コード
    INSTI TYPE PA0022-INSTI,                                "教育機関名
    FACCD TYPE PA0022-FACCD,                                "学部
    DPTMT TYPE PA0022-DPTMT,                                "学科
    SLTP1 TYPE PA0022-SLTP1,                                "専攻１
    SLABS TYPE PA0022-SLABS,                                "卒業区分
    EMARK TYPE PA0022-EMARK,                                "最終学歴
    MSGTYP TYPE MSGTYP,
    MSGID TYPE MSGID,
    MSGNR TYPE MSGNR,
    MESSAGE TYPE STRING,
  END OF TYP_W_ERROR,

  TYP_IT_ERROR TYPE STANDARD TABLE OF TYP_W_ERROR,

  BEGIN OF TYP_W_UPMOD,
    UPMOD TYPE C LENGTH 1 ,                                 "upmode 1-4
    MODENM TYPE C LENGTH 8,                                 "登录 1 更新2  消除3 期间限定4
  END OF TYP_W_UPMOD,

  TYP_IT_UPMOD TYPE STANDARD TABLE OF TYP_W_UPMOD,

  BEGIN OF TYP_W_TABOUT,
    DATA TYPE STRING,
  END OF TYP_W_TABOUT,

  TYP_IT_TABOUT TYPE STANDARD TABLE OF TYP_W_TABOUT.

*&---------------------------------------------------------------------*
* 内表或全局变量声明
*&---------------------------------------------------------------------*
DATA:
  GIT_ERROR_OUT TYPE TYP_IT_ERROR_OUT,                      "error出力信息表
  GIT_ERROR     TYPE TYP_IT_ERROR,                          "error信息表（含message）
  GIT_UPLOAD    TYPE TYP_IT_SINNSOKU,                       "更新成功表
  GIT_UPMOD     TYPE TYP_IT_UPMOD,                          "UPMODE表
  GIT_BDCDATA   TYPE STANDARD TABLE OF BDCDATA,             "BDC使用
  GV_READ       TYPE I ,                                    "读取件数
  GV_UPLOAD     TYPE I ,                                    "上传件数
  GV_ERROR      TYPE I ,                                    "错误件数
  GV_WARN       TYPE I ,                                    "警告件数
  GV_OUT        TYPE I .                                    "基准外件数

*&---------------------------------------------------------------------*
* 常量声明
*&---------------------------------------------------------------------*
CONSTANTS:
  C_SEPARATOR TYPE C VALUE ','.

*&---------------------------------------------------------------------*
* 选择屏幕
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL01 WITH FRAME TITLE TEXT-S01.

SELECTION-SCREEN BEGIN OF LINE.                             "LINE
SELECTION-SCREEN POSITION 1.                                "起始位置
SELECTION-SCREEN COMMENT 1(14) TEXT-RB1.                    "ファイル入力元
SELECTION-SCREEN POSITION 33.
PARAMETERS RB_PC TYPE C RADIOBUTTON GROUP GP1 USER-COMMAND COM1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 35(16) TEXT-R01 FOR FIELD RB_PC.
SELECTION-SCREEN POSITION 53.
PARAMETERS RB_SV TYPE C RADIOBUTTON GROUP GP1.
SELECTION-SCREEN COMMENT 55(8) TEXT-R02 FOR FIELD RB_SV.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.                             "LINE
SELECTION-SCREEN POSITION 1.
SELECTION-SCREEN COMMENT 1(16) TEXT-RB2.                    "入力ファイル形式
SELECTION-SCREEN POSITION 33.
PARAMETERS RB_CSV TYPE C RADIOBUTTON GROUP GP2 USER-COMMAND COM2 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 35(3) TEXT-R03 FOR FIELD RB_CSV.
SELECTION-SCREEN POSITION 53.
PARAMETERS RB_TSV TYPE C RADIOBUTTON GROUP GP2.
SELECTION-SCREEN COMMENT 55(9) TEXT-R04 FOR FIELD RB_TSV.
SELECTION-SCREEN POSITION 73.
PARAMETERS RB_FIXED TYPE C RADIOBUTTON GROUP GP2.
SELECTION-SCREEN COMMENT 75(6) TEXT-R05 FOR FIELD RB_FIXED.
SELECTION-SCREEN END OF LINE.

PARAMETERS P_UPLOAD TYPE C LENGTH 128 .                      "Upload文件

SELECTION-SCREEN END OF BLOCK BL01.

SELECTION-SCREEN BEGIN OF BLOCK BL02 WITH FRAME TITLE TEXT-S02.

SELECTION-SCREEN BEGIN OF LINE.                             "LINE
SELECTION-SCREEN POSITION 1.
SELECTION-SCREEN COMMENT 1(16) TEXT-RB3.                    "エラーファイル出力先
SELECTION-SCREEN POSITION 33.
PARAMETERS RB_PC_E TYPE C RADIOBUTTON GROUP GP3 USER-COMMAND COM3 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 35(16) TEXT-R01 FOR FIELD RB_PC_E.
SELECTION-SCREEN POSITION 53.
PARAMETERS RB_SV_E TYPE C RADIOBUTTON GROUP GP3.
SELECTION-SCREEN COMMENT 55(8) TEXT-R02 FOR FIELD RB_SV_E.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.                             "LINE
SELECTION-SCREEN POSITION 1.
SELECTION-SCREEN COMMENT 1(16) TEXT-RB4.                    "エラーファイル形式
SELECTION-SCREEN POSITION 33.
PARAMETERS RB_CSV_E TYPE C RADIOBUTTON GROUP GP4 USER-COMMAND COM4 DEFAULT 'X'.  "CSV
SELECTION-SCREEN COMMENT 35(3) TEXT-R03 FOR FIELD RB_CSV_E.
SELECTION-SCREEN POSITION 53.
PARAMETERS RB_TSV_E TYPE C RADIOBUTTON GROUP GP4.           "TSV
SELECTION-SCREEN COMMENT 55(9) TEXT-R04 FOR FIELD RB_TSV_E.
SELECTION-SCREEN POSITION 73.
PARAMETERS RB_FIX_E TYPE C RADIOBUTTON GROUP GP4.           "固定长度
SELECTION-SCREEN COMMENT 75(6) TEXT-R05 FOR FIELD RB_FIX_E.
SELECTION-SCREEN END OF LINE.

PARAMETERS P_ERROR TYPE C LENGTH 128.                        "ERROR文件

SELECTION-SCREEN END OF BLOCK BL02.

SELECTION-SCREEN BEGIN OF BLOCK BL03 WITH FRAME TITLE TEXT-S03.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
SELECTION-SCREEN COMMENT 1(14) TEXT-OP1.
SELECTION-SCREEN POSITION 33.
PARAMETERS CB_OP1 TYPE C AS CHECKBOX DEFAULT 'X'.            "実行結果
SELECTION-SCREEN COMMENT 35(8) TEXT-CB1 FOR FIELD CB_OP1.
SELECTION-SCREEN POSITION 53.
PARAMETERS CB_OP2 TYPE C AS CHECKBOX DEFAULT 'X'.            "エラーリスト
SELECTION-SCREEN COMMENT 55(8) TEXT-CB2 FOR FIELD CB_OP2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
SELECTION-SCREEN COMMENT 1(8) TEXT-OP2.
SELECTION-SCREEN POSITION 33.
PARAMETERS CB_OP3 TYPE C AS CHECKBOX DEFAULT 'X'.            "テスト実行
SELECTION-SCREEN COMMENT 35(8) TEXT-CB3 FOR FIELD CB_OP3.

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BL03.

*&---------------------------------------------------------------------*
* 选择屏幕check
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.

*&---------------------------------------------------------------------*
* 主処理
*&---------------------------------------------------------------------*
START-OF-SELECTION.
* 主処理
  PERFORM F_MAIN_PROC.

*&---------------------------------------------------------------------*
* 主処理结束
*&---------------------------------------------------------------------*
END-OF-SELECTION.
* 下载错误文件
  PERFORM F_DL_DATA.
* WRITE 结果
  PERFORM F_WRITE_KEKKA.

*----------------------------------------------------------------------*
*SEARCH HELP
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_UPLOAD .
  PERFORM F_F4_FOR_UPLOAD.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_ERROR.
  PERFORM F_F4_FOR_ERROR.

*&---------------------------------------------------------------------*
*&      Form  F_MAIN_PROCESSING
*&---------------------------------------------------------------------*
*       主処理
*----------------------------------------------------------------------*
FORM F_MAIN_PROC.
  DATA:
    LIT_SINNSOKU TYPE TYP_IT_SINNSOKU.

* 数据下载
  PERFORM F_PC_UPLOAD CHANGING LIT_SINNSOKU .

* 数据校验
  PERFORM F_CHECK_DATA CHANGING LIT_SINNSOKU .
*
* BDC
  PERFORM F_BDC_DATA USING LIT_SINNSOKU.

ENDFORM.                    " F_MAIN_PROC
*&---------------------------------------------------------------------*
*&      Form  F_F4_FOR_UPLOAD
*&---------------------------------------------------------------------*
*       UPLOAD HELP
*----------------------------------------------------------------------*
FORM F_F4_FOR_UPLOAD .
  DATA:
    LIT_FIELNM TYPE FILETABLE ,
    LW_FIELNM  TYPE FILE_TABLE,
    LV_RETURN  TYPE I.

*・選択画面「上载文件」检索帮助的设定
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = '上传文件'
      FILE_FILTER             = 'CSV文件(*.CSV)|*.csv|Excel文件(*.xlsx)|*.xlsx'
      INITIAL_DIRECTORY       = 'D:\SAP_UP'
    CHANGING
      FILE_TABLE              = LIT_FIELNM
      RC                      = LV_RETURN
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.

  IF SY-SUBRC <> 0.
    MESSAGE ID     SY-MSGID
            TYPE   SY-MSGTY
            NUMBER SY-MSGNO
            WITH   SY-MSGV1
                   SY-MSGV2
                   SY-MSGV3
                   SY-MSGV4.
  ELSE.
    READ TABLE LIT_FIELNM INTO LW_FIELNM INDEX 1.
*   第一条设置成選択画面「上传文件」
    P_UPLOAD = LW_FIELNM-FILENAME.

  ENDIF.

ENDFORM.                    " F_F4_FOR_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  F_F4_FOR_ERROR
*&---------------------------------------------------------------------*
*       ERROR HELP
*----------------------------------------------------------------------*
FORM F_F4_FOR_ERROR .
  DATA:
    LV_FIELNAME   TYPE STRING,
    LV_FILENAME_1 TYPE STRING,
    LV_PATH       TYPE STRING,
    LV_FULLPATH   TYPE STRING.

  LV_FIELNAME = '教育／研修情報エラー' && '.CSV'.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
      WINDOW_TITLE         = 'ERROR文件'
      DEFAULT_FILE_NAME    = LV_FIELNAME
      INITIAL_DIRECTORY    = 'D:\SAP_DL'
    CHANGING
      FILENAME             = LV_FILENAME_1
      PATH                 = LV_PATH
      FULLPATH             = LV_FULLPATH
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID     SY-MSGID
            TYPE   SY-MSGTY
            NUMBER SY-MSGNO
            WITH   SY-MSGV1
                   SY-MSGV2
                   SY-MSGV3
                   SY-MSGV4.
  ELSE.

*  完整路径」保存到「ERROR文件」
    P_ERROR = LV_FULLPATH.

  ENDIF.

ENDFORM.                    " F_F4_FOR_ERROR
*&---------------------------------------------------------------------*
*&      Form  F_PC_UPLOAD
*&---------------------------------------------------------------------*
*       PC UPLOAD
*----------------------------------------------------------------------*
*      <--C_LIT_SINNSOKU  UPLOAD
*----------------------------------------------------------------------*
FORM F_PC_UPLOAD CHANGING C_LIT_SINNSOKU TYPE TYP_IT_SINNSOKU.
  DATA: LV_FILENAME TYPE RLGRAP-FILENAME,                   "function用
        LV_INDEX    TYPE I,                                 "序号
        LT_INTERN   TYPE KCDE_INTERN,                       "kcd
        LW_INTERN   TYPE KCDE_INTERN_STRUC,                 "kcd
        LW_SINNSOKU TYPE TYP_W_SINNSOKU.                    "工作区

  FIELD-SYMBOLS: <LFS_FIELD> TYPE ANY.

* 为路径赋值
  WRITE P_UPLOAD TO LV_FILENAME.

* 读取CSV文件
  CALL FUNCTION 'KCD_CSV_FILE_TO_INTERN_CONVERT'
    EXPORTING
      I_FILENAME      = LV_FILENAME
      I_SEPARATOR     = C_SEPARATOR
    TABLES
      E_INTERN        = LT_INTERN
    EXCEPTIONS
      UPLOAD_CSV      = 1
      UPLOAD_FILETYPE = 2
      OTHERS          = 3.

* 当读取不到路径时
  IF SY-SUBRC <> 0.

    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* 将读取的CSV内容转换保存到内表gt_input中
  LOOP AT LT_INTERN INTO LW_INTERN.

    MOVE : LW_INTERN-COL TO LV_INDEX.
    ASSIGN COMPONENT LV_INDEX OF STRUCTURE LW_SINNSOKU TO <LFS_FIELD>.
    MOVE : LW_INTERN-VALUE TO <LFS_FIELD> .

    AT END OF ROW.

*     更改更新表
      APPEND LW_SINNSOKU TO C_LIT_SINNSOKU.
      CLEAR LW_SINNSOKU.

    ENDAT.

  ENDLOOP.

ENDFORM.                    " F_PC_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_DATA
*&---------------------------------------------------------------------*
*       CHECK
*----------------------------------------------------------------------*
*      <--C_LIT_SINNSOKU  text
*----------------------------------------------------------------------*
FORM F_CHECK_DATA  CHANGING C_LIT_SINNSOKU TYPE TYP_IT_SINNSOKU.
  DATA:
    LW_SINNSOKU     TYPE TYP_W_SINNSOKU,                    "工作区*1
    LW_ERROR_OUT    TYPE TYP_W_ERROR_OUT,                   "工作区*2
*    LIT_SINNSOKU_SV TYPE TYP_IT_SINNSOKU,                   "备份
    LW_ERROR        TYPE TYP_W_ERROR,
    LW_UPMOD        TYPE TYP_W_UPMOD,
    LIT_PA0022      TYPE STANDARD TABLE OF PA0022,
    LIT_PA0001      TYPE STANDARD TABLE OF PA0001,
    LW_PA0022       LIKE LINE OF LIT_PA0022,
    LW_PA0001       LIKE LINE OF LIT_PA0001,
    LV_MSG          TYPE STRING.

  FIELD-SYMBOLS:
    <FS_ERROR> TYPE TYP_W_ERROR.

* 更新モード赋值
  LW_UPMOD-UPMOD = '1'.
  LW_UPMOD-MODENM = '登録'.

  APPEND LW_UPMOD TO GIT_UPMOD.

  LW_UPMOD-UPMOD = '2'.
  LW_UPMOD-MODENM = '変更'.
  APPEND LW_UPMOD TO GIT_UPMOD.

  LW_UPMOD-UPMOD = '3'.
  LW_UPMOD-MODENM = '削除'.

  APPEND LW_UPMOD TO GIT_UPMOD.
  LW_UPMOD-UPMOD = '4'.

  LW_UPMOD-MODENM = '期限設定'.
  APPEND LW_UPMOD TO GIT_UPMOD.

  CLEAR LW_UPMOD.

*关联check使用表  员工姓名等
  SELECT *
    FROM PA0022
    INTO TABLE LIT_PA0022.

*关联check使用表 公司相关（公司代码 在职否）
  SELECT *
    FROM PA0001
    INTO TABLE LIT_PA0001.

  IF C_LIT_SINNSOKU IS NOT INITIAL.
* 排序
    SORT C_LIT_SINNSOKU
      BY PERNR ASCENDING                                    "从业员号 升序
         BEGDA ASCENDING.                                   "开始日期 升序

  ENDIF.

*为title赋值
  CLEAR LW_SINNSOKU.
  READ TABLE C_LIT_SINNSOKU INTO LW_SINNSOKU INDEX 1.

  MOVE-CORRESPONDING LW_SINNSOKU TO LW_ERROR.
  APPEND LW_ERROR TO GIT_ERROR.
  CLEAR LW_ERROR.
  
  
  IF CB_OP2 IS NOT INITIAL .
    
* 为error表赋值
    LOOP AT C_LIT_SINNSOKU INTO LW_SINNSOKU FROM 2.
*  更新mode check
      READ TABLE GIT_UPMOD INTO LW_UPMOD                      "UPMOD CHECK
        WITH KEY UPMOD = LW_SINNSOKU-UPMOD.

      IF SY-SUBRC <> 0 .                                      "1-4 以外

        MOVE-CORRESPONDING LW_SINNSOKU TO LW_ERROR.

        IF LW_SINNSOKU-UPMOD IS INITIAL .

          LW_ERROR-MSGID = 'ZX999'.
          LW_ERROR-MSGTYP = 'E'.
          LW_ERROR-MSGNR = '008'.

        ELSE.

          LW_ERROR-MSGID = 'ZX999'.
          LW_ERROR-MSGTYP = 'E'.
          LW_ERROR-MSGNR = '003'.

        ENDIF.

        APPEND LW_ERROR TO GIT_ERROR.                         "ERROR 出力表添加
        CLEAR  LW_ERROR.                                      "清楚工作区域
        DELETE C_LIT_SINNSOKU.                                "删除当前更新条目

        CONTINUE.

      ENDIF.

*   必输项check
      IF LW_SINNSOKU-UPMOD IS INITIAL                         "当更新模式为空时

        OR LW_SINNSOKU-BUKRS IS INITIAL                       "当公司代码为空时
        OR LW_SINNSOKU-PERNR IS INITIAL                       "当从业员代码为空时
        OR LW_SINNSOKU-BEGDA IS INITIAL                       "当开始日期为空时
        OR LW_SINNSOKU-ENDDA IS INITIAL                       "当结束日为空时
        OR LW_SINNSOKU-SLART IS INITIAL.                      "当教育机关cod为空时

        MOVE-CORRESPONDING LW_SINNSOKU TO LW_ERROR.           "添加到工作区
        LW_ERROR-MSGID = 'ZX999'.
        LW_ERROR-MSGTYP = 'E'.
        LW_ERROR-MSGNR = '008'.
        APPEND LW_ERROR TO GIT_ERROR.                         "向出力表中添加
        CLEAR  LW_ERROR.                                      "清空工作区
        DELETE C_LIT_SINNSOKU.                                "删除当前条

        CONTINUE.

      ENDIF.

*   基准外判定
      READ TABLE LIT_PA0001 INTO LW_PA0001 WITH KEY PERNR = LW_SINNSOKU-PERNR .

      IF LW_PA0001-PERSG = '8'.                             "退职者

        GV_OUT = GV_OUT + 1.
        DELETE C_LIT_SINNSOKU.

        CONTINUE.

      ENDIF.

*   当 更新模式为 登录时
      IF LW_SINNSOKU-UPMOD = '1' .

        READ TABLE LIT_PA0022 INTO LW_PA0022
          WITH KEY PERNR = LW_SINNSOKU-PERNR                  "从业员号
                   SLART = LW_SINNSOKU-SLART.                 "机关cod

        IF SY-DATUM < LW_PA0022-BEGDA.

          MOVE-CORRESPONDING LW_SINNSOKU TO LW_ERROR.
*          编辑error信息。
          LW_ERROR-MSGID = 'ZX999'.
          LW_ERROR-MSGTYP = 'E'.
          LW_ERROR-MSGNR = '004'.
          APPEND LW_ERROR TO GIT_ERROR.
          CLEAR  LW_ERROR.
          DELETE C_LIT_SINNSOKU.

          CONTINUE.

        ENDIF.

      ENDIF.

*   当 更新模式为更新时
      IF LW_SINNSOKU-UPMOD = '2'.

        READ TABLE LIT_PA0022 INTO LW_PA0022
          WITH KEY PERNR = LW_SINNSOKU-PERNR
                   BEGDA = LW_SINNSOKU-BEGDA
                   ENDDA = LW_SINNSOKU-ENDDA
                   SLART = LW_SINNSOKU-SLART.

        IF SY-SUBRC <> 0.

          MOVE-CORRESPONDING LW_SINNSOKU TO LW_ERROR.

          LW_ERROR-MSGID = 'ZX999'.
          LW_ERROR-MSGTYP = 'E'.
          LW_ERROR-MSGNR = '005'.

          APPEND LW_ERROR TO GIT_ERROR.
          CLEAR  LW_ERROR.
          DELETE C_LIT_SINNSOKU.
          CONTINUE.

        ENDIF.

      ENDIF.

*   当 更新模式为消除时
      IF LW_SINNSOKU-UPMOD = '3'.

        READ TABLE LIT_PA0022 INTO LW_PA0022
          WITH KEY PERNR = LW_SINNSOKU-PERNR
                   BEGDA = LW_SINNSOKU-BEGDA
                   ENDDA = LW_SINNSOKU-ENDDA
                   SLART = LW_SINNSOKU-SLART.

        IF SY-SUBRC <> 0.

          MOVE-CORRESPONDING LW_SINNSOKU TO LW_ERROR.

          LW_ERROR-MSGID = 'ZX999'.
          LW_ERROR-MSGTYP = 'E'.
          LW_ERROR-MSGNR = '006'.

          APPEND LW_ERROR TO GIT_ERROR.
          CLEAR  LW_ERROR.
          DELETE C_LIT_SINNSOKU.

          CONTINUE.

        ENDIF.

      ENDIF.

*   当 更新模式为期间限定时
      IF LW_SINNSOKU-UPMOD = '4'.

        READ TABLE LIT_PA0022 INTO LW_PA0022
          WITH KEY PERNR = LW_SINNSOKU-PERNR
                   BEGDA = LW_SINNSOKU-BEGDA
                   SLART = LW_SINNSOKU-SLART.

        IF SY-SUBRC <> 0.

          MOVE-CORRESPONDING LW_SINNSOKU TO LW_ERROR.
          LW_ERROR-MSGID = 'ZX999'.
          LW_ERROR-MSGTYP = 'E'.
          LW_ERROR-MSGNR = '007'.
          APPEND LW_ERROR TO GIT_ERROR.
          CLEAR  LW_ERROR.
          DELETE C_LIT_SINNSOKU.
          CONTINUE.

        ENDIF.

      ENDIF.

    ENDLOOP.

*  IF GIT_ERROR_OUT IS NOT INITIAL .
*
*    LOOP AT GIT_ERROR_OUT INTO LW_ERROR_OUT.
*
*      MOVE-CORRESPONDING LW_ERROR_OUT TO LW_ERROR.
*      APPEND LW_ERROR TO GIT_ERROR.
*      CLEAR LW_ERROR.
*
*    ENDLOOP.
*
*  ENDIF.

* 修改error表中的 message信息

  LOOP AT GIT_ERROR ASSIGNING <FS_ERROR> FROM 2.

    READ TABLE GIT_UPMOD INTO LW_UPMOD WITH KEY
      UPMOD = <FS_ERROR>-UPMOD.

    IF SY-SUBRC <> 0.

      MESSAGE ID       <FS_ERROR>-MSGID
                TYPE   <FS_ERROR>-MSGTYP
                NUMBER <FS_ERROR>-MSGNR    INTO LV_MSG .

      <FS_ERROR>-MESSAGE = '更新モード' && LV_MSG .

    ENDIF.


    IF <FS_ERROR>-UPMOD IS INITIAL .
      MESSAGE ID <FS_ERROR>-MSGID
            TYPE <FS_ERROR>-MSGTYP
          NUMBER <FS_ERROR>-MSGNR INTO LV_MSG .

      <FS_ERROR>-MESSAGE = '更新モード' && LV_MSG .

    ENDIF.

    IF <FS_ERROR>-BUKRS IS INITIAL .

      MESSAGE ID <FS_ERROR>-MSGID
            TYPE <FS_ERROR>-MSGTYP
          NUMBER <FS_ERROR>-MSGNR INTO LV_MSG .

      <FS_ERROR>-MESSAGE = '会社コード' && LV_MSG && <FS_ERROR>-MESSAGE .

    ENDIF.

    IF <FS_ERROR>-PERNR IS INITIAL .

      MESSAGE ID <FS_ERROR>-MSGID
            TYPE <FS_ERROR>-MSGTYP
          NUMBER <FS_ERROR>-MSGNR INTO LV_MSG.

      <FS_ERROR>-MESSAGE = '従業員番号' && LV_MSG && <FS_ERROR>-MESSAGE .

    ENDIF.

    IF <FS_ERROR>-BEGDA IS INITIAL .

      MESSAGE ID <FS_ERROR>-MSGID
            TYPE <FS_ERROR>-MSGTYP
          NUMBER <FS_ERROR>-MSGNR INTO LV_MSG.

      <FS_ERROR>-MESSAGE = '有効開始日' && LV_MSG && <FS_ERROR>-MESSAGE .

    ENDIF.

    IF <FS_ERROR>-ENDDA IS INITIAL .
      MESSAGE ID <FS_ERROR>-MSGID
            TYPE <FS_ERROR>-MSGTYP
          NUMBER <FS_ERROR>-MSGNR INTO LV_MSG.

      <FS_ERROR>-MESSAGE = '有効終了日' && LV_MSG && <FS_ERROR>-MESSAGE .

*      <FS_ERROR>-MESSAGE = '有効終了日が未入力です,'&& <FS_ERROR>-MESSAGE.
    ENDIF.

    IF <FS_ERROR>-SLART IS INITIAL .

      MESSAGE ID <FS_ERROR>-MSGID
            TYPE <FS_ERROR>-MSGTYP
          NUMBER <FS_ERROR>-MSGNR INTO LV_MSG.

      <FS_ERROR>-MESSAGE = '教育機関コード' && LV_MSG && <FS_ERROR>-MESSAGE .

*      <FS_ERROR>-MESSAGE = '教育機関コードが未入力です,'&& <FS_ERROR>-MESSAGE.

    ENDIF.

*    CASE SPACE.
*      WHEN <FS_ERROR>-UPMOD.
*        <FS_ERROR>-MESSAGE = '更新モード'.
*      WHEN <FS_ERROR>-BUKRS.
*        <FS_ERROR>-MESSAGE = '会社コードが未入力です,' && <FS_ERROR>-MESSAGE.
*      WHEN <FS_ERROR>-PERNR.
*        <FS_ERROR>-MESSAGE = '従業員番号が未入力です,' && <FS_ERROR>-MESSAGE .
*      WHEN <FS_ERROR>-BEGDA.
*        <FS_ERROR>-MESSAGE = '有効開始日が未入力です,' && <FS_ERROR>-MESSAGE .
*      WHEN <FS_ERROR>-ENDDA.
*        <FS_ERROR>-MESSAGE = '有効終了日が未入力です,' && <FS_ERROR>-MESSAGE .
*      WHEN <FS_ERROR>-SLART.
*        <FS_ERROR>-MESSAGE = '教育機関コードが未入力です,' && <FS_ERROR>-MESSAGE .
*      WHEN OTHERS.
*
*    ENDCASE.

*   当 更新模式为 登录时
    IF <FS_ERROR>-UPMOD = '1' .

      READ TABLE LIT_PA0022 INTO LW_PA0022
        WITH KEY PERNR = <FS_ERROR>-PERNR
                 SLART = <FS_ERROR>-SLART.

      IF SY-DATUM < LW_PA0022-BEGDA.

        <FS_ERROR>-MSGID   = 'ZX999'.
        <FS_ERROR>-MSGTYP = 'E'.
        <FS_ERROR>-MSGNR   = '004'.

        MESSAGE ID <FS_ERROR>-MSGID
              TYPE <FS_ERROR>-MSGTYP
            NUMBER <FS_ERROR>-MSGNR INTO LV_MSG.

        <FS_ERROR>-MESSAGE = LV_MSG && <FS_ERROR>-MESSAGE.

      ENDIF.

    ENDIF.

*   当 更新模式为 更新时
    IF <FS_ERROR>-UPMOD = '2' .

      READ TABLE LIT_PA0022 INTO LW_PA0022
        WITH KEY PERNR = <FS_ERROR>-PERNR
                 BEGDA = <FS_ERROR>-BEGDA
                 ENDDA = <FS_ERROR>-ENDDA
                 SLART = <FS_ERROR>-SLART.

      IF SY-SUBRC <> 0.

        <FS_ERROR>-MSGID   = 'ZX999'.
        <FS_ERROR>-MSGTYP = 'E'.
        <FS_ERROR>-MSGNR   = '005'.

        MESSAGE ID <FS_ERROR>-MSGID
              TYPE <FS_ERROR>-MSGTYP
            NUMBER <FS_ERROR>-MSGNR INTO LV_MSG.

        <FS_ERROR>-MESSAGE = LV_MSG && <FS_ERROR>-MESSAGE.

      ENDIF.

    ENDIF.

*   当 更新模式为 消除时
    IF <FS_ERROR>-UPMOD = '3' .

      READ TABLE LIT_PA0022 INTO LW_PA0022
        WITH KEY PERNR = <FS_ERROR>-PERNR
                 BEGDA = <FS_ERROR>-BEGDA
                 ENDDA = <FS_ERROR>-ENDDA
                 SLART = <FS_ERROR>-SLART.

      IF SY-SUBRC <> 0.

        <FS_ERROR>-MSGID   = 'ZX999'.
        <FS_ERROR>-MSGTYP = 'E'.
        <FS_ERROR>-MSGNR   = '006'.

        MESSAGE ID <FS_ERROR>-MSGID
              TYPE <FS_ERROR>-MSGTYP
            NUMBER <FS_ERROR>-MSGNR INTO LV_MSG.

        <FS_ERROR>-MESSAGE = LV_MSG && <FS_ERROR>-MESSAGE .

      ENDIF.

    ENDIF.

*   当 更新模式为 期间设定时
    IF <FS_ERROR>-UPMOD = '4' .

      READ TABLE LIT_PA0022 INTO LW_PA0022
        WITH KEY PERNR = <FS_ERROR>-PERNR
                 BEGDA = <FS_ERROR>-BEGDA
                 SLART = <FS_ERROR>-SLART.

      IF SY-SUBRC <> 0.
        <FS_ERROR>-MSGID   = 'ZX999'.
        <FS_ERROR>-MSGTYP = 'E'.
        <FS_ERROR>-MSGNR   = '007'.

        MESSAGE ID <FS_ERROR>-MSGID
              TYPE <FS_ERROR>-MSGTYP
            NUMBER <FS_ERROR>-MSGNR INTO LV_MSG.


        <FS_ERROR>-MESSAGE = LV_MSG && <FS_ERROR>-MESSAGE .

      ENDIF.

    ENDIF.
    
    ENDLOOP. 
       
  ENDIF.

ENDFORM.                    " F_CHECK_DATA

*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       一括登录
*----------------------------------------------------------------------*
*      -->U_LIT_SINNSOKU
*----------------------------------------------------------------------*
FORM F_BDC_DATA  USING    U_LIT_SINNSOKU TYPE TYP_IT_SINNSOKU.
  DATA:
    LW_ERROR  TYPE TYP_W_ERROR,                             "error工作区
    LW_ERROR_OUT  TYPE TYP_W_ERROR_OUT,                     "error出力工作区
    LW_SINNSOKU  TYPE TYP_W_SINNSOKU,                       "更新工作区
    LW_SINNSOKU_UP  TYPE TYP_W_SINNSOKU,                       "更新工作区
    LIT_MESSTAB  TYPE STANDARD TABLE OF BDCMSGCOLL,         "返回消息
    LW_MESSTAB   TYPE BDCMSGCOLL,                           "返回消息
    LW_PARAMS    TYPE CTU_PARAMS,                           "BDC参数设置
    LV_MSG       TYPE C LENGTH 200.

  LW_PARAMS-DISMODE = 'N'.                                "后台执行 不显示
  LW_PARAMS-UPDMODE = 'A'.                                "同步
  LW_PARAMS-DEFSIZE = ABAP_TRUE.

  LOOP AT U_LIT_SINNSOKU INTO LW_SINNSOKU FROM 2.

    IF LW_SINNSOKU-UPMOD = '1'.                             "当更新模式为登录时.

      PERFORM BDC_DYNPRO      USING 'SAPMP50A' '1000'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=INS'.
      PERFORM BDC_FIELD       USING 'RP50G-PERNR'
                                    LW_SINNSOKU-PERNR.
      PERFORM BDC_FIELD       USING 'RP50G-CHOIC'
                                    '0022'.
      PERFORM BDC_DYNPRO      USING 'MP002200' '2000'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'FACHTXT2'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=UPD'.
      PERFORM BDC_FIELD       USING 'P0022-BEGDA'
                                    LW_SINNSOKU-BEGDA.
      PERFORM BDC_FIELD       USING 'P0022-ENDDA'
                                    LW_SINNSOKU-ENDDA.
      PERFORM BDC_FIELD       USING 'P0022-SLART'
                                    LW_SINNSOKU-SLART.
      PERFORM BDC_FIELD       USING 'P0022-AUSBI'
                                    LW_SINNSOKU-AUSBI.
      PERFORM BDC_FIELD       USING 'P0022-INSTI'
                                    LW_SINNSOKU-INSTI.
      PERFORM BDC_FIELD       USING 'P0022-SLAND'
                                    LW_SINNSOKU-SLAND.
      PERFORM BDC_FIELD       USING 'P0022-SLABS'
                                    LW_SINNSOKU-SLABS.
      PERFORM BDC_FIELD       USING 'P0022-EMARK'
                                    LW_SINNSOKU-EMARK.
      PERFORM BDC_FIELD       USING 'P0022-SLTP1'
                                    LW_SINNSOKU-SLTP1.

    ENDIF.

    IF LW_SINNSOKU-UPMOD = '2'.                             "当更新模式为更新时.

      PERFORM BDC_DYNPRO      USING 'SAPMP50A' '1000'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=MOD'.
      PERFORM BDC_FIELD       USING 'RP50G-PERNR'
                                    LW_SINNSOKU-PERNR.
      PERFORM BDC_FIELD       USING 'RP50G-CHOIC'
                                    '0022'.
      PERFORM BDC_DYNPRO      USING 'MP002200' '2000'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'P0022-BEGDA'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=UPD'.
      PERFORM BDC_FIELD       USING 'P0022-BEGDA'
                                    LW_SINNSOKU-BEGDA.
      PERFORM BDC_FIELD       USING 'P0022-ENDDA'
                                    LW_SINNSOKU-ENDDA.
      PERFORM BDC_FIELD       USING 'P0022-SLART'
                                    LW_SINNSOKU-SLART.
      PERFORM BDC_FIELD       USING 'P0022-AUSBI'
                                    LW_SINNSOKU-AUSBI.
      PERFORM BDC_FIELD       USING 'P0022-INSTI'
                                    LW_SINNSOKU-INSTI.
      PERFORM BDC_FIELD       USING 'P0022-SLAND'
                                    LW_SINNSOKU-SLAND.
      PERFORM BDC_FIELD       USING 'P0022-SLABS'
                                    LW_SINNSOKU-SLABS.
      PERFORM BDC_FIELD       USING 'P0022-EMARK'
                                    LW_SINNSOKU-EMARK.
      PERFORM BDC_FIELD       USING 'P0022-SLTP1'
                                    LW_SINNSOKU-SLTP1.

    ENDIF.

    IF LW_SINNSOKU-UPMOD = '3'.                             "当更新模式为消除时.

      PERFORM BDC_DYNPRO      USING 'SAPMP50A' '1000'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=DEL'.
      PERFORM BDC_FIELD       USING 'RP50G-PERNR'
                                    LW_SINNSOKU-PERNR.
      PERFORM BDC_FIELD       USING 'RP50G-CHOIC'
                                    '0022'.
      PERFORM BDC_DYNPRO      USING 'MP002200' '2000'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'P0022-BEGDA'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=UPDL'.
      PERFORM BDC_FIELD       USING 'P0022-BEGDA'
                                    LW_SINNSOKU-BEGDA.
      PERFORM BDC_FIELD       USING 'P0022-ENDDA'
                                    LW_SINNSOKU-ENDDA.

      PERFORM BDC_DYNPRO      USING 'SAPMP50A' '1000'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=DEL'.
      PERFORM BDC_FIELD       USING 'RP50G-PERNR'
                                    LW_SINNSOKU-PERNR.
      PERFORM BDC_FIELD       USING 'RP50G-BEGDA'
                                    LW_SINNSOKU-BEGDA.
      PERFORM BDC_FIELD       USING 'RP50G-ENDDA'
                                    LW_SINNSOKU-ENDDA.
      PERFORM BDC_DYNPRO      USING 'MP002200' '2000'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'P0022-BEGDA'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=UPDL'.
      PERFORM BDC_FIELD       USING 'P0022-BEGDA'
                                    LW_SINNSOKU-BEGDA.
      PERFORM BDC_FIELD       USING 'P0022-ENDDA'
                                    LW_SINNSOKU-ENDDA.
      PERFORM BDC_DYNPRO      USING 'MP002200' '2000'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '/E'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'P0022-BEGDA'.

    ENDIF.

    IF LW_SINNSOKU-UPMOD = '4'.                             "当更新模式为期间限定时.

      PERFORM BDC_DYNPRO      USING 'SAPMP50A' '1000'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=LIS9'.
      PERFORM BDC_FIELD       USING 'RP50G-PERNR'
                                    LW_SINNSOKU-PERNR.
      PERFORM BDC_FIELD       USING 'RP50G-CHOIC'
                                    '0022'.
      PERFORM BDC_DYNPRO      USING 'SAPMP50A' '0500'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'RP50M-ABGRD'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=TAKE'.
      PERFORM BDC_FIELD       USING 'RP50M-ABGRD'
                                     SY-DATUM.

    ENDIF.

    CALL TRANSACTION 'PA30'
      USING GIT_BDCDATA                                     "BDC数据（BDDCDATA）
      OPTIONS FROM   LW_PARAMS                              " 'N'.
      MESSAGES INTO  LIT_MESSTAB.                           " 'A'.

*   3.判断执行结果
    IF SY-SUBRC <> 0.

      MOVE-CORRESPONDING LW_SINNSOKU TO LW_ERROR.

      LOOP AT LIT_MESSTAB INTO LW_MESSTAB.
*        WHERE MSGTYP = 'W'                                  "警告
*           OR MSGTYP = 'E'.                                 "错误

        CLEAR LV_MSG.

        MESSAGE ID     LW_MESSTAB-MSGID
                TYPE   LW_MESSTAB-MSGTYP
                NUMBER LW_MESSTAB-MSGNR
                WITH   LW_MESSTAB-MSGV1
                       LW_MESSTAB-MSGV2
                       LW_MESSTAB-MSGV3
                       LW_MESSTAB-MSGV4 INTO LV_MSG.

        LW_ERROR-MESSAGE = LW_ERROR-MESSAGE && LV_MSG.

        LW_ERROR-MSGID  = LW_MESSTAB-MSGID.
        LW_ERROR-MSGTYP = LW_MESSTAB-MSGTYP.

      ENDLOOP.

      APPEND LW_ERROR TO GIT_ERROR.                         "BDC出错添加去error表
      CLEAR:LW_ERROR.

    ELSE .

      MOVE-CORRESPONDING LW_SINNSOKU TO LW_SINNSOKU_UP.
      APPEND LW_SINNSOKU_UP TO GIT_UPLOAD.                  "更新成功表
      CLEAR: LW_SINNSOKU_UP.
      GV_UPLOAD = GV_UPLOAD + 1.

    ENDIF.

    LOOP AT LIT_MESSTAB INTO LW_MESSTAB  .

      IF LW_MESSTAB-MSGTYP = 'W'.
        GV_WARN = GV_WARN + 1.

      ENDIF.

    ENDLOOP.

    CLEAR: LW_SINNSOKU , GIT_BDCDATA , LIT_MESSTAB.

  ENDLOOP.

  LOOP AT GIT_ERROR INTO LW_ERROR FROM 2.

    MOVE-CORRESPONDING LW_ERROR TO LW_ERROR_OUT.
    APPEND LW_ERROR_OUT TO GIT_ERROR_OUT.
    CLEAR LW_ERROR_OUT.

  ENDLOOP.

ENDFORM.                    " F_BDC_DATA
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO
  USING U_V_PROGRAM TYPE BDC_PROG
        U_V_DYNPRO  TYPE BDC_DYNR.

  DATA:
    LW_BDCDATA TYPE BDCDATA.

  LW_BDCDATA-PROGRAM  = U_V_PROGRAM.
  LW_BDCDATA-DYNPRO   = U_V_DYNPRO.
  LW_BDCDATA-DYNBEGIN = 'X'.
  APPEND LW_BDCDATA TO GIT_BDCDATA.

ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD
  USING U_V_FNAM TYPE FNAM_____4
        U_V_FVAL TYPE ANY.

  DATA:
    LW_BDCDATA TYPE BDCDATA.

  LW_BDCDATA-FNAM = U_V_FNAM.
  LW_BDCDATA-FVAL = U_V_FVAL.
  APPEND LW_BDCDATA TO GIT_BDCDATA.

ENDFORM.                    "BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  F_DL_DATA
*&---------------------------------------------------------------------*
*       error文件下载
*----------------------------------------------------------------------*
FORM F_DL_DATA .

  DATA:
    LV_ERROR  TYPE STRING,
    IT_TABOUT TYPE TYP_IT_TABOUT,
    LW_ERROR  TYPE TYP_W_ERROR,
    LW_TABOUT TYPE TYP_W_TABOUT.

  LV_ERROR = P_ERROR.

  CLEAR IT_TABOUT.

*    LW_TABOUT-DATA =  '更新モード' && ',' &&
*      '会社コード'&& ',' &&
*      '従業員番号'&& ',' &&
*      '有効開始日'&& ',' &&
*      '有効終了日'&& ',' &&
*      '国コード'&& ',' &&
*      '教育修了コード'&& ',' &&
*      '教育機関コード'&& ',' &&
*      '教育機関名'&& ',' &&
*      '学部'&& ',' &&
*      '学科'&& ',' &&
*      '専攻１'&& ',' &&
*      '卒業区分'&& ',' &&
*      '最終学歴'&& '/' .

    LW_TABOUT-DATA =  'A' &&  ',' &&
    'B' && ',' &&
    'C' && ',' &&
    'D' && ',' &&
    'E' && ',' &&
    'F' && ',' &&
    'G' && ',' &&
    'H' && ',' &&
    'I' && ',' &&
    'J' && ',' &&
    'K' && ',' &&
    'L' && ',' &&
    'M' && ',' &&
    'N'.

  APPEND LW_TABOUT TO IT_TABOUT.
  CLEAR LW_TABOUT.

  LOOP AT GIT_ERROR INTO LW_ERROR FROM 2.

    CONCATENATE
      LW_ERROR-UPMOD
      LW_ERROR-BUKRS
      LW_ERROR-PERNR
      LW_ERROR-BEGDA
      LW_ERROR-ENDDA
      LW_ERROR-SLAND
      LW_ERROR-AUSBI
      LW_ERROR-SLART
      LW_ERROR-INSTI
      LW_ERROR-FACCD
      LW_ERROR-DPTMT
      LW_ERROR-SLTP1
      LW_ERROR-SLABS
      LW_ERROR-EMARK

    INTO LW_TABOUT-DATA SEPARATED BY ','.

    APPEND LW_TABOUT TO IT_TABOUT.
    CLEAR LW_TABOUT.

  ENDLOOP.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD
    EXPORTING
*     bin_filesize            =
      FILENAME                = LV_ERROR
**                filetype                = 'DAT'
*     codepage                = '8404'
*     ignore_cerr             = ABAP_TRUE
*     replacement             = '#'
    CHANGING
      DATA_TAB                = IT_TABOUT
    EXCEPTIONS
      FILE_WRITE_ERROR        = 1
      NO_BATCH                = 2
      GUI_REFUSE_FILETRANSFER = 3
      INVALID_TYPE            = 4
      NO_AUTHORITY            = 5
      UNKNOWN_ERROR           = 6
      HEADER_NOT_ALLOWED      = 7
      SEPARATOR_NOT_ALLOWED   = 8
      FILESIZE_NOT_ALLOWED    = 9
      HEADER_TOO_LONG         = 10
      DP_ERROR_CREATE         = 11
      DP_ERROR_SEND           = 12
      DP_ERROR_WRITE          = 13
      UNKNOWN_DP_ERROR        = 14
      ACCESS_DENIED           = 15
      DP_OUT_OF_MEMORY        = 16
      DISK_FULL               = 17
      DP_TIMEOUT              = 18
      FILE_NOT_FOUND          = 19
      DATAPROVIDER_EXCEPTION  = 20
      CONTROL_FLUSH_ERROR     = 21
      NOT_SUPPORTED_BY_GUI    = 22
      ERROR_NO_GUI            = 23
      OTHERS                  = 24.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " F_DL_DATA
*&---------------------------------------------------------------------*
*&      Form  F_WRITE_KEKKA
*&---------------------------------------------------------------------*
FORM F_WRITE_KEKKA .
  DATA:
      LIT_PA0001      TYPE STANDARD TABLE OF PA0001,
      LW_PA0001       LIKE LINE OF LIT_PA0001,
      LV_TAB          TYPE I.

  SELECT *
    FROM PA0001
  INTO TABLE LIT_PA0001.

  DATA:
    LW_UPLOAD TYPE TYP_W_SINNSOKU,
    LW_ERROR TYPE TYP_W_ERROR.

  GV_ERROR = LINES( GIT_ERROR ) - 1.

  WRITE:  '【件数リスト】',
        / ,
        /'読込件数           :',   GV_READ,   '件',
        /'正常件数           :',   GV_UPLOAD, '件',
        /'警告件数           :',   GV_WARN,   '件',
        /'異常件数           :',   GV_ERROR,  '件',
        /'処理対象外件数     :',   GV_OUT,    '件',/,/.

  IF CB_OP1 IS NOT INITIAL AND CB_OP2 IS NOT INITIAL.

    WRITE: '【実行結果詳細】',
          /,
          'Seq',13'更新モード',25'会社コード',38'従業員番号',53'氏名',95'国コード',105'教育修了コード',
          123'教育機関コード',138'学部',148'専攻１',158'卒業区分',168'最終学歴',178'有効開始日',190'有効終了日',
          / SY-ULINE.

    LOOP AT GIT_UPLOAD INTO LW_UPLOAD.

      LV_TAB = SY-TABIX.
      CLEAR : LW_PA0001.
      READ TABLE LIT_PA0001 INTO LW_PA0001 WITH KEY PERNR = LW_UPLOAD-PERNR.

      WRITE:/,LV_TAB UNDER 'Seq' ,LW_UPLOAD-UPMOD UNDER '更新モード', LW_UPLOAD-PERNR UNDER '従業員番号',LW_UPLOAD-SLAND UNDER '国コード',LW_UPLOAD-AUSBI UNDER '教育修了コード',
            LW_UPLOAD-SLART UNDER '教育機関コード', LW_UPLOAD-FACCD UNDER '学部',LW_UPLOAD-DPTMT UNDER '専攻１',LW_UPLOAD-SLABS UNDER '卒業区分' ,LW_UPLOAD-EMARK UNDER'最終学歴',
            LW_UPLOAD-BEGDA UNDER '有効開始日',LW_UPLOAD-ENDDA UNDER '有効終了日',LW_PA0001-ENAME UNDER '氏名',LW_PA0001-BUKRS UNDER '会社コード'.

      CLEAR LW_UPLOAD.
    ENDLOOP.

    WRITE:/,/ '【エラーリスト】',
        /,
        'Seq',13'更新モード',25'会社コード',38'従業員番号',53'氏名',95'国コード',105'教育修了コード',
        123'教育機関コード',138'学部',148'専攻１',158'卒業区分',168'最終学歴',178'有効開始日',190'有効終了日', 205'メッセージタイプ',220'メッセージID',250 'エラー内容',
        / SY-ULINE.

    LOOP AT GIT_ERROR INTO LW_ERROR FROM 2.

      LV_TAB = SY-TABIX - 1.
      CLEAR: LW_PA0001.
      READ TABLE LIT_PA0001 INTO LW_PA0001 WITH KEY PERNR = LW_ERROR-PERNR.

      WRITE:/,LV_TAB UNDER 'Seq' ,LW_ERROR-UPMOD UNDER '更新モード', LW_ERROR-PERNR UNDER '従業員番号',LW_ERROR-SLAND UNDER '国コード',LW_ERROR-AUSBI UNDER '教育修了コード',
            LW_ERROR-SLART UNDER '教育機関コード', LW_ERROR-FACCD UNDER '学部',LW_ERROR-DPTMT UNDER '専攻１',LW_ERROR-SLABS UNDER '卒業区分' ,LW_ERROR-EMARK UNDER'最終学歴',
            LW_ERROR-BEGDA UNDER '有効開始日',LW_ERROR-ENDDA UNDER '有効終了日',LW_ERROR-MESSAGE UNDER 'エラー内容',LW_PA0001-ENAME UNDER '氏名',LW_PA0001-BUKRS UNDER '会社コード',
            LW_ERROR-MSGTYP UNDER 'メッセージタイプ' ,LW_ERROR-MSGID UNDER 'メッセージID'.

      CLEAR LW_ERROR.

    ENDLOOP.
    
  ENDIF.

ENDFORM.                    "F_WRITE_KEKKA
