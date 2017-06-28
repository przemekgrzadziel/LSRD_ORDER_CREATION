class /LSRD/CL_A_CREATE_SUPP_ORDER definition
  public
  inheriting from /LSRD/CL_LAYER_A
  create public .

public section.

  types:
    tt_gernr TYPE STANDARD TABLE OF gernr .
  types:
    BEGIN OF t_select_param,
        protid   TYPE /lsrd/e_protocol_id,
        prtdsc   TYPE text75,
        kunnr    TYPE /lsrd/e_trial_kunnr,
        bpartner TYPE bu_partner,
        po_nr    TYPE bstkd,
        po_typ   TYPE bsark,
        ref      TYPE ihrez,
        vkorg    TYPE vkorg,
        vtweg    TYPE vtweg,
        spart    TYPE spart,
        medcnt   TYPE i,
        sernr    TYPE ranges_sernr,
      END OF t_select_param .
  types:
    BEGIN OF t_equi,
        equnr  TYPE equnr,
        matnr  TYPE matnr,
        sernr  TYPE gernr,
        wrk    TYPE werks_d,
        lgort  TYPE lgort_d,
        charge TYPE charg_d,
      END OF t_equi .
  types:
    tt_equi TYPE SORTED TABLE OF t_equi WITH UNIQUE KEY equnr .
  types:
    BEGIN OF t_mara,
        matnr TYPE matnr,
        meins TYPE meins,
      END OF t_mara .
  types:
    tt_mara TYPE SORTED TABLE OF t_mara WITH UNIQUE KEY matnr .
  types:
    BEGIN OF t_mat_serno,
        equnr TYPE equnr,
        matnr TYPE matnr,
        sernr TYPE gernr,
      END OF t_mat_serno .
  types:
    tt_mat_serno TYPE SORTED TABLE OF t_mat_serno WITH UNIQUE KEY equnr .
  types:
    BEGIN OF t_marc,
        matnr TYPE matnr,
        werks TYPE werks_d,
        sernp TYPE serail,
      END OF t_marc .
  types:
    tt_marc TYPE SORTED TABLE OF t_marc WITH UNIQUE KEY matnr werks .

  constants CV_MAIN_CONTROLLER type STRING value '/LSRD/CL_A_CREATE_SUPP_ORDER' ##NO_TEXT.
  constants CV_MAIN_VIEW type STRING value '/LSRD/CL_U_CREATE_SUPP_ORDER' ##NO_TEXT.
  constants CV_MAIN_MODEL type STRING value '/LSRD/CL_D_CREATE_SUPP_ORDER' ##NO_TEXT.
  constants CV_SIGN type DDSIGN value 'I' ##NO_TEXT.
  constants CV_OPTION_EQ type DDOPTION value 'EQ' ##NO_TEXT.
  constants CV_OPTION_BT type DDOPTION value 'BT' ##NO_TEXT.
  constants MC_DOC_TYPE_ZCFG type AUART value 'ZLS1' ##NO_TEXT.
  constants CV_PARTN_ROLE_SP type PARVW value 'AG' ##NO_TEXT.
  constants CV_PARTN_ROLE_WE type PARVW value 'WE' ##NO_TEXT.
  constants MC_MSGTY_E type MSGTY value 'E' ##NO_TEXT.
  constants MC_LOG_OBJECT type BALOBJ_D value '/LSRD/' ##NO_TEXT.
  constants MC_LOG_SUBOBJECT type BALSUBOBJ value '/LSRD/CTM' ##NO_TEXT.
  constants MC_LOG_ACTIVITY_CREATE type BU_AKTYP value '01' ##NO_TEXT.
  constants MC_MSGID_LSRD_04 type SYMSGID value '/LSRD/04' ##NO_TEXT.
  constants MC_MSGTY_S type SYMSGTY value 'S' ##NO_TEXT.
  constants MC_MSGNO_10 type SYMSGNO value '010' ##NO_TEXT.

  methods START_OF_SELECTION
    for event START_OF_SELECTION of /LSRD/CL_U_CREATE_SUPP_ORDER
    importing
      !IV_PROTID
      !IV_PRTDSC
      !IV_KUNNR
      !IV_BPARTN
      !IV_PO_NR
      !IV_PO_TYP
      !IV_REF
      !IV_VKORG
      !IV_VTWEG
      !IV_SPART
      !IV_MEDCNT
      !IT_SERNR .
  methods END_OF_SELECTION
    for event END_OF_SELECTION of /LSRD/CL_U_CREATE_SUPP_ORDER .
  methods CONSTRUCTOR
    importing
      !IV_REPID type SYREPID
      !IV_DYNNR type SYDYNNR .

  methods /LSRD/IF_LAYER_A~CREATE_MODEL
    redefinition .
protected section.
private section.

  data MS_SELECT_PARAM type T_SELECT_PARAM .
  data MO_MODEL type ref to /LSRD/CL_D_CREATE_SUPP_ORDER .
  data MT_EQUI type TT_EQUI .
  data MT_MAT_TAB type /LSRD/T_CTM_HCM .
  data MT_MARA type TT_MARA .
  data MO_LOG type ref to /LSRD/CL_LOG .
  data MT_MAT_SERNO type TT_MAT_SERNO .
  data MS_BP_CUST_LINK type CVI_CUST_LINK .

  methods CHECK_SERIAL_STATUS
    importing
      !IV_EQUNR type EQUNR
    raising
      CX_DO_SERIALIZED .
  methods GET_SINGLE_BP_CUST_LINK
    returning
      value(RS_BP_CUST_LINK) type CVI_CUST_LINK .
  methods CALL_METHOD_ADD_TO_AU
    importing
      !IV_PROTID type /LSRD/E_PROTOCOL_ID
      !IS_VBAK type VBAK
      !IS_VBAP type VBAP
      !IS_MARC type T_MARC
      !IS_MAT_SERNO type T_MAT_SERNO
    raising
      /LSRD/CX_ERROR .
  methods RAISE_COMMIT_SERIALS .
  methods SERNR_ADD_TO_AU
    importing
      !IV_PROTID type /LSRD/E_PROTOCOL_ID
      !IS_VBAK type VBAK
      !IS_VBAP type VBAP
      !IS_MARC type T_MARC
    raising
      /LSRD/CX_ERROR .
  methods GET_DATA_FOR_SERNO_UPDATE
    importing
      !IV_VBELN type VBELN
    exporting
      !ES_VBAK type VBAK
      !ET_VBAP type VBAP_T
    raising
      /LSRD/CX_ERROR .
  methods PROCESS_UPDATE_SERIAL_NUMBERS
    importing
      !IV_UPDATE_SERNO type XFELD
      !IV_VBELN type VBELN .
  methods STORE_MATERIAL_AND_SERIAL
    importing
      !IV_EQUNR type EQUNR
      !IV_MATRN type MATNR
      !IV_SERNR type GERNR .
  methods UPDATE_SERIAL_NO
    importing
      !IV_PROTID type /LSRD/E_PROTOCOL_ID
      !IS_VBAK type VBAK
      !IT_VBAP type VBAP_T
    raising
      /LSRD/CX_ERROR .
  methods BAPI_SALESORDER_CREATEFROMDAT2
    importing
      !IS_HEADER_IN type BAPISDHD1
      !IS_HEADER_INX type BAPISDHD1X
    exporting
      !EV_VBELN type VBELN
    changing
      !CT_ITEMS_IN type OIL_BAPISDIM_T
      !CT_ITEMS_INX type OIL_BAPISDIMX_T
      !CT_PARTNERS type OIJ_BAPIPARNR_T
      !CT_SCHEDULES_IN type OIJ_BAPISCHDL_T
      !CT_SCHEDULES_INX type OIJ_BAPISCHDLX_T
      !CT_RETURN type BAPIRET2TAB .
  methods CHECK_FOR_COMMIT
    importing
      !IT_RETURN type BAPIRET2TAB
    returning
      value(RV_UPDATE_SERNO) type XFELD .
  methods FILL_SCHEDULES
    importing
      !IV_ITEM type POSNR_VA
    changing
      !CT_SCHEDULES_IN type OIJ_BAPISCHDL_T
      !CT_SCHEDULES_INX type OIJ_BAPISCHDLX_T .
  methods CONVERSION_EXIT_CUNIT_INPUT
    importing
      !IV_INPUT type MEINS
      !IV_SPRAS type SPRAS default SY-LANGU
    exporting
      !EV_OUTPUT type DZIEME .
  methods READ_TABLES_WITH_MATERIAL_DATA
    importing
      !IV_MATRN type MATNR
    exporting
      !ES_MARA type T_MARA
      !ES_EQUI type T_EQUI
    raising
      /LSRD/CX_ERROR .
  methods FILL_BAPI_HEADER_IN
    importing
      !IV_DOC_TYPE type AUART
      !IV_SALES_ORG type VKORG
      !IV_DISTR_CHAN type VTWEG
      !IV_DIVISION type SPART
      !IV_PRICE_DATE type PRSDT
      !IV_PO_METHOD type BSARK
    exporting
      !ES_HEADER_IN type BAPISDHD1
      !ES_HEADER_INX type BAPISDHD1X .
  methods FILL_BAPI_PARTNER_ROLES
    importing
      !IS_BP_CUST_LINK type CVI_CUST_LINK
    exporting
      !ET_PARTNERS type OIJ_BAPIPARNR_T .
  methods FILL_ITEMS
    importing
      !IV_ITEM type POSNR_VA
      !IS_MAT_TAB type /LSRD/CTM_HCM
    changing
      !CT_ITEMS_IN type OIL_BAPISDIM_T
      !CT_ITEMS_INX type OIL_BAPISDIMX_T
    raising
      /LSRD/CX_ERROR .
  methods PROCESS_READ_AND_CHECK_DATA
    raising
      /LSRD/CX_ERROR .
  methods PROCESS_READ_DATA
    exceptions
      /LSRD/CX_ERROR .
  methods FILL_BAPI_INPUT
    exporting
      !ES_HEADER_IN type BAPISDHD1
      !ES_HEADER_INX type BAPISDHD1X
      !ET_RETURN type BAPIRET2TAB
      !ET_ITEMS_IN type OIL_BAPISDIM_T
      !ET_ITEMS_INX type OIL_BAPISDIMX_T
      !ET_PARTNERS type OIJ_BAPIPARNR_T
      !ET_SCHEDULES_IN type OIJ_BAPISCHDL_T
      !ET_SCHEDULES_INX type OIJ_BAPISCHDLX_T
    raising
      /LSRD/CX_ERROR .
  methods PROCESS_CREATE_SALES_ORDER .
  methods PASS_SEL_SCREEN_PARAM
    importing
      !IV_PROTID type /LSRD/E_PROTOCOL_ID
      !IV_PRTDSC type TEXT75
      !IV_KUNNR type /LSRD/E_TRIAL_KUNNR
      !IV_BPARTN type BU_PARTNER
      !IV_PO_NR type BSTKD
      !IV_PO_TYP type BSARK
      !IV_REF type IHREZ
      !IV_VKORG type VKORG
      !IV_VTWEG type VTWEG
      !IV_SPART type SPART
      !IV_MEDCNT type I
      !IT_SERNR type RANGES_SERNR .
  methods CONVERSION_EXIT_MATN1_INPUT
    importing
      !IV_INPUT type ANY
    exporting
      !EV_OUTPUT type ANY .
  methods PASS_ERRORS_TO_LOG
    importing
      !IV_VBELN type VBELN
      !IV_PROTID type /LSRD/E_PROTOCOL_ID
      !IT_RETURN type BAPIRET2TAB .
  methods DISPLAY_LOG .
ENDCLASS.



CLASS /LSRD/CL_A_CREATE_SUPP_ORDER IMPLEMENTATION.


  METHOD /lsrd/if_layer_a~create_model.
*Create Model Instance
    me->mo_model ?= /lsrd/cl_layer_d=>create_model( iv_model_ref = me->cv_main_model ).
  ENDMETHOD.


  METHOD bapi_salesorder_createfromdat2.
    CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
      EXPORTING
        order_header_in     = is_header_in
        order_header_inx    = is_header_inx
      IMPORTING
        salesdocument       = ev_vbeln
      TABLES
        return              = ct_return[]
        order_items_in      = ct_items_in[]
        order_items_inx     = ct_items_inx[]
        order_partners      = ct_partners[]
        order_schedules_in  = ct_schedules_in
        order_schedules_inx = ct_schedules_inx.
  ENDMETHOD.


  METHOD call_method_add_to_au.
    DATA: lv_debitor       TYPE kunnr,
          lv_anzsn         TYPE anzsn,
          lv_zeilen_id     TYPE msgzeile,
          lv_serial_commit TYPE c.
*--------------------------------------------------------------------*
    lv_debitor = me->ms_bp_cust_link-customer."iv_protid.
    CALL FUNCTION 'SERNR_ADD_TO_AU'
      EXPORTING
        sernr                 = is_mat_serno-sernr
        profile               = is_marc-sernp
        material              = is_vbap-matnr
        m_charge              = is_vbap-charg
        quantity              = 1
        document              = is_vbap-vbeln
        item                  = is_vbap-posnr
        debitor               = lv_debitor
        vbtyp                 = is_vbak-vbtyp
        sd_auart              = is_vbak-auart
        sd_postyp             = is_vbap-pstyv
        i_more_allowed        = abap_true
      IMPORTING
        anzsn                 = lv_anzsn
        zeilen_id             = lv_zeilen_id
        serial_commit         = lv_serial_commit
      EXCEPTIONS
        konfigurations_error  = 1
        serialnumber_errors   = 2
        serialnumber_warnings = 3
        no_profile_operation  = 4
        OTHERS                = 5.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /lsrd/cx_error.
    ENDIF.
  ENDMETHOD.


  METHOD check_for_commit.
    DATA: ls_return TYPE bapiret2.
*--------------------------------------------------------------------*
    READ TABLE it_return[] TRANSPORTING NO FIELDS WITH KEY type = me->mc_msgty_e.
    IF sy-subrc = 0.
* rollback changes
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
* confirm changes
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
* set information to update
      rv_update_serno = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD check_serial_status.
    DATA: lv_user_status TYPE asttx.
*--------------------------------------------------------------------*
    me->mo_model->bapi_equi_getstatus( EXPORTING iv_equipment  = iv_equnr
                                       IMPORTING ev_userstatus = lv_user_status ).
    IF lv_user_status NS TEXT-001.
      RAISE EXCEPTION TYPE cx_do_serialized.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    DATA: lv_extnumber TYPE  balnrext.
*--------------------------------------------------------------------*
    super->constructor( EXPORTING iv_repid = iv_repid
                                  iv_dynnr = iv_dynnr ).

*create log instance
    mo_log = NEW #( ).
    mo_log->init( id_object    = me->mc_log_object
                  id_subobject = me->mc_log_subobject
                  id_extnumber = lv_extnumber
                  id_activity  = me->mc_log_activity_create ).

  ENDMETHOD.


  METHOD conversion_exit_cunit_input.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = iv_input
        language       = iv_spras
      IMPORTING
        output         = ev_output
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDMETHOD.


  METHOD conversion_exit_matn1_input.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = iv_input
      IMPORTING
        output = ev_output.
  ENDMETHOD.


  METHOD display_log.
    mo_log->display( i_delete_duplicate = abap_false ).
  ENDMETHOD.


  METHOD end_of_selection.

    me->process_create_sales_order( ).

    me->display_log( ).
  ENDMETHOD.


  METHOD fill_bapi_header_in.
    es_header_in-doc_type   = iv_doc_type.
    es_header_in-sales_org  = iv_sales_org.
    es_header_in-distr_chan = iv_distr_chan.
    es_header_in-division   = iv_division.
    es_header_in-price_date = iv_price_date.
    es_header_in-po_method  = iv_po_method.

    es_header_inx-doc_type   = abap_true.
    es_header_inx-sales_org  = abap_true.
    es_header_inx-distr_chan = abap_true.
    es_header_inx-division   = abap_true.
    es_header_inx-price_date = abap_true.
    es_header_inx-po_method  = abap_true.
  ENDMETHOD.


  METHOD fill_bapi_input.
    DATA: ls_mat_tab TYPE /lsrd/ctm_hcm,
          lv_item    TYPE posnr_va VALUE '000010'.
*--------------------------------------------------------------------*
    fill_bapi_header_in( EXPORTING iv_doc_type   = me->mc_doc_type_zcfg
                                   iv_sales_org  = me->ms_select_param-vkorg
                                   iv_distr_chan = me->ms_select_param-vtweg
                                   iv_division   = me->ms_select_param-spart
                                   iv_price_date = sy-datum
                                   iv_po_method  = me->ms_select_param-po_typ
                         IMPORTING es_header_in  = es_header_in
                                   es_header_inx = es_header_inx ).

    fill_bapi_partner_roles( EXPORTING is_bp_cust_link = me->ms_bp_cust_link
                             IMPORTING et_partners     = et_partners[] ).


    LOOP AT me->mt_mat_tab[] INTO ls_mat_tab.
      TRY.
          fill_items( EXPORTING iv_item      = lv_item
                                is_mat_tab   = ls_mat_tab
                      CHANGING  ct_items_in  = et_items_in[]
                                ct_items_inx = et_items_inx[] ).

          fill_schedules( EXPORTING iv_item          = lv_item
                          CHANGING  ct_schedules_in  = et_schedules_in[]
                                    ct_schedules_inx = et_schedules_inx[] ).
* add 10 to item
          ADD 10 TO lv_item.
        CATCH /lsrd/cx_error.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_bapi_partner_roles.
    DATA: ls_partners TYPE bapiparnr.
*--------------------------------------------------------------------*
    ls_partners-partn_role = me->cv_partn_role_sp. "Sold-to party
    ls_partners-partn_numb = is_bp_cust_link-customer."me->ms_select_param-protid.
    APPEND ls_partners TO et_partners[].
    CLEAR: ls_partners.

    ls_partners-partn_role = me->cv_partn_role_we. "Ship-to party
* check we should use BP from selection screen or link from CVI_CUST_LINK
    IF is_bp_cust_link-customer IS NOT INITIAL.
      ls_partners-partn_numb = is_bp_cust_link-customer.
    ELSE.
      ls_partners-partn_numb = me->ms_select_param-bpartner.
    ENDIF.
    APPEND ls_partners TO et_partners[].
    CLEAR: ls_partners.
  ENDMETHOD.


  METHOD fill_items.
    DATA: ls_items_in  TYPE bapisditm,
          ls_items_inx TYPE bapisditmx,
          ls_mara      TYPE t_mara,
          ls_equi      TYPE t_equi.
*--------------------------------------------------------------------*
    me->read_tables_with_material_data( EXPORTING iv_matrn = is_mat_tab-matnr
                                        IMPORTING es_mara  = ls_mara
                                                  es_equi  = ls_equi ).

    ls_items_in-itm_number = iv_item.
    me->conversion_exit_matn1_input( EXPORTING iv_input  = is_mat_tab-matnr
                                      IMPORTING ev_output = ls_items_in-material ).

    ls_items_in-batch      = ls_equi-charge.
    ls_items_in-plant      = ls_equi-wrk.
    ls_items_in-store_loc  = ls_equi-lgort.
    ls_items_in-target_qty = 1.
* unit conversion
    me->conversion_exit_cunit_input( EXPORTING iv_input  = ls_mara-meins
                                     IMPORTING ev_output = ls_items_in-target_qu ).

    APPEND ls_items_in TO ct_items_in[].

*=============XXX==========================================
    ls_items_inx-itm_number = ls_items_in-itm_number.
    ls_items_inx-material   = abap_true.
    IF ls_items_in-batch IS NOT INITIAL.
      ls_items_inx-batch    = abap_true.
    ENDIF.
    ls_items_inx-plant      = abap_true.
    ls_items_inx-store_loc  = abap_true.
    ls_items_inx-target_qty = abap_true.
    ls_items_inx-target_qu  = abap_true.
    APPEND ls_items_inx TO ct_items_inx[].
  ENDMETHOD.


  METHOD fill_schedules.
    DATA: ls_schedules_in  TYPE bapischdl,
          ls_schedules_inx TYPE bapischdlx.
*--------------------------------------------------------------------*
    ls_schedules_in-itm_number = iv_item.
    ls_schedules_in-sched_line = 1.
    ls_schedules_in-req_qty = 1.
    APPEND ls_schedules_in TO ct_schedules_in[].

    ls_schedules_inx-itm_number = iv_item.
    ls_schedules_inx-sched_line = 1.
    ls_schedules_inx-req_qty = abap_true.
    APPEND ls_schedules_inx TO ct_schedules_inx[].
  ENDMETHOD.


  METHOD get_data_for_serno_update.

    me->mo_model->read_single_sd_header_data( EXPORTING iv_vbeln = iv_vbeln
                                              IMPORTING es_vbak  = es_vbak ).

    me->mo_model->read_sd_item_data( EXPORTING iv_vbeln = iv_vbeln
                                     CHANGING  ct_vbap  = et_vbap[] ).

  ENDMETHOD.


  METHOD get_single_bp_cust_link.
    DATA: lt_bp_cust_link TYPE cvis_cust_link_t.
*--------------------------------------------------------------------*
    lt_bp_cust_link[] = me->mo_model->get_bp_cust_link( ).
    IF lt_bp_cust_link[] IS NOT INITIAL.
      READ TABLE lt_bp_cust_link INTO rs_bp_cust_link INDEX 1.
    ENDIF.
  ENDMETHOD.


  METHOD pass_errors_to_log.
    me->mo_log->if_reca_message_list~add_from_bapi( EXPORTING it_bapiret = it_return[] ).
  ENDMETHOD.


  METHOD pass_sel_screen_param.
    me->ms_select_param-protid   = iv_protid.
    me->ms_select_param-prtdsc   = iv_prtdsc.
    me->ms_select_param-kunnr    = iv_kunnr.
    me->ms_select_param-bpartner = iv_bpartn.
    me->ms_select_param-po_nr    = iv_po_nr.
    me->ms_select_param-po_typ   = iv_po_typ.
    me->ms_select_param-ref      = iv_ref.
    me->ms_select_param-vkorg    = iv_vkorg.
    me->ms_select_param-vtweg    = iv_vtweg.
    me->ms_select_param-spart    = iv_spart.
    me->ms_select_param-medcnt   = iv_medcnt.
    me->ms_select_param-sernr[]  = it_sernr[].
  ENDMETHOD.


  METHOD process_create_sales_order.
    DATA: ls_header_in     TYPE bapisdhd1,
          ls_header_inx    TYPE bapisdhd1x,
          lt_return        TYPE bapiret2tab,
          lt_items_in      TYPE oil_bapisdim_t,
          lt_items_inx     TYPE oil_bapisdimx_t,
          lt_partners      TYPE oij_bapiparnr_t,
          lt_schedules_in  TYPE oij_bapischdl_t,
          lt_schedules_inx TYPE oij_bapischdlx_t,
          lv_vbeln         TYPE vbeln,
          lv_update_serno  TYPE xfeld.
*--------------------------------------------------------------------*
    TRY.
        me->mt_equi[] = me->mo_model->get_equipment_master_data( ).

        me->mt_mat_tab[] = me->mo_model->get_ctm_hcm_data( ).

        me->mt_mara[] = me->mo_model->get_materail_master_data( ).

        me->ms_bp_cust_link = me->get_single_bp_cust_link( ).

        me->fill_bapi_input( IMPORTING es_header_in     = ls_header_in
                                       es_header_inx    = ls_header_inx
                                       et_return        = lt_return[]
                                       et_items_in      = lt_items_in[]
                                       et_items_inx     = lt_items_inx[]
                                       et_partners      = lt_partners[]
                                       et_schedules_in  = lt_schedules_in[]
                                       et_schedules_inx = lt_schedules_inx[] ).

        me->bapi_salesorder_createfromdat2( EXPORTING is_header_in     = ls_header_in
                                                      is_header_inx    = ls_header_inx
                                            IMPORTING ev_vbeln         = lv_vbeln
                                            CHANGING  ct_return        = lt_return[]
                                                      ct_items_in      = lt_items_in[]
                                                      ct_items_inx     = lt_items_inx[]
                                                      ct_partners      = lt_partners[]
                                                      ct_schedules_in  = lt_schedules_in[]
                                                      ct_schedules_inx = lt_schedules_inx[] ).

        lv_update_serno = me->check_for_commit( lt_return[] ).


        process_update_serial_numbers( EXPORTING iv_update_serno = lv_update_serno
                                                 iv_vbeln        = lv_vbeln ).

        me->pass_errors_to_log( EXPORTING iv_vbeln  = lv_vbeln
                                          iv_protid = me->ms_select_param-protid
                                          it_return = lt_return[] ).

      CATCH /lsrd/cx_error.
    ENDTRY.
  ENDMETHOD.


  METHOD process_read_and_check_data.

* get materials from equipment master data using serial number from selection screen
   me->process_read_data( ).



  ENDMETHOD.


  METHOD process_read_data.

    me->mo_model->process_read_data( me->ms_select_param ).

  ENDMETHOD.


  METHOD process_update_serial_numbers.
    DATA: ls_vbak TYPE vbak,
          lt_vbap TYPE vbap_t.
*--------------------------------------------------------------------*
    CHECK iv_update_serno = abap_true.
    TRY.
        me->get_data_for_serno_update( EXPORTING iv_vbeln = iv_vbeln
                                       IMPORTING es_vbak  = ls_vbak
                                                 et_vbap  = lt_vbap[] ).

        me->update_serial_no( EXPORTING iv_protid = me->ms_select_param-protid
                                        is_vbak   = ls_vbak
                                        it_vbap   = lt_vbap[] ).

        me->raise_commit_serials( ).

      CATCH /lsrd/cx_error.
    ENDTRY.
  ENDMETHOD.


  METHOD raise_commit_serials.
    CALL FUNCTION 'SERIAL_LISTE_POST_AU'.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ENDMETHOD.


  METHOD read_tables_with_material_data.
* get data from mara
    READ TABLE me->mt_mara[] INTO es_mara WITH TABLE KEY matnr = iv_matrn.

* read info from equi table
    READ TABLE me->mt_equi[] INTO es_equi WITH KEY matnr = iv_matrn.
    IF sy-subrc = 0.
* connect material and serial no in one table
      me->store_material_and_serial( EXPORTING iv_equnr = es_equi-equnr
                                               iv_matrn = iv_matrn
                                               iv_sernr = es_equi-sernr ).
* remove element - to avoid reuse element
      DELETE TABLE me->mt_equi[] WITH TABLE KEY equnr = es_equi-equnr.
    ELSE.
      RAISE EXCEPTION TYPE /lsrd/cx_error.
    ENDIF.
  ENDMETHOD.


  METHOD sernr_add_to_au.
    DATA: ls_mat_serno TYPE t_mat_serno.
*--------------------------------------------------------------------*
    LOOP AT me->mt_mat_serno[] INTO ls_mat_serno WHERE matnr = is_vbap-matnr.
      TRY.
          me->check_serial_status( ls_mat_serno-equnr ).

          me->call_method_add_to_au( EXPORTING iv_protid    = iv_protid
                                               is_vbak      = is_vbak
                                               is_vbap      = is_vbap
                                               is_marc      = is_marc
                                               is_mat_serno = ls_mat_serno ).
        CATCH cx_do_serialized.
          me->mo_log->add( id_msgty = me->mc_msgty_e
                           id_msgid = me->mc_msgid_lsrd_04
                           id_msgno = me->mc_msgno_10
                           id_msgv1 = ls_mat_serno-sernr ).
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD start_of_selection.
* save imported data
    me->pass_sel_screen_param( iv_protid = iv_protid
                               iv_prtdsc = iv_prtdsc
                               iv_kunnr  = iv_kunnr
                               iv_bpartn = iv_bpartn
                               iv_po_nr  = iv_po_nr
                               iv_po_typ = iv_po_typ
                               iv_ref    = iv_ref
                               iv_vkorg  = iv_vkorg
                               iv_vtweg  = iv_vtweg
                               iv_spart  = iv_spart
                               iv_medcnt = iv_medcnt
                               it_sernr  = it_sernr[] ).

* Add in future  selection part- when someone provides more information
    me->process_read_and_check_data( ).

  ENDMETHOD.


  METHOD store_material_and_serial.
    DATA: ls_mat_serno TYPE t_mat_serno.
*--------------------------------------------------------------------*
    ls_mat_serno-equnr = iv_equnr.
    ls_mat_serno-matnr = iv_matrn.
    ls_mat_serno-sernr = iv_sernr.
    INSERT ls_mat_serno INTO TABLE me->mt_mat_serno[].
  ENDMETHOD.


  METHOD update_serial_no.
    DATA: lr_vbap  TYPE REF TO vbap,
          lv_sernp TYPE serail,
          ls_marc  TYPE t_marc.
*--------------------------------------------------------------------*
    LOOP AT it_vbap[] REFERENCE INTO lr_vbap.

      ls_marc = me->mo_model->get_single_marc( iv_matnr = lr_vbap->matnr
                                               iv_werks = lr_vbap->werks ).

      me->sernr_add_to_au( EXPORTING iv_protid = iv_protid
                                     is_vbak   = is_vbak
                                     is_vbap   = lr_vbap->*
                                     is_marc   = ls_marc ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
