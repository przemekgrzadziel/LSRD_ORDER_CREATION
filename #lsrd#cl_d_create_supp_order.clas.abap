class /LSRD/CL_D_CREATE_SUPP_ORDER definition
  public
  inheriting from /LSRD/CL_LAYER_D
  create public .

public section.

  methods PROCESS_READ_DATA
    importing
      !IS_SELECT_PARAM type /LSRD/CL_A_CREATE_SUPP_ORDER=>T_SELECT_PARAM
    raising
      /LSRD/CX_ERROR .
  methods GET_EQUIPMENT_MASTER_DATA
    returning
      value(RT_EQUI) type /LSRD/CL_A_CREATE_SUPP_ORDER=>TT_EQUI .
  methods GET_CTM_HCM_DATA
    returning
      value(RT_MAT_TAB) type /LSRD/T_CTM_HCM .
  methods GET_MATERAIL_MASTER_DATA
    returning
      value(RT_MARA) type /LSRD/CL_A_CREATE_SUPP_ORDER=>TT_MARA .
  methods READ_SINGLE_SD_HEADER_DATA
    importing
      !IV_VBELN type VBELN
    exporting
      !ES_VBAK type VBAK
    raising
      /LSRD/CX_ERROR .
  methods READ_SD_ITEM_DATA
    importing
      !IV_VBELN type VBELN
    changing
      !CT_VBAP type VBAP_T
    raising
      /LSRD/CX_ERROR .
  methods GET_PLANT_DATA_FOR_MATNR
    returning
      value(RT_MARC) type /LSRD/CL_A_CREATE_SUPP_ORDER=>TT_MARC .
  methods GET_SINGLE_MARC
    importing
      !IV_MATNR type MATNR
      !IV_WERKS type WERKS_D
    returning
      value(RS_MARC) type /LSRD/CL_A_CREATE_SUPP_ORDER=>T_MARC .
  methods GET_BP_CUST_LINK
    returning
      value(RT_BP_CUST_LINK) type CVIS_CUST_LINK_T .
  methods BAPI_EQUI_GETSTATUS
    importing
      !IV_EQUIPMENT type EQUNR
      !IV_LANGUAGE type SPRAS default SY-LANGU
      !IV_LANGUAGE_ISO type LAISO optional
    exporting
      !EV_SYSTEMSTATUS type J_STEXT
      !EV_USERSTATUS type ASTTX
      !ES_RETURN type BAPIRET2
    changing
      !CT_SYSTEM_STATUS type BAPI_ITOB_STATUS_TAB optional
      !CT_USER_STATUS type BAPI_ITOB_STATUS_TAB optional
    raising
      /LSRD/CX_ERROR .
protected section.
private section.

  data MS_SELECT_PARAM type /LSRD/CL_A_CREATE_SUPP_ORDER=>T_SELECT_PARAM .
  data MT_EQUI type /LSRD/CL_A_CREATE_SUPP_ORDER=>TT_EQUI .
  data MT_MAT_TAB type /LSRD/T_CTM_HCM .
  data MT_MARA type /LSRD/CL_A_CREATE_SUPP_ORDER=>TT_MARA .
  data MT_MARC type /LSRD/CL_A_CREATE_SUPP_ORDER=>TT_MARC .
  data MT_BP_CUST_LINK type CVIS_CUST_LINK_T .

  methods PASS_PARTNER_ID
    changing
      !CT_PARTNER_IDS type BU_PARTNER_T .
  methods PROCESS_READ_CUSTOMER_DATA .
  methods READ_PLANT_DATA_FOR_MATNR .
  methods PASS_SEL_SCREEN_PARAM
    importing
      !IS_SELECT_PARAM type /LSRD/CL_A_CREATE_SUPP_ORDER=>T_SELECT_PARAM
    raising
      /LSRD/CX_ERROR .
  methods READ_DATA
    raising
      /LSRD/CX_ERROR .
  methods READ_EQUIPMENT_MASTER_DATA .
  methods READ_MATERIALS_BY_PROTOCOL_ID .
  methods READ_MATERIAL_MASTER .
  methods READ_CUSTOMER_DATA
    importing
      !IT_PARTNER_IDS type BU_PARTNER_T .
ENDCLASS.



CLASS /LSRD/CL_D_CREATE_SUPP_ORDER IMPLEMENTATION.


  METHOD bapi_equi_getstatus.
    CALL FUNCTION 'BAPI_EQUI_GETSTATUS'
      EXPORTING
        equipment     = iv_equipment
        language      = iv_language
        language_iso  = iv_language_iso
      IMPORTING
        systemstatus  = ev_systemstatus
        userstatus    = ev_userstatus
        return        = es_return
      TABLES
        system_status = ct_system_status[]
        user_status   = ct_user_status[].
  ENDMETHOD.


  METHOD get_bp_cust_link.
    rt_bp_cust_link[] = me->mt_bp_cust_link[].
  ENDMETHOD.


  METHOD get_ctm_hcm_data.
    rt_mat_tab[] = me->mt_mat_tab[].
  ENDMETHOD.


  METHOD get_equipment_master_data.
    rt_equi[] = me->mt_equi[].
  ENDMETHOD.


  METHOD GET_MATERAIL_MASTER_DATA.
    rt_mara[] = me->mt_mara[].
  ENDMETHOD.


  METHOD get_plant_data_for_matnr.
    rt_marc[] = me->mt_marc[].
  ENDMETHOD.


  METHOD get_single_marc.
    READ TABLE me->mt_marc[]
          INTO rs_marc
          WITH TABLE KEY matnr = iv_matnr
                         werks = iv_werks.
  ENDMETHOD.


  METHOD pass_partner_id.
    DATA: ls_partner_id TYPE bupa_partner.
*--------------------------------------------------------------------*
    ls_partner_id-partner = me->ms_select_param-bpartner.
    APPEND ls_partner_id TO ct_partner_ids[].
  ENDMETHOD.


  METHOD pass_sel_screen_param.
    me->ms_select_param = is_select_param.
  ENDMETHOD.


  METHOD process_read_customer_data.
    DATA: lt_partner_ids TYPE bu_partner_t.
*--------------------------------------------------------------------*
    me->pass_partner_id( CHANGING ct_partner_ids = lt_partner_ids[] ).

    me->read_customer_data( lt_partner_ids[] ).
  ENDMETHOD.


  METHOD process_read_data.

* save incomming values
    me->pass_sel_screen_param( is_select_param ).

* read data
    me->read_data( ).

  ENDMETHOD.


  METHOD read_customer_data.
    DATA: lo_cvi_mapper TYPE REF TO cvi_mapper.
*--------------------------------------------------------------------*
    lo_cvi_mapper = cvi_mapper=>get_instance( ).

    me->mt_bp_cust_link[] = lo_cvi_mapper->get_assigned_customers_for_bps( i_partner_ids = it_partner_ids[] ).

  ENDMETHOD.


  METHOD read_data.
*material
    me->read_materials_by_protocol_id( ).

*Equipment master data
    me->read_equipment_master_data( ).

*read data from material master
    me->read_material_master( ).

* read Plant data for material
    me->read_plant_data_for_matnr( ).

* using partner get customer data
    me->process_read_customer_data( ).

  ENDMETHOD.


  METHOD read_equipment_master_data.
* contine reading when me->mt_mat_tab[] is empty
*    SELECT equnr matnr sernr
*           werk lager charge
*      FROM equi
*      INTO TABLE me->mt_equi[]
*       FOR ALL ENTRIES IN me->mt_mat_tab[]
*     WHERE sernr IN me->ms_select_param-sernr[]
*       AND matnr = me->mt_mat_tab-matnr.
*
*    SELECT equnr matnr sernr
*           werk lager charge
*      FROM equi
*      INTO TABLE me->mt_equi[]
*      FOR ALL ENTRIES IN me->mt_mat_tab[]
*       WHERE  matnr = me->mt_mat_tab-matnr.


* read equi using selected material
    SELECT DISTINCT a~equnr
           a~matnr a~sernr
           b~b_werk AS werk
           b~b_lager AS lager
           a~charge
*             a~sequence
*             c~vfdat
      FROM equi AS a
      JOIN eqbs AS b
        ON b~equnr = a~equnr
      JOIN mch1 AS c
        ON c~matnr  = a~matnr
       AND c~charg  = a~charge
*      JOIN marc AS d
*        ON d~matnr = a~matnr
      INTO TABLE me->mt_equi[]
      FOR ALL ENTRIES IN me->mt_mat_tab[]
     WHERE a~matnr = me->mt_mat_tab-matnr.
*      AND c~vfdat >= sy-datum.
*      AND d~sernp <> space.
  ENDMETHOD.


  METHOD read_materials_by_protocol_id.
    CALL METHOD /lsrd/cl_d_ctm_if_out=>get_materials_by_protocol_id
      EXPORTING
        iv_protocol_id = me->ms_select_param-protid
        iv_valid_only  = abap_true
      IMPORTING
        et_mat_tab     = me->mt_mat_tab[].
  ENDMETHOD.


  METHOD READ_MATERIAL_MASTER.
    SELECT matnr meins
      FROM mara
      INTO TABLE me->mt_mara[]
       FOR ALL ENTRIES IN me->mt_mat_tab[]
     WHERE matnr = me->mt_mat_tab-matnr.
  ENDMETHOD.


  METHOD read_plant_data_for_matnr.
    SELECT matnr werks sernp
      FROM marc
      INTO TABLE me->mt_marc[]
       FOR ALL ENTRIES IN me->mt_mara[]
     WHERE matnr = me->mt_mara-matnr.
  ENDMETHOD.


  METHOD read_sd_item_data.
    CALL FUNCTION 'SD_VBAP_READ_WITH_VBELN'
      EXPORTING
        i_vbeln          = iv_vbeln
      TABLES
        et_vbap          = ct_vbap[]
      EXCEPTIONS
        record_not_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /lsrd/cx_error.
    ENDIF.
  ENDMETHOD.


  METHOD read_single_sd_header_data.
    es_vbak = cl_sd_data_select=>get_vbak( iv_vbeln = iv_vbeln ).
    IF es_vbak IS INITIAL.
      RAISE EXCEPTION TYPE /lsrd/cx_error.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
