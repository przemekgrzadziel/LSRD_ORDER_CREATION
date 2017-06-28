*&---------------------------------------------------------------------*
*& Report           /LSRD/I_CREATE_SUPP_ORD_C01
*&---------------------------------------------------------------------*
*& Software Suite  : Life Science Research and Development Add-on      *
*& Owner           : Deloitte                                          *
*& Development ID  : TS_0                                              *
*& Description     : Create Supply order  - GUI                        *
*&                                                                     *
*&---------------------------------------------------------------------*
*& Init.  Who                    Date(DDMMYYY) Text                    *
*&---------------------------------------------------------------------*
*& INIT   Przemyslaw Grzadziel   09.06.2017    Creation                *
*&---------------------------------------------------------------------*
CLASS lcl_appl DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS: class_constructor,
      get_instance
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_appl,
      initialization,
      at_selection_screen_output
        IMPORTING
          iv_protid TYPE /lsrd/e_protocol_id
          iv_kunnr  TYPE /lsrd/e_trial_kunnr
          it_sernr  TYPE ranges_sernr
        CHANGING
          cv_medcnt TYPE i,
      set_description_for_study
        IMPORTING
          iv_protid TYPE /lsrd/e_protocol_id
        CHANGING
          cv_prtdsc TYPE text75,
      count_mednos
        IMPORTING
          it_sernr  TYPE ranges_sernr
        CHANGING
          cv_medcnt TYPE i,
      start_of_selection
        IMPORTING
          iv_protid TYPE /lsrd/e_protocol_id
          iv_prtdsc TYPE text75
          iv_kunnr  TYPE /lsrd/e_trial_kunnr
          iv_bpartn TYPE bu_partner
          iv_po_nr  TYPE bstkd
          iv_po_typ TYPE bsark
          iv_ref    TYPE ihrez
          iv_vkorg  TYPE vkorg
          iv_vtweg  TYPE vtweg
          iv_spart  TYPE spart
          iv_medcnt TYPE i
          it_sernr  TYPE ranges_sernr,
      end_of_selection.

    CLASS-DATA: so_instance TYPE REF TO lcl_appl.
  PRIVATE SECTION.
    DATA: mv_controller_ref TYPE string,
          mv_view_ref       TYPE string,
          mo_exception      TYPE REF TO /lsrd/cx_error,
          mv_msg            TYPE string,
          mo_controller     TYPE REF TO /lsrd/cl_a_create_supp_order,
          mo_view           TYPE REF TO /lsrd/cl_u_create_supp_order.

    METHODS:
      process_initialization,
      process_sel_screen_output
        IMPORTING
          iv_protid TYPE /lsrd/e_protocol_id
          iv_kunnr  TYPE /lsrd/e_trial_kunnr
          it_sernr  TYPE ranges_sernr
        CHANGING
          cv_medcnt TYPE i,
      set_fields_read_only,
      check_medno_counter
        IMPORTING
          it_sernr  TYPE  ranges_sernr
        CHANGING
          cv_medcnt TYPE i,
      set_fields_visible_read_only
        IMPORTING
          iv_protid TYPE /lsrd/e_protocol_id
          iv_kunnr  TYPE /lsrd/e_trial_kunnr,
      set_mvc
        IMPORTING
          iv_repid TYPE syrepid DEFAULT sy-repid
          iv_dynnr TYPE sydynnr DEFAULT sy-dynnr,
      process_count_mednos
        IMPORTING
          it_sernr  TYPE ranges_sernr
        CHANGING
          cv_medcnt TYPE i,
      count_mednos_bt_range
        IMPORTING
          is_sernr TYPE range_sernr
        CHANGING
          ct_sernr TYPE /lsrd/cl_a_create_supp_order=>tt_gernr,
      process_set_descr_for_study
        IMPORTING
          iv_protid TYPE /lsrd/e_protocol_id
        CHANGING
          cv_prtdsc TYPE text75.



*      set_controller
*        IMPORTING
*          iv_repid TYPE syrepid DEFAULT sy-repid
*          iv_dynnr TYPE sydynnr DEFAULT sy-dynnr,
*      set_view,
*      status.
*      process_pbo.


ENDCLASS.
CLASS lcl_appl IMPLEMENTATION.
  METHOD class_constructor.
    CREATE OBJECT so_instance.
  ENDMETHOD.
  METHOD get_instance.
    ASSERT so_instance IS BOUND.
    ro_instance = so_instance.
  ENDMETHOD.
  METHOD initialization.
* set field not active, only
    DATA: lo_instance TYPE REF TO lcl_appl.
*--------------------------------------------------------------------*
* get instance
    lo_instance = lcl_appl=>get_instance( ).

* process init report
    lo_instance->process_initialization( ).

* set mvc
    lo_instance->set_mvc( ).

  ENDMETHOD.
  METHOD at_selection_screen_output.
* set field not active, only
    DATA: lo_instance TYPE REF TO lcl_appl.
*--------------------------------------------------------------------*
* get instance
    lo_instance = lcl_appl=>get_instance( ).

* process init report
    lo_instance->process_sel_screen_output( EXPORTING iv_protid = iv_protid
                                                      iv_kunnr  = iv_kunnr
                                                      it_sernr  = it_sernr[]
                                            CHANGING  cv_medcnt = cv_medcnt ).
  ENDMETHOD.
  METHOD set_description_for_study.
* set field not active, only
    DATA: lo_instance TYPE REF TO lcl_appl.
*--------------------------------------------------------------------*
* get instance
    lo_instance = lcl_appl=>get_instance( ).

    lo_instance->process_set_descr_for_study( EXPORTING iv_protid = iv_protid
                                              CHANGING  cv_prtdsc = cv_prtdsc ).
  ENDMETHOD.
  METHOD count_mednos.
* set field not active, only
    DATA: lo_instance TYPE REF TO lcl_appl.
*--------------------------------------------------------------------*
* get instance
    lo_instance = lcl_appl=>get_instance( ).

    lo_instance->process_count_mednos( EXPORTING it_sernr  = it_sernr[]
                                       CHANGING  cv_medcnt = cv_medcnt ).
  ENDMETHOD.
  METHOD start_of_selection.
* set field not active, only
    DATA: lo_instance TYPE REF TO lcl_appl.
*--------------------------------------------------------------------*
* get instance
    lo_instance = lcl_appl=>get_instance( ).

* trigger start of selection
    lo_instance->mo_view->triger_start_of_selection( iv_protid = iv_protid
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
  ENDMETHOD.
  METHOD end_of_selection.
* set field not active, only
    DATA: lo_instance TYPE REF TO lcl_appl.
*--------------------------------------------------------------------*
* get instance
    lo_instance = lcl_appl=>get_instance( ).

    lo_instance->mo_view->triger_end_of_selection( ).

  ENDMETHOD.
  METHOD process_initialization.

* set fields read only
    me->set_fields_read_only( ).

  ENDMETHOD.
  METHOD process_sel_screen_output.
* set fields read only and show element
    me->set_fields_visible_read_only( EXPORTING iv_protid = iv_protid
                                                iv_kunnr  = iv_kunnr ).

* get initial prot_id description
*    me->set_prot_id_dscr(  ).

* check med counter
    me->check_medno_counter( EXPORTING it_sernr  = it_sernr[]
                             CHANGING  cv_medcnt = cv_medcnt ).


  ENDMETHOD.
  METHOD set_fields_read_only.
    LOOP AT SCREEN.
      IF screen-group1 = 'NA1' OR
         screen-group1 = 'NA2'.
        screen-input     = '0'.
        screen-invisible = '1'.
      ELSEIF screen-group1 = 'NA3'.
        screen-input = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.
  METHOD check_medno_counter.
    CHECK it_sernr[] IS INITIAL.
    CLEAR cv_medcnt.
  ENDMETHOD.
  METHOD set_fields_visible_read_only.
    LOOP AT SCREEN.
      IF ( ( screen-group1 = 'NA1' AND
             iv_protid IS INITIAL ) OR
           ( screen-group1 = 'NA2' AND
             iv_kunnr IS INITIAL ) ).
        screen-input     = '0'.
        screen-invisible = '1'.
      ELSEIF ( ( screen-group1 = 'NA1' AND
                iv_protid IS NOT INITIAL ) OR
               ( screen-group1 = 'NA2' AND
                 iv_kunnr IS NOT INITIAL ) ).
        screen-input = '0'.
      ELSEIF screen-group1 = 'NA3'.
        screen-input = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.
  METHOD set_mvc.
    CASE iv_dynnr.
      WHEN 1000.
        me->mv_controller_ref = /lsrd/cl_a_create_supp_order=>cv_main_controller.
        me->mv_view_ref       = /lsrd/cl_a_create_supp_order=>cv_main_view.
    ENDCASE.

*CONTROLLER Creation
    TRY.
        /lsrd/cl_layer_a=>create_controller( EXPORTING iv_repid          = iv_repid
                                                       iv_dynnr          = iv_dynnr
                                                       iv_controller_ref = me->mv_controller_ref
                                             IMPORTING eo_controller     = go_controller ).

        go_view = /lsrd/cl_layer_u=>create_view( iv_repid          = iv_repid
                                                 iv_dynnr          = iv_dynnr
                                                 io_controller     = go_controller
                                                 iv_view_ref       = me->mv_view_ref ).


        me->mo_controller ?= /lsrd/cl_layer_a=>get_controller( iv_repid          = iv_repid
                                                               iv_dynnr          = iv_dynnr
                                                               iv_controller_ref = me->mv_controller_ref ).


        me->mo_view ?=  /lsrd/cl_layer_u=>get_view( iv_repid = iv_repid
                                                    iv_dynnr = iv_dynnr ).


        SET HANDLER me->mo_controller->start_of_selection
                    me->mo_controller->end_of_selection
                FOR me->mo_view.


      CATCH /lsrd/cx_error INTO me->mo_exception.
        "// Display Long text message
        me->mv_msg = me->mo_exception->get_text( ).
        MESSAGE me->mv_msg TYPE 'E'.
    ENDTRY.
  ENDMETHOD.
  METHOD process_count_mednos.
    DATA: ls_sernr TYPE range_sernr,
          lt_sernr TYPE /lsrd/cl_a_create_supp_order=>tt_gernr.
*--------------------------------------------------------------------*
    IF it_sernr[] IS INITIAL.
      CLEAR cv_medcnt.
      RETURN.
    ENDIF.

    LOOP AT it_sernr[] INTO ls_sernr.
      CASE ls_sernr-option.
        WHEN /lsrd/cl_a_create_supp_order=>cv_option_eq.
          APPEND ls_sernr-low TO lt_sernr[].
        WHEN /lsrd/cl_a_create_supp_order=>cv_option_bt.
          me->count_mednos_bt_range( EXPORTING is_sernr = ls_sernr
                                     CHANGING  ct_sernr = lt_sernr[] ).
      ENDCASE.
      SORT lt_sernr[] BY table_line.
      DELETE ADJACENT DUPLICATES FROM lt_sernr[] COMPARING table_line.
    ENDLOOP.

    cv_medcnt = lines( lt_sernr ).

  ENDMETHOD.
  METHOD count_mednos_bt_range.
    DATA: lv_gernr TYPE gernr.
*--------------------------------------------------------------------*
    lv_gernr = is_sernr-low.
    WHILE lv_gernr <= is_sernr-high.
      APPEND lv_gernr TO ct_sernr[].
      ADD 1 TO lv_gernr.
      UNPACK lv_gernr TO lv_gernr.
    ENDWHILE.
  ENDMETHOD.
  METHOD process_set_descr_for_study.

  ENDMETHOD.
ENDCLASS.
