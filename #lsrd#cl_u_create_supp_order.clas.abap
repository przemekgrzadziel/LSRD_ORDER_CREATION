class /LSRD/CL_U_CREATE_SUPP_ORDER definition
  public
  inheriting from /LSRD/CL_LAYER_U
  create public .

public section.

  events START_OF_SELECTION
    exporting
      value(IV_PROTID) type /LSRD/E_PROTOCOL_ID
      value(IV_PRTDSC) type TEXT75
      value(IV_KUNNR) type /LSRD/E_TRIAL_KUNNR
      value(IV_BPARTN) type BU_PARTNER
      value(IV_PO_NR) type BSTKD
      value(IV_PO_TYP) type BSARK
      value(IV_REF) type IHREZ
      value(IV_VKORG) type VKORG
      value(IV_VTWEG) type VTWEG
      value(IV_SPART) type SPART
      value(IV_MEDCNT) type I
      value(IT_SERNR) type RANGES_SERNR .
  events END_OF_SELECTION .

  methods TRIGER_START_OF_SELECTION
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
      !IT_SERNR type RANGES_SERNR
    raising
      /LSRD/CX_ERROR .
  methods TRIGER_END_OF_SELECTION .
protected section.
private section.
ENDCLASS.



CLASS /LSRD/CL_U_CREATE_SUPP_ORDER IMPLEMENTATION.


  METHOD triger_end_of_selection.
    RAISE EVENT end_of_selection.
  ENDMETHOD.


  METHOD triger_start_of_selection.
    RAISE EVENT start_of_selection EXPORTING iv_protid = iv_protid
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
                                             it_sernr  = it_sernr[].
  ENDMETHOD.
ENDCLASS.
