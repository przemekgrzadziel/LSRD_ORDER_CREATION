*&---------------------------------------------------------------------*
*& Report           /LSRD/I_CREATE_SUPP_ORD_S01
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
SELECTION-SCREEN BEGIN OF BLOCK bl01 WITH FRAME TITLE TEXT-ti1.
PARAMETERS: p_protid TYPE /lsrd/e_protocol_id,
            p_prtdsc TYPE text75 MODIF ID na1.
SELECTION-SCREEN END OF BLOCK bl01.

SELECTION-SCREEN BEGIN OF BLOCK bl02 WITH FRAME TITLE TEXT-ti2.
PARAMETERS: p_kunnr TYPE /lsrd/e_trial_kunnr NO-DISPLAY.
PARAMETERS: p_bpartn TYPE bu_partner.
*             p_name  TYPE name1_gp MODIF ID na2,
*            p_addrs TYPE text100  MODIF ID na2,
*            p_cntry TYPE land1_gp MODIF ID na2.
SELECTION-SCREEN END OF BLOCK bl02.

SELECTION-SCREEN BEGIN OF BLOCK bl03 WITH FRAME TITLE TEXT-ti3.
PARAMETERS: p_po_nr  TYPE vbkd-bstkd,
            p_po_typ TYPE bsark DEFAULT 'MANU',
            p_ref    TYPE ihrez,
            p_vkorg  TYPE vkorg,
            p_vtweg  TYPE vtweg,
            p_spart  TYPE spart.
SELECTION-SCREEN END OF BLOCK bl03.
SELECTION-SCREEN BEGIN OF BLOCK bl05 WITH FRAME TITLE TEXT-ti5.
*SELECT-OPTIONS: s_seque FOR -sequence.
SELECT-OPTIONS: s_medno FOR equi-sernr NO INTERVALS MATCHCODE OBJECT eqsn OBLIGATORY.
PARAMETERS: p_medcnt TYPE i MODIF ID na3.
SELECTION-SCREEN END OF BLOCK bl05.


* screen events
INITIALIZATION.
  lcl_appl=>initialization( ).


AT SELECTION-SCREEN OUTPUT.
  lcl_appl=>at_selection_screen_output( EXPORTING iv_protid = p_protid
                                                  iv_kunnr  = p_kunnr
                                                  it_sernr  = s_medno[]
                                        CHANGING  cv_medcnt = p_medcnt ).

AT SELECTION-SCREEN ON p_protid.
  lcl_appl=>set_description_for_study( EXPORTING iv_protid = p_protid
                                       CHANGING  cv_prtdsc = p_prtdsc ).

AT SELECTION-SCREEN ON s_medno.
  lcl_appl=>count_mednos( EXPORTING it_sernr  = s_medno[]
                          CHANGING  cv_medcnt = p_medcnt ).

START-OF-SELECTION.
  lcl_appl=>start_of_selection( iv_protid = p_protid
                                iv_prtdsc = p_prtdsc
                                iv_kunnr  = p_kunnr
                                iv_bpartn = p_bpartn
                                iv_po_nr  = p_po_nr
                                iv_po_typ = p_po_typ
                                iv_ref    = p_ref
                                iv_vkorg  = p_vkorg
                                iv_vtweg  = p_vtweg
                                iv_spart  = p_spart
                                iv_medcnt = p_medcnt
                                it_sernr  = s_medno[] ).

END-OF-SELECTION.
  lcl_appl=>end_of_selection( ).
