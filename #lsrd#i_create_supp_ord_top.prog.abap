*&---------------------------------------------------------------------*
*& Report           /LSRD/I_CREATE_SUPP_ORD_TOP
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
TABLES: equi.
DATA: go_view       TYPE REF TO /lsrd/if_layer_u,
      go_controller TYPE REF TO /lsrd/if_layer_a.
