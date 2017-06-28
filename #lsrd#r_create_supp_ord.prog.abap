*&---------------------------------------------------------------------*
*& Report           /LSRD/R_CREATE_SUPP_ORD
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
REPORT  /lsrd/r_create_supp_ord.
INCLUDE /lsrd/i_create_supp_ord_top. " global data
INCLUDE /lsrd/i_create_supp_ord_c01. " local classes def / imp
INCLUDE /lsrd/i_create_supp_ord_s01. " selection Screen data
