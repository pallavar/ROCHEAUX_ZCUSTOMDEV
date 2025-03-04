**"* use this source file for the definition and implementation of
**"* local helper classes, interface definitions and type
**"* declarations

*CLASS zcl_badi_material_ref DEFINITION
*  PUBLIC
*  FINAL
*  CREATE PUBLIC .
*
*  PUBLIC SECTION.
*
*    INTERFACES if_badi_interface .
*    INTERFACES if_ex_material_reference .
*  PROTECTED SECTION.
*  PRIVATE SECTION.
*ENDCLASS.
*
*
*
*CLASS zcl_badi_material_ref IMPLEMENTATION.
*
*
*  METHOD if_ex_material_reference~create_material.
*
*    MOVE-CORRESPONDING i_mara TO e_marau.
*
** Set custom UOM based on product type
*    CASE i_mara-mtart.
*      WHEN 'FERT' OR 'AF' OR  'DF' OR 'EF' OR 'GF' OR 'HF' OR 'KF' OR 'XF' OR 'RF'
*          OR 'ZF' OR 'QF' OR 'FF' OR 'SERV'.
*        e_marau-yy1_uomconversion_prdu = 'LB'. "Custom UOM (Unit)
*      WHEN 'MF' OR 'NL' OR 'YF'.
*        e_marau-yy1_uomconversion_prdu = 'ROL'.
*      WHEN 'PE'.
*        e_marau-yy1_uomconversion_prdu = 'CAS'.
*      WHEN 'SL'.
*        e_marau-yy1_uomconversion_prdu = 'YD'.
*    ENDCASE.
*
** Compare custom UOM and std UOM.
*    IF e_marau-yy1_uomconversion_prdu = e_marau-meins.
*      e_marau-yy1_uomconversion_prd = '1'.  "Custom UOM
*
*    ELSE.
*      TYPES: BEGIN OF zmy_uom_structure,
*               quantitydenominator TYPE p LENGTH 5 DECIMALS 0,
*               quantitynumerator   TYPE p LENGTH 5 DECIMALS 0,
*             END OF zmy_uom_structure.
*
*      DATA: ls_uom TYPE zmy_uom_structure.
*
*      SELECT SINGLE quantitydenominator, quantitynumerator
*        FROM i_productunitsofmeasure WITH PRIVILEGED ACCESS
*        WHERE product = @i_mara-matnr AND
*             " alternativeunit = @i_mara-yy1_uomconversion_prdu
*              alternativeunit = @e_marau-yy1_uomconversion_prdu
*               INTO (@ls_uom-quantitydenominator, @ls_uom-quantitynumerator).
*      IF sy-subrc = 0.             "unit conversion available
*        e_marau-yy1_uomconversion_prd = ls_uom-quantitydenominator / ls_uom-quantitynumerator.
*
*      ELSE.                        "conversion not maintained
*        e_marau-yy1_uomconversion_prd = '1'.  "Custom UOM
*      ENDIF.
*    ENDIF.
*
*    "  CT_UOM
*  ENDMETHOD.
*ENDCLASS.
