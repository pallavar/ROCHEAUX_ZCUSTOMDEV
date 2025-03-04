@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZR_RFAI'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

define view entity ZR_RFAI as select from zrfai

association        to parent ZR_RFA    as _AF       on  $projection.FaId = _AF.FaId

{
    key item_uuid as ItemUuid,
    fa_id as FaId,
    order_po as OrderPo,
    item_id as ItemId,
    unit_field as UnitField,
    @Semantics.quantity.unitOfMeasure : 'UnitField'
    ord_qty as OrdQty,
    cuky_field as CukyField,
    @Semantics.amount.currencyCode: 'CukyField'
     po_unit_price as PoUnitPrice,
         @Semantics.quantity.unitOfMeasure : 'UnitField'
    rcvd_qty as RcvdQty,
    @Semantics.amount.currencyCode: 'CukyField'
    rcvd_amount as RcvdAmount,
        @Semantics.quantity.unitOfMeasure : 'UnitField'
    allowed_quantity as AllowedQuantity,
    @Semantics.amount.currencyCode: 'CukyField'
    alloc_frt as AllocFrt,
    @Semantics.amount.currencyCode: 'CukyField'
    frt_value as FrtValue,
    @Semantics.amount.currencyCode: 'CukyField'
    con_value as ConValue,
    local_last_changed_at as LocalLastChangedAt,
    
    _AF
}
