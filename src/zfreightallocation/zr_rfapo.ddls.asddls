@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Freight Allocation PO Data Definition'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZR_RFAPO as select from zrfapo
association        to parent ZR_RFA    as _AF       on  $projection.FaId = _AF.FaId
association [0..*] to I_PurchaseOrderAPI01 as _PurchaseOrders on $projection.PoId = _PurchaseOrders.PurchaseOrder
  
{
    key item_uuid as ItemUuid,
    fa_id as FaId,
    po_id as PoId,
    local_last_changed_at as LocalLastChangedAt,
    _AF,
    _PurchaseOrders
}
