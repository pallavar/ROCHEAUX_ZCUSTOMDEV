@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: 'ZR_RFAIPO'
}
@ObjectModel: {
  usageType.dataClass: #MIXED, 
  usageType.serviceQuality: #X, 
  usageType.sizeCategory: #S
}
@AccessControl.authorizationCheck: #CHECK

define view entity ZC_RFAIPO as projection on ZR_RFAPO
{
   @UI.hidden: true
    key ItemUuid,
     @UI.hidden: true
    FaId,
     @EndUserText: {
    quickInfo: 'Purchasing Document Number'
  }
    @Consumption.valueHelpDefinition: [{entity: {name: 'I_PurchaseOrderAPI01', element: 'PurchaseOrder' }, useForValidation: true }]
     
    PoId,
   @UI.hidden: true
    LocalLastChangedAt,
    /* Associations */
    _PurchaseOrders,
    _AF : redirected to parent ZC_RFA
}
