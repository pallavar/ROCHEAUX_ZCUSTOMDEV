@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: 'ZR_RFAI'
}
@ObjectModel: {
  usageType.dataClass: #MIXED, 
  usageType.serviceQuality: #X, 
  usageType.sizeCategory: #S
}
@AccessControl.authorizationCheck: #CHECK
define view entity ZC_RFAI
  as projection on ZR_RFAI
{
  
  @UI.hidden: true
  key ItemUuid,
  @UI.hidden: true
  FaId,
  @EndUserText: {
    quickInfo: 'Purchasing Document Number'
  }
  OrderPo,
  ItemId,
  UnitField,
  @Semantics: {
    quantity.unitOfMeasure: 'UnitField'
  }
  OrdQty,
  CukyField,
  @Semantics: {
    amount.currencyCode: 'CukyField'
  }
  PoUnitPrice,
  @Semantics: {
    quantity.unitOfMeasure: 'UnitField'
  }
  RcvdQty,
  @Semantics: {
    amount.currencyCode: 'CukyField'
  }
  RcvdAmount,
  @Semantics: {
    quantity.unitOfMeasure: 'UnitField'
  }
  @UI.hidden: true
  AllowedQuantity,
  @Semantics: {
    amount.currencyCode: 'CukyField'
  }
  AllocFrt,
  @Semantics: {
    amount.currencyCode: 'CukyField'
  }
  FrtValue,
  @Semantics: {
    amount.currencyCode: 'CukyField'
  }
  ConValue,
  @UI.hidden: true
  LocalLastChangedAt,
  _AF : redirected to parent ZC_RFA
  
}
