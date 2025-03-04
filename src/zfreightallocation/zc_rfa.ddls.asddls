@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: 'Freight Allocation'
}
@AccessControl.authorizationCheck: #CHECK
define root view entity ZC_RFA
  provider contract transactional_query
  as projection on ZR_RFA
{
  @EndUserText: {
    quickInfo: 'FreightAllocationID'
  }
  key FaId,
  @Semantics: {
    amount.currencyCode: 'CurrencyCode'
  }
  Freightamount,
  
   @Consumption.valueHelpDefinition: [{entity: {name: 'I_CurrencyStdVH', element: 'Currency' }, useForValidation: true }]
  CurrencyCode,
  @EndUserText: {
    quickInfo: 'Created By User'
  }
  @Semantics: {
    user.createdBy: true
  }
  CreatedBy,
  @EndUserText: {
    quickInfo: 'Creation Date Time'
  }
  @Semantics: {
    systemDateTime.createdAt: true
  }
  CreatedAt,
  @EndUserText: {
    quickInfo: 'Local Instance Last Changed By User'
  }
  @Semantics: {
    user.lastChangedBy: true
  }
  LastChangedBy,
  @EndUserText: {
    quickInfo: 'Last Change Date Time'
  }
  @Semantics: {
    systemDateTime.lastChangedAt: true
  }
  LastChangedAt,
  @EndUserText: {
    quickInfo: 'Local Instance Last Change Date Time'
  }
  @UI.hidden: true
  @Semantics: {
    systemDateTime.localInstanceLastChangedAt: true
  }
  LoclLastChangedAt,
  _Currency,
  _FAI : redirected to composition child ZC_RFAI,
  _ZFAPO : redirected to composition child ZC_RFAIPO
  
}
