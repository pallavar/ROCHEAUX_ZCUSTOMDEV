@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Freight Allocation'
@Metadata.allowExtensions: true
@Search.searchable: true
define root view entity ZR_RFA as select from zrfa
composition [0..*] of   ZR_RFAI     as _FAI
composition [0..*] of   ZR_RFAPO     as _ZFAPO
association [0..1] to I_Currency               as _Currency      on $projection.CurrencyCode = _Currency.Currency
  
{
    key fa_id as FaId,
    
    
    
    @Semantics.amount.currencyCode: 'CurrencyCode'
    freightamount as Freightamount,
    currency_code as CurrencyCode,
    
     @Semantics.user.createdBy: true
      created_by              as CreatedBy,

      @Semantics.systemDateTime.createdAt: true
      created_at              as CreatedAt,

      @Semantics.user.lastChangedBy: true
      last_changed_by         as LastChangedBy,

      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at         as LastChangedAt,

      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      locl_last_changed_at    as LoclLastChangedAt,
      
     @Search.defaultSearchElement: true
      _Currency,
      _ZFAPO,
      _FAI

}
