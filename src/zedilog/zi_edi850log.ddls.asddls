//@AbapCatalog.sqlViewName: 'ZI_EDI850LOG1'
@AbapCatalog.viewEnhancementCategory: [ #NONE ]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface View for EDI850 Log'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
@ObjectModel.resultSet.sizeCategory: #XS
@ObjectModel.query.implementedBy: 'ABAP:ZCLASS_GET_DOMAIN_FIX_VALUES'

@ObjectModel.usageType:{
serviceQuality:#X,
sizeCategory: #S,
dataClass: #MIXED
}
define root view entity ZI_EDI850LOG as select from zedi850_log

     association to ZEDI_STATUS as _ToStatus
     on $projection.Edi850Status = _ToStatus.Statuscode 

{
    key po_id as PoId,
    edi850_status as Edi850Status,
    edistatuscode as Status,
    edistatusdesc as StatusDesc,
    case edistatusdesc
      when 'Retriggered'  then 2    -- 'Open'       | 2: yellow colour
      when 'Success'  then 3    -- 'Accepted'   | 3: green colour
      when 'Error'  then 1    -- 'Rejected'   | 1: red colour
                else 0    -- 'nothing'    | 0: unknown
    end                   as OverallStatusCriticality,
   
    
    @Semantics.user.createdBy: true
    local_created_by as LocalCreatedBy,
    
    @Semantics.systemDateTime.createdAt: true
    local_created_at as LocalCreatedAt,
    
    @Semantics.user.lastChangedBy: true
    local_last_changed_by as LocalLastChangedBy,
    
    @Semantics.systemDateTime.localInstanceLastChangedAt: true
    local_last_changed_at as LocalLastChangedAt,
    
    @Semantics.systemDateTime.lastChangedAt: true
    last_changed_at as LastChangedAt,
    
    _ToStatus
}
