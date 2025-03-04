@EndUserText.label: 'DDL for Domain Value List'
@ObjectModel.resultSet.sizeCategory: #XS
@ObjectModel.query.implementedBy: 'ABAP:ZCLASS_GET_DOMAIN_FIX_VALUES'

define custom entity ZI_DOMAIN_FIX_VALUE
{


@UI.facet      : [
           {
             id      :  'Domain Fixed Value',
             purpose :  #STANDARD,
             type    :  #IDENTIFICATION_REFERENCE,
             label   :  'Flights',
             position: 10 }
         ]
         
  @EndUserText.label     : 'domain name'
   @UI.lineItem   : [{ position: 10 }]
      @UI.selectionField : [{position: 10}]
      @UI.identification: [{position: 10}]
    // @UI.hidden : true 
    
  key domain_name  : sxco_ad_object_name;
  
  
   @UI.lineItem   : [{ position: 20 }]
      @UI.selectionField : [{position: 20}]
      @UI.identification: [{position: 20}]
      //@UI.hidden  : true
  key pos         : abap.numc( 4 );
  
   @UI.lineItem   : [{ position: 30 }]
      @UI.selectionField : [{position: 30}]
      @UI.identification: [{position: 30}]
      @EndUserText.label     : 'lower_limit'
//      @Consumption.valueHelpDefinition: 
//      [{ entity: { name : 'ZI_DOMAIN_FIX_VAL', element: 'low'} ,
//         additionalBinding: [{ element: 'domain_name',
//                               localConstant: 'Z_BILLTYPE_DATAELEMENT', usage: #FILTER }]
//                               , distinctValues: true
//                               }]
      low         : abap.char( 10 );
      
      
        @UI.lineItem   : [{ position: 40 }]
      @UI.selectionField : [{position: 40}]
      @UI.identification: [{position: 40}]
      @EndUserText.label     : 'upper_limit'
      high        : abap.char(10);
      
        @UI.lineItem   : [{ position: 50 }]
      @UI.selectionField : [{position: 50}]
      @UI.identification: [{position: 50}]
      @EndUserText.label     : 'Description'
//      @Consumption.valueHelpDefinition: 
//      [{ entity: { name : 'ZI_DOMAIN_FIX_VAL' , element: 'description' } ,
//         additionalBinding: [{ element: 'domain_name',
//                               localConstant: 'Z_BILLTYPE_DATAELEMENT', usage: #FILTER }]
//                               , distinctValues: true
//                               }]
      description : abap.char(60);
  
}


