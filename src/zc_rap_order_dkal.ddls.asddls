@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Orders projection view'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZC_RAP_Order_Dkal as projection on ZI_RAP_Order
{
    key OrderUuid,
    @Search.defaultSearchElement: true
    OrderId,
    Name,
    Status,
    @Consumption.valueHelpDefinition: [{ entity: { name: '/DMO/I_Customer', element: 'CustomerID'} }]
    Customer,
    CreationDate,
    CancellationDate,
    CompletionDate,
    DeliveryCountry,
    TotalPrice,
    Currency,
    @EndUserText.label: 'Complexity'
    @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_CALCULATION_COMPLEXITY'
    virtual Complexity: abap.string( 256 ),
    
    /* Associations */
    _Country,
    _Customer,
    _Item: redirected to composition child ZC_RAP_Item_Dkal 
}
