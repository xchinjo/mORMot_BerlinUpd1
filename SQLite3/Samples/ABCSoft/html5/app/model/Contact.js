Ext.define('ExtMVC.model.Contact', {
	extend : 'Ext.data.Model',
	idProperty : 'ID',
	totalproperty : 'total',
	fields : [{
			name : 'ID',
			type : 'int'
		}, {
			name : 'REGDTE',
			type : 'datetime'
		}, {
			name : 'REGNAT',
			type : 'string'
		}, {
			name : 'REGCDE',
			type : 'string'
		}
	]
});
