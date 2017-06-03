Ext.define('ExtMVC.store.Contacts', {
	extend : 'Ext.data.Store',
	requires : ['ExtMVC.model.Contact'],
	constructor : function (cfg) {
		var me = this;
		cfg = cfg || {};
		me.callParent([Ext.apply({
					autoLoad : true,
					storeId : 'contactsStore',
					model : 'ExtMVC.model.Contact',
					pageSize : 10,
					totalproperty : 'records',
					remoteSort : true,
					remoteFilter : false,
					proxy : {
						type : 'rest',
						simpleSortMode : true,
						directionParam : "dir",
						sortParam : "sort",
						noCache : false,
						actionMethods : {
							create : 'POST',
							read : 'GET',
							update : 'PUT',
							destroy : 'DELETE'
						},
						api : {
							create : 'http://10.8.12.17:7770/root/REGM_REGSTU',
							read : 'http://10.8.12.17:7770/root/REGM_REGSTU/?SELECT=*',
							update : 'http://10.8.12.17:7770/root/REGM_REGSTU/',
							destroy : 'http://10.8.12.17:7770/root/REGM_REGSTU/'
						},
						reader : {
							type : 'json',
							root : 'values'
						},
						writer : {
							type : 'json',
							encode : false
						},
					}
				}, cfg)]);
	}
});
