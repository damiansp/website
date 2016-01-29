var eugLL = new google.maps.LatLng( 44.060665, -123.122550);

function initialize() {
    mapOptions = { center: eugLL,
		   zoom: 11,
		   mapTypeId: google.maps.MapTypeId.ROADMAP,
		   disableDefaultUI: true,
		   panControl: false,
		   mapTypeControl: false,
		   streetViewControl: false,
		   scaleControl: false,

		   mapTypeControlOptions: {
		       mapTypeIds: [ google.maps.MapTypeId.SATELLITE,
				     google.maps.MapTypeId.ROADMAP,
				     google.maps.MapTypeId.TERRAIN ]
				     },
		   streetViewControl: true,
		   overviewMapControl: true };

    map = new google.maps.Map( document.getElementById('map-canvas'),
			       mapOptions );

    $('#satellite-button').on('click', function() {
	map.setMapTypeId(google.maps.MapTypeId.SATELLITE);
    });

    $('#road-map-button').on('click', function() {
	map.setMapTypeId(google.maps.MapTypeId.ROADMAP);
    });

    $('#terrain-button').on('click', function() {
	map.setMapTypeId(google.maps.MapTypeId.TERRAIN);
    });

};

google.maps.event.addDomListener(window, 'load', initialize);




