// At 44.046947 N, 1 deg = 11,1113.1185m
var METERS_PER_DEGREE = 111113.1185;

function metersToDegrees(meters) {
    var degrees = meters / METERS_PER_DEGREE;
    return degrees
}

// Distance between Prism grid points in degress lat/lon
var fourKmInDegrees = metersToDegrees(4000);

// Note: coord order for Google maps is (lat, lon) = (y, x)
var eugeneCenter = new google.maps.LatLng(44.04767, -123.1007);
var xrange = [-123.568675, -122.603938],
    yrange = [  43.867677,   44.226478];

var eugXs = [],
    eugYs = [];

// Span the range of x and y adding points at every 4 km
var nextX = xrange[0],
    nextY = yrange[0];

while (nextX <= xrange[1]) {
    eugXs.push(nextX);
    nextX += fourKmInDegrees;
}

while (nextY <= yrange[1]) {
    eugYs.push(nextY);
    nextY += fourKmInDegrees;
}

var loci = [];
for (var x in eugXs) {
    for (var y in eugYs) {
	loci.push([eugXs[x], eugYs[y]]);
    }
}


function initialize() {
    
    var mapOptions = { center: eugeneCenter,
		       zoom: 10,
                       panControl: true,
                       mapTypeControl: true, 
		       mapTypeControlOptions: {
	                   style: google.maps.MapTypeControlStyle.DEFAULT,
			   mapTypeIds: [ google.maps.MapTypeId.SATELLITE,
					 google.maps.MapTypeId.ROADMAP,
					 google.maps.MapTypeId.TERRAIN ]
	               },
                       streetViewControl: true,
                       overviewMapControl: true };
    
    var map = new google.maps.Map( document.getElementById('map-canvas'),
				   mapOptions );
    var lociLL = [];
    for (var l in loci) {
	lociLL.push(new google.maps.LatLng(loci[l][1], loci[l][0]));
    }


    for (var m = 0; m < lociLL.length; m++) {
	new google.maps.Marker({ position: lociLL[m],
		                 map: map });
    } 

}

google.maps.event.addDomListener(window, 'load', initialize);