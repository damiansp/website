var dataset;
var w = 1350,
    h = 500;
var padding = 0;


var xScale = d3.scale.linear()
    .domain([-123.5687, -122.6327])
    .range([padding, w - padding * 2]);

var yScale = d3.scale.linear() 
    .domain([43.86768, 44.19167])
    .range([h - padding, padding]);

// Create scales for color coding statistics on the heat map
var tempRange = [-6.615, 26.416];
var tempScale = d3.scale.linear()
    .domain(tempRange)
    .range([0, 255]);

var minTempRange = [-9.74, 19.02];
var minTempScale = d3.scale.linear()
    .domain(minTempRange)
    .range([0, 255]);

var maxTempRange = [-3.51, 37.91];
var maxTempScale = d3.scale.linear()
    .domain(maxTempRange)
    .range([0, 255]);


var tempIrregRange = [-4.8075, 4.9025];
var tempIrregScale = d3.scale.linear()
    .domain(tempIrregRange)
    .range([0, 255]);

var minTempIrregRange = [-4.335, 4.460];
var minTempIrregScale = d3.scale.linear()
    .domain(tempIrregRange)
    .range([0, 255]);

var maxTempIrregRange = [-6.91, 6.42];
var maxTempIrregScale = d3.scale.linear()
    .domain(maxTempIrregRange)
    .range([0, 255]);

var pptRange = [0.0, 124.54];
var pptScale = d3.scale.linear()
    .domain(pptRange)
    .range([0, 255]);

var pptIrregRange = [-26.17, 77.64];
var pptIrregScale = d3.scale.linear()
    .domain(pptIrregRange)
    .range([0, 255]);

function statScale(stat, value) {
    if (stat == 'ppt') {
	return pptScale(value);
    } else if (stat == 'pptIrreg') {
	return pptIrregScale(value);
    } else if (stat == 'temp') {
	return tempScale(value);
    } else if (stat == 'tempIrreg') {
	return tempIrregScale(value);
    } else if (stat == 'tmin') {
	return minTempScale(value);
    } else if (stat == 'minTempIrreg') {
	return minTempIrregScale(value);
    } else if (stat == 'tmax') {
	return maxTempScale(value);
    } else if (stat == 'maxTempIrreg') {
	return maxTempIrregScale(value);
    }
};

var locusW = w / 26,
    locusH = h / 9;
    
var subset;


// Extract data for a given date
function daySubset(date, dataset) {
    set = [];
    
    for (var o = 0; o < dataset.length; o++) {
	if (dataset[o]['date'] == date) {
	    set.push(dataset[o]);
	}
    }
    return set;
}

var dateSelected = '1/1/14';
var selectedStat = 'tempIrreg';

plot(dateSelected, 'tempIrreg');

// Update data (statistic) type
$('#ppt-button').on('click', function() {
	selectedStat = 'ppt';
	plot(dateSelected, selectedStat);
});

$('#min-temp-button').on('click', function() {
    selectedStat = 'tmin';
    plot(dateSelected, selectedStat);
});

$('#max-temp-button').on('click', function() {
    selectedStat = 'tmax';
    plot(dateSelected, selectedStat);
});

$('#temp-button').on('click', function() {
    selectedStat = 'temp';
    plot(dateSelected, selectedStat);
});

$('#min-temp-dev-button').on('click', function() {
    selectedStat = 'minTempIrreg';
    plot(dateSelected, selectedStat);
});

$('#max-temp-dev-button').on('click', function() {
    selectedStat = 'maxTempIrreg';
    plot(dateSelected, selectedStat);
});

$('#temp-dev-button').on('click', function() {
    selectedStat = 'tempIrreg';
    plot(dateSelected, selectedStat);
});

function validDate(date) {
    splitDate = date.split('/');
    if (splitDate.length != 2) {
	alert('Date not in proper format');
	return false;
    } else if (splitDate[0][0] == 0 | splitDate[1][0] == 0) {
	alert ('Do not put leading 0s in the date');
	return false;
    } else if (splitDate[0] > 12) {
	alert('month > 12');
	return false;
    } else if (splitDate[0] == 2 & splitDate[1] > 28) {
	alert('Feb has only 28 days');
	return false;
    } else if ((splitDate[0] == 4 | splitDate[0] == 6 | splitDate[0] == 9 |
		splitDate[0] == 11) & splitDate[1] > 30) {
	alert('Month has only 30 days');
	return false;
    } else if (splitDate[1] > 31) {
	alert('Month has only 31 days');
	return false;
    } else {
	return true;
    }
};

$('#date-button').on('click', function() {
	var dateInput = $('#date-input').val();
	if(validDate(dateInput)) {
	    dateSelected = dateInput + '/14';
	    plot(dateSelected, selectedStat);
	}
});


function plot(date, stat) {
    $('svg').remove();

    d3.csv('eug.csv', function(data) {
	dataset = data;

    
	subset = daySubset(date, dataset);
	// Note: the data are structured that the first 270 rows are Jan 1,
	// (for Locus ID 1 - 270), the next 270, Jan 2, and so on.

	var svg = d3.select('body')
	    .append('svg')
	    .attr({ 'width': w, 'height': h });

	svg.append('g')
	    .selectAll('rect')
	    .data(subset)
	    .enter()
	    .append('rect')
	    .attr({ 'x': function(d) { return xScale(d['x']); },
		    'y': function(d) { return yScale(d['y']); },
		    'width': locusW,
		    'height': locusH,
		    'fill': function(d) { 
			// Temperature deviation from day's mean
			return 'rgb(' + 
			    Math.round(statScale(stat, d[stat])) +
			    ', 0 ,' + 
			    Math.round(255 - statScale(stat, d[stat]))+
			    ')';
		    }
	    });
	$('svg').css('opacity', $('#transparency').val());
	$('#current-stat').html(selectedStat);
	$('#current-date').html(dateSelected);

    });  // d3.csv

};



// Vary transparency (easier to do with the CSS param than in d3)
$('#transparency').on('change', function() {
	$('svg').css('opacity', this.value);
    });


//$(document).ready(function() {
//}); 