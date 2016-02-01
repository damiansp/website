// Width and height for all SVG elements
var w = window.innerWidth * 0.8;
var h = window.innerHeight * 0.8;

// Define map projection
var projection = d3.geo.albersUsa()
  .translate([w/2, h/2])
  .scale([w * 0.9]); // w?

// Define path generator
var path = d3.geo.path()
  .projection(projection);


/**
 * Load data from a csv and generate choropleth from a given field
 * @param field: name of the field in the data to be represented
 * @param svgName: the name of the svg where the data are to be plotted
 * @param color1, color2: the endpoints of the color gradient to represent
 *        min and max values
 */
function choropleth(elemID, field, color1, color2, displayName, gradientName) {
  //Create SVG element
  var svg = d3.select(elemID)
    .append("svg")
    .attr("width", w)
    .attr("height", h);
  
  d3.csv("stateCupid.csv", function(data) {
      // Define color gradient
      var color = d3.scale.linear()
        .domain([ d3.min(data, function(d) { return d[field]; }),
                  d3.max(data, function(d) { return d[field]; }) ])
        .range([color1, color2]);


      // Load in GeoJSON data
      d3.json("usStates.json", function(json) {
          
          //Merge the okc data and GeoJSON
          //Loop through once for each okc data value
          for (var i = 0; i < data.length; i++) {
            
            //Grab state name
            var dataState = data[i].state;

            //Grab data value, and convert from string to float
            var fieldValue = parseFloat(data[i][field]);

            //Find the corresponding state inside the GeoJSON
            for (var j = 0; j < json.features.length; j++) {
              var jsonState = json.features[j].properties.name;
              
              if (dataState == jsonState) {
                //Copy the data value into the JSON
                json.features[j].properties.value = fieldValue;
                //Stop looking through the JSON
                break;
              }
            }
          }
          
          // Bind data and create one path per GeoJSON feature
          svg.selectAll("path")
            .data(json.features)
            .enter()
            .append("path")
            .attr("d", path)
            .style("fill", function(d) {
                //Get data value
                var value = d.properties.value;
                
                if (value) {
                  //If value exists…
                  return color(value);
                } else {
                  //If value is undefined…
                  return "#ccc";
                }
              })
            .style("stroke", '#666');
        }); 
  
      // Add a key
      var gradient = svg.append('svg:defs')
        .append('svg:linearGradient')
        .attr('id', gradientName)
        .attr({
            'x1': '0%', 'x2': '0%',
            'y1': '0%', 'y2': '100%',
            'spreadMethod': 'pad'
        });

      gradient
        .append('svg:stop')
        .attr({
            'offset': '0%',
            'stop-color': color2,
            'stop-opacity': 1
        });

      gradient
        .append('svg:stop')
        .attr({
            'offset': '100%',
            'stop-color': color1,
            'stop-opacity': 1
        });
    
      var key = svg.append('g');
      key.append('rect')
        .attr({
            'x': w - 0.05 * w,
            'y': 10,
            'height': h * 0.8,
            'width': 20,
            'stroke': 'black',
            'fill': 'url(#' + gradientName + ')'
        });

      key.append('text')
        .text(displayName)
        .attr({
            'text-anchor': 'middle',
            'x': w - 0.05 * w,
            'y': h * 0.9,
            'fill': 'black'
        });
      
      key.append('text')
        .text(d3.min(data, function(d) {
              return Math.round(100 * d[field]) / 100;
        }))
        .attr({
            'text-anchor': 'end',
            'x': w - 0.05 * w - 10,
            'y': h * 0.8 + 10,
            'fill': 'black'
        });

      key.append('text')
        .text(d3.max(data, function(d) {
              return Math.round(100 * d[field]) / 100;
        }))
        .attr({
            'text-anchor': 'end',
            'x': w - 0.05 * w - 10,
            'y': 20,
            'fill': 'black'
        });


    });

};

/** args = elemID, field, color1, color2, displayName, gradientName*/
choropleth('#age', 'age', 'pink', 'black', 'Average Age', 'g1');
choropleth('#match', 'pMatch', 'blue', 'red', 'Match Score', 'g2');
// No idea why this one is not displaying correctly
//choropleth('#enemy', 'enemy', 'orange', 'purple', '"Enemy" Score', 'g3');
choropleth('#attraction', 'attraction', 'grey', 'red', 'Attraction', 'g4');
choropleth('#height', 'height', 'yellow', 'blue', 'Height (cm)', 'g5');
choropleth('#multilingual', 'state_ml', 'orange', 'blue',
           'Multilingual', 'g6');
choropleth('#high-school', 'state_hs', 'green', 'orange',
           'Prop. HS Only', 'g7');
choropleth('#post-grad', 'state_pg', 'purple', 'yellow', 'Post Grad', 'g8');
choropleth('#bisexual', 'state_bi', 'blue', 'pink', 'Bisexual', 'g9');
choropleth('#casual-sex', 'casSex', 'steelblue', 'red', 'Casual Sex', 'g10');
//choropleth('#overall', 'overall', 'cyan', 'magenta', 'Match Score', 'g11');
