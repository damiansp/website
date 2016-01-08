var pca,
  h = window.innerHeight * 0.9, // svg height
  w = window.innerWidth * 0.9,  // width
  r = 10; // scatterplot point radius

// Plotting Colors
var fishColor = '#0000FF',
    crayfishColor = '#FF0000',
    molluscColor = '#FF8888',
    otterFootColor = '#000000',
    otterTailColor = '#AA2288',
    frogColor = '#00FF00';

function assignColor(animal) {
  switch(animal) {
  case 'fish':
    return fishColor;
    break;
  case 'crayfish':
    return crayfishColor;
    break;
  case 'mollusc':
    return molluscColor;
    break;
  case 'otter_foot':
    return otterFootColor;
    break;
  case 'otter_tail':
    return otterTailColor;
    break;
  case 'frog':
    return frogColor;
    break;
  default:
    return '#FFFF00';
    break;
  }
}

  
d3.csv('pcaScores.csv', function(d) {
    pca = d;

    pcx = 'pc1';
    pcy = 'pc2';


    var svg = d3.select('#main').append('svg')
      .attr('class', 'full-width')
      .attr('height', h)
      .attr('width', w);
    
    function plot(pcx, pcy) {
      // Get max and min vals for the pcs being plotted
      var maxX = 0, minX = 0, maxY = 0, minY = 0;
     
      for (s in pca) {
        maxX = Math.max(maxX, pca[s][pcx]);
        minX = Math.min(minX, pca[s][pcx]);
        maxY = Math.max(maxY, pca[s][pcy]);
        minY = Math.min(minY, pca[s][pcy]);
      }

      // Scaling from pca dimensions to window dimensions
      var xScale = d3.scale.linear()
        .domain([minX, maxX])
        .range([2*r, w - (2*r)]);
    
      var yScale = d3.scale.linear()
        .domain([minY, maxY])
        .range([h - (2*r), 2*r]);

      // Axis settings
      var xAxis = { 'x1': minX, 'x2': maxX,
                    'y1': 0, 'y2': 0 },
        yAxis = { 'x1': 0, 'x2': 0,
                  'y1': minY, 'y2': maxY },
          axes = [xAxis, yAxis];
          

      // Plot initial data points
      svg.selectAll('circle')
        .data(pca).enter()
        .append('circle');

      // Plot coordinate axes
      svg.selectAll('line')
        .data(axes).enter()
        .append('line')

      // Updates
      svg.selectAll('circle')
        .transition()
        .duration(500)
        .attr('cx', function(d) { return xScale(d[pcx]); })
        .attr('cy', function(d) { return yScale(d[pcy]); })
        .attr('r', r)
        .attr('fill', function(d) { return assignColor(d.taxon); })
        .attr('stroke', function(d) {
            return (d.taxon == 'otter_foot' ? '#AAAAAA' : 'black')
              });

      svg.selectAll('line')
        .transition(300)
        .attr({ 'x1': function(d) { return xScale(d.x1); },
              'x2': function(d) { return xScale(d.x2) },
              'y1': function(d) { return yScale(d.y1) },
              'y2': function(d) { return yScale(d.y2) },
              'stroke': 'rgba(0, 0, 0, 0.4)' });
        
      
    }

    plot(pcx, pcy);

    
    $('#update').on('click', function(e) {
        e.preventDefault();
        pcx = $('#x').val();
        pcy = $('#y').val();
        plot(pcx, pcy);
      });

  });






