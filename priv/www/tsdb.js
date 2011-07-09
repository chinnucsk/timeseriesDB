if (typeof(tsdb) == "undefined") { tsdb = {}; }


var values = [];

tsdb.run = function(){
//    $("#canvas").append($('<p>').append("Inserted by JavaScript"));
    var d1 = [];
    for (var i = 0; i < 14; i += 0.5)
        d1.push([i, Math.sin(i)]);
    var d2 = [[0, 3], [4, 8], [8, 5], [9, 13]];
    // a null signifies separate line segments
    var d3 = [[0, 12], [7, 12], null, [7, 2.5], [12, 2.5]];


    $.getJSON('timeseries/nprocs.json',
	      function(data) {
		  $.each(data,
			 function(i, v){
			     var d = v['ts'];
			     var date = new Date(d[0],d[1],d[2],d[3],d[4],d[5],d[6],d[7]);
			     values.push([date, v['value']]);
			 });

		  var options = {yaxis: {min: 0},
				 xaxis: {mode: 'time'},
				 selection: { mode: "xy" }
				};
		  var canvas = $('#canvas');
		  var plot = $.plot(canvas,
				    [values],
				    options);
		  canvas.bind("plotselected",
			      function (event, ranges) {
				  var newOptions = $.extend(true, {}, options, {
								xaxis: { min: ranges.xaxis.from,
									 max: ranges.xaxis.to },
								yaxis: { min: ranges.yaxis.from,
									 max: ranges.yaxis.to }
							    });
				  $.plot(canvas,
					 [values],
					 newOptions);
			      });
	      });
};
