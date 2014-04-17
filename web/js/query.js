// pgcharts query UI

var qdata;

//
// TABLE
//

function drawTable(data)
{
    var table = $("<table />");
    var thead = $("<thead />")
    var tbody = $("<tbody />");

    table.addClass("table table-stripped");
    table.append(thead);
    table.append(tbody);

    $("#qresult").empty().append(table);

    drawHeader(thead, data[0]);
    
    for (var i = 0; i < data.length; i++) {
        drawRow(tbody, data[i]);
    }
}

function drawHeader(thead, rowData)
{
    var row = $("<tr />")
    thead.append(row);

    for(var key in rowData)
    {
        row.append($("<th>" + key + "</td>"));
    }
}

function drawRow(tbody, rowData)
{
    var row = $("<tr />")
    tbody.append(row);

    for(var key in rowData)
    {
        row.append($("<td>" + rowData[key] + "</td>"));
    }
}

$("#raw").click(function(event) {
    $(this).tab('show');
    $("#qresult").empty();
    drawTable(qdata);
});

$("form").submit(function(event) {
    // alert( "Handler for .submit() called." );
    event.preventDefault();

    $("#raw").tab('show');

    h2 = $("<h2 />");
    h2.val("Fetching results...");
    $("#qresult").empty().append(h2);

    var posting = $.ajax({
        type: "POST",
        url: "/json",
        data: {"dburi": $("#dburi").val(),
               "query": $("#query").val()},
        dataType: "json"
    });

    posting.done(function (data) {
        qdata = data;
        drawTable(data);
    });
});

//
// PIE
//
function pie(JSONdata)
{
    var width = 960,
        height = 500,
        radius = Math.min(width, height) / 2;

    var color = d3.scale.ordinal()
        .range(["#98abc5", "#8a89a6", "#7b6888", "#6b486b", "#a05d56", "#d0743c", "#ff8c00"]);

    var arc = d3.svg.arc()
        .outerRadius(radius - 10)
        .innerRadius(0);

    var pie = d3.layout.pie()
        .sort(null)
        .value(function(d) { return d.population; });

    var svg = d3.select("#qresult").append("svg")
        .attr("width", width)
        .attr("height", height)
        .append("g")
        .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")");
    
    var data = [];

    for(i=0; i<JSONdata.length; i++)
    {
        data.push({
            label: JSONdata[i].RANGE,
            population: +JSONdata[i].FREQ
        });
    }
    console.log(data);

    var g = svg.selectAll(".arc")
        .data(pie(data))
        .enter().append("g")
        .attr("class", "arc");

    g.append("path")
        .attr("d", arc)
        .style("fill", function(d) { return color(d.data.label); });

    g.append("text")
        .attr("transform", function(d) { return "translate(" + arc.centroid(d) + ")"; })
        .attr("dy", ".35em")
        .style("text-anchor", "middle")
        .text(function(d) { return d.data.label; });
};

$("#pie").click(function(event) {
    $(this).tab('show');
    $("#qresult").empty();
    pie(qdata);
});

//
// DONUT
//
function donut(JSONdata)
{
    var width = 960,
        height = 500,
        radius = Math.min(width, height) / 2;

    var color = d3.scale.ordinal()
        .range(["#98abc5", "#8a89a6", "#7b6888", "#6b486b", "#a05d56", "#d0743c", "#ff8c00"]);

    var arc = d3.svg.arc()
        .outerRadius(radius - 10)
        .innerRadius(radius - 70);

    var pie = d3.layout.pie()
        .sort(null)
        .value(function(d) { return d.population; });

    var svg = d3.select("#qresult").append("svg")
        .attr("width", width)
        .attr("height", height)
        .append("g")
        .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")");

    var data = [];

    for(i=0; i<JSONdata.length; i++)
    {
        data.push({
            label: JSONdata[i].RANGE,
            population: +JSONdata[i].FREQ
        });
    }

    var g = svg.selectAll(".arc")
        .data(pie(data))
        .enter().append("g")
        .attr("class", "arc");

    g.append("path")
        .attr("d", arc)
        .style("fill", function(d) { return color(d.data.label); });

    g.append("text")
        .attr("transform", function(d) { return "translate(" + arc.centroid(d) + ")"; })
        .attr("dy", ".35em")
        .style("text-anchor", "middle")
        .text(function(d) { return d.data.label; });
};

$("#donut").click(function(event) {
    $(this).tab('show');
    $("#qresult").empty();
    donut(qdata);
});

//
// BAR
//
function bar(JSONdata)
{
    var margin = {top: 20, right: 20, bottom: 30, left: 80},
        width = 960 - margin.left - margin.right,
        height = 500 - margin.top - margin.bottom;

    var x = d3.scale.ordinal()
        .rangeRoundBands([0, width], .1);

    var y = d3.scale.linear()
        .range([height, 0]);

    var xAxis = d3.svg.axis()
        .scale(x)
        .orient("bottom");

    var yAxis = d3.svg.axis()
        .scale(y)
        .orient("left")
        .ticks(10);

    var svg = d3.select("#qresult").append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    var data = [];

    for(i=0; i<JSONdata.length; i++)
    {
        data.push({
            label: JSONdata[i].RANGE,
            population: +JSONdata[i].FREQ
        });
    }

    x.domain(data.map(function(d) { return d.label; }));
    y.domain([0, d3.max(data, function(d) { return d.population; })]);

    svg.append("g")
        .attr("class", "x axis")
        .attr("transform", "translate(0," + height + ")")
        .call(xAxis);

    svg.append("g")
        .attr("class", "y axis")
        .call(yAxis)
        .append("text")
        .attr("transform", "rotate(-90)")
        .attr("y", 6)
        .attr("dy", ".71em")
        .style("text-anchor", "end")
        .text("Population");

    svg.selectAll(".bar")
        .data(data)
        .enter().append("rect")
        .attr("class", "bar")
        .attr("x", function(d) { return x(d.label); })
        .attr("width", x.rangeBand())
        .attr("y", function(d) { return y(d.population); })
        .attr("height", function(d) { return height - y(d.population) });
};

$("#bar").click(function(event) {
    $(this).tab('show');
    $("#qresult").empty();
    bar(qdata);
});
