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

    table.addClass("table table-stripped table-hover table-condensed");
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
    var data = [];

    for(i=0; i<JSONdata.length; i++)
    {
        data.push([JSONdata[i].RANGE, JSONdata[i].FREQ]);
    }

    $('#qresult').highcharts({
        chart: {
            plotBackgroundColor: null,
            plotBorderWidth: 1,//null,
            plotShadow: false
        },
        title: {
            text: 'Query results'
        },
        tooltip: {
    	    pointFormat: '{series.name}: <b>{point.percentage:.1f}%</b>'
        },
        plotOptions: {
            pie: {
                allowPointSelect: true,
                cursor: 'pointer',
                dataLabels: {
                    enabled: true,
                    format: '<b>{point.name}</b>: {point.percentage:.1f} %',
                    style: {
                        color: (Highcharts.theme && Highcharts.theme.contrastTextColor) || 'black'
                    }
                },
                showInLegend: true
            }
        },
        series: [{
            type: 'pie',
            name: 'Query results',
            data: data
        }]
    });
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
    var data = [];

    for(i=0; i<JSONdata.length; i++)
    {
        data.push([JSONdata[i].RANGE, JSONdata[i].FREQ]);
    }

    $('#qresult').highcharts({
        chart: {
            plotBackgroundColor: null,
            plotBorderWidth: 0,
            plotShadow: false
        },
        title: {
            text: 'Query<br>result',
            align: 'center',
            verticalAlign: 'middle',
            y: 50
        },
        tooltip: {
            pointFormat: '{series.name}: <b>{point.percentage:.1f}%</b>'
        },
        plotOptions: {
            pie: {
                dataLabels: {
                    enabled: true,
                    distance: -50,
                    style: {
                        fontWeight: 'bold',
                        color: 'white',
                        textShadow: '0px 1px 2px black'
                    }
                },
                startAngle: -90,
                endAngle: 90,
                center: ['50%', '75%']
            }
        },
        series: [{
            type: 'pie',
            name: 'Range',
            innerSize: '50%',
            data: data
        }]
    });
}

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
    var cats = [];
    var data = [];

    for(i=0; i<JSONdata.length; i++)
    {
        cats.push(JSONdata[i].RANGE);
        data.push(JSONdata[i].FREQ);
    }
    
    $('#qresult').highcharts({
        chart: {
            type: 'column'
        },
        title: {
            text: 'Query Result'
        },
        xAxis: {
            categories: cats
        },
        yAxis: {
            min: 0,
            title: {
                text: 'Ranges'
            }
        },
        tooltip: {
            headerFormat: '<span style="font-size:10px">{point.key}</span><table>',
            pointFormat: '<tr><td style="color:{series.color};padding:0">{series.name}: </td>' +
                '<td style="padding:0"><b>{point.y:.1f} mm</b></td></tr>',
            footerFormat: '</table>',
            shared: true,
            useHTML: true
        },
        plotOptions: {
            column: {
                pointPadding: 0.2,
                borderWidth: 0
            }
        },
        series: [{
            name: 'Ranges',
            data: data
        }]
    });
};

$("#bar").click(function(event) {
    $(this).tab('show');
    $("#qresult").empty();
    bar(qdata);
});
