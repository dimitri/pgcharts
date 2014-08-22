// pgcharts query UI
var qdata;
var qdesc;
var qcats;
var qseries;
var xtitle;
var ytitle;
var type;

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

function display_result()
{
    $("#raw").tab('show');

    h2 = $("<h2 />");
    h2.val("Fetching results...");
    $("#qresult").empty().append(h2);

    var posting = $.ajax({
        type: "POST",
        url: "/json",
        data: {"dbname": $("#dbname").val(),
               "query":  myCodeMirror.getValue()},
        dataType: "json"
    });

    posting.done(function (data) {
        qdata = data;
        qdesc = $("#qdesc").val();
        qcats = $("#cats").val().toUpperCase();
        qseries = $("#series").val().toUpperCase();
        xtitle = $("#xtitle").val();
        ytitle = $("#ytitle").val();
        type = $("#chart-type option:selected").val().toLowerCase();

        if (type)
        {
            $("#"+type).tab("show");
            $("#qresult").empty();
            switch(type)
            {
                case "column":  col(data); break;
                case "bar":     bar(data); break;
                case "pie":     pie(data); break;
                case "donut": donut(data); break;
            }
        }
        else
        {
            drawTable(data);
        }
    });
}

$("#btn-run-query").click(function(event) {
    // alert( "Handler for .submit() called." );
    event.preventDefault();
    display_result();
});


$("#btn-save-raw-query").click(function(event) {
    // alert( "Handler for .submit() called." );
    event.preventDefault();

    var query = {
        "qid":    $("#qid").val(),
        "dbname": $("#dbname").val(),
        "qname":  $("#qname").val(),
        "qdesc":  $("#qdesc").val(),
        "query":  myCodeMirror.getValue("\n")
    };
    
    var myForm    = document.createElement("form");
    myForm.method = "post";
    myForm.action = "/q/save";
    
    for (var k in query)
    {
        var myInput = document.createElement("input") ;
        myInput.setAttribute("name", k) ;
        myInput.setAttribute("value", query[k]);
        myForm.appendChild(myInput) ;
    }

    document.body.appendChild(myForm);
    myForm.submit();
    document.body.removeChild(myForm);
});


//
// PIE
//
function pie(JSONdata)
{
    var data = [];

    for(i=0; i<JSONdata.length; i++)
    {
        data.push([JSONdata[i][qcats], JSONdata[i][qseries]]);
    }

    $('#qresult').highcharts({
        chart: {
            plotBackgroundColor: null,
            plotBorderWidth: 1,//null,
            plotShadow: false
        },
        title: {
            text: qdesc
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
            name: ytitle,
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
        data.push([JSONdata[i][qcats], JSONdata[i][qseries]]);
    }

    $('#qresult').highcharts({
        chart: {
            plotBackgroundColor: null,
            plotBorderWidth: 0,
            plotShadow: false
        },
        title: {
            text: qdesc,
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
            name: ytitle,
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
// COL
//
function col(JSONdata)
{
    var cats = [];
    var data = [];

    for(i=0; i<JSONdata.length; i++)
    {
        cats.push(JSONdata[i][qcats]);
        data.push(JSONdata[i][qseries]);
    }
    
    $('#qresult').highcharts({
        chart: {
            type: 'column'
        },
        title: {
            text: qdesc
        },
        xAxis: {
            categories: cats,
            title: {
                text: xtitle
            }
        },
        yAxis: {
            min: 0,
            title: {
                text: ytitle
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
            name: ytitle,
            data: data
        }]
    });
};

$("#column").click(function(event) {
    $(this).tab('show');
    $("#qresult").empty();
    col(qdata);
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
        cats.push(JSONdata[i][qcats]);
        data.push(JSONdata[i][qseries]);
    }

    $('#qresult').highcharts({
        chart: {
            type: 'bar'
        },
        title: {
            text: qdesc
        },
        xAxis: {
            categories: cats,
            title: {
                text: xtitle
            }
        },
        yAxis: {
            min: 0,
            title: {
                text: ytitle,
                align: 'high'
            },
            labels: {
                overflow: 'justify'
            }
        },
        tooltip: {
            valueSuffix: ' millions'
        },
        plotOptions: {
            bar: {
                dataLabels: {
                    enabled: true
                }
            }
        },
        legend: {
            layout: 'vertical',
            align: 'right',
            verticalAlign: 'top',
            x: -40,
            y: 100,
            floating: true,
            borderWidth: 1,
            backgroundColor: (Highcharts.theme && Highcharts.theme.legendBackgroundColor || '#FFFFFF'),
            shadow: true
        },
        credits: {
            enabled: false
        },
        series: [{
            name: ytitle,
            data: data
        }]
    });
};

$("#bar").click(function(event) {
    $(this).tab('show');
    $("#qresult").empty();
    bar(qdata);
});

if (doit)
{
    display_result();
}
